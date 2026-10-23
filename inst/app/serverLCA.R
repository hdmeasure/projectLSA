# ==== Reactive Data LCA ====
server_lca <- function(input, output, session, ai_context, console_context) {
  # text typed (or drafted by the assistant) in the Report form
  report_notes <- server_report_form(input, output, session, "lca", ai_context, method_ctx = function() { req(lca_models()); list(vars = input$vars_lca, k = input$best_class_lca) })

  library(poLCA)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(tibble)
  library(ggiraph)
  library(data.table)
  library(glca)
  library(stats)
  library(haven)

  best_c_r <- reactiveVal(NULL)
  set.seed(100)

  # Fitted models are held in a reactiveVal (instead of eventReactive) so that a
  # saved workspace can be restored straight into them.
  lca_models <- reactiveVal(NULL)

  # Variables selected in a restored workspace, applied once the picker exists
  lca_restored_vars <- reactiveVal(NULL)
observeEvent(input$run_lca, {
  req(data_lca(), input$vars_lca)
  updateTabsetPanel(session, "main_tab_lca", selected = "fit_tab_lca")
})
data_source <- reactive({
  if (input$data_source_lca == "cheat") {
    data("cheating", package = "poLCA")
    df <- cheating %>% rownames_to_column("id_auto") 
  } else if (input$data_source_lca == "lazar") {
    freq <- data.frame(
      V1   = c(1, 1, 1, 1, 2, 2, 2, 2),
      V2   = c(1, 1, 2, 2, 1, 1, 2, 2),
      V3   = c(1, 2, 1, 2, 1, 2, 1, 2),
      Freq = c(220, 160, 60, 160, 60, 60, 60, 220)
    )
    df <- freq[rep(seq_len(nrow(freq)), freq$Freq), c("V1", "V2", "V3")]
    rownames(df) <- NULL
    df$id_auto <- seq_len(nrow(df))
  
  } else if(input$data_source_lca == "gss82") {
    data("gss82", package = "poLCA")
    df <- gss82 %>% rownames_to_column("id_auto") %>%
      dplyr::mutate(
        PURPOSE  = dplyr::recode(PURPOSE,  "Waste of time"=1,"Depends"=2,"Good"=3),
        ACCURACY = dplyr::recode(ACCURACY, "Not true"=1,"Mostly true"=2),
        UNDERSTA = dplyr::recode(UNDERSTA, "Fair/Poor"=1,"Good"=2),
        COOPERAT = dplyr::recode(COOPERAT, "Impatient"=1,"Cooperative"=2,"Interested"=3)
      )
    
  } else {
    req(input$datafile_lca)
    showModal(modalDialog(title = NULL, "Reading Your File, Please wait...", footer = NULL, easyClose = FALSE))
    ext <- tolower(tools::file_ext(input$datafile_lca$name))

    if (ext == "rds") {
      res <- readRDS(input$datafile_lca$datapath)
      if (is.list(res) && !is.data.frame(res) && identical(res$type, "projectLSA_workspace")) {
        if (!identical(res$module, "LCA")) {
          removeModal()
          stop("Uploaded workspace belongs to a different module: ", res$module)
        }
        # Restore workspace state
        lca_models(res$lca_models)
        df <- res$raw_data

        st <- res$input_state
        if (!is.null(st)) {
          if (!is.null(st$min_class_lca))  updateNumericInput(session, "min_class_lca", value = st$min_class_lca)
          if (!is.null(st$max_class_lca))  updateNumericInput(session, "max_class_lca", value = st$max_class_lca)
          if (!is.null(st$best_class_lca)) updateNumericInput(session, "best_class_lca", value = st$best_class_lca)
          if (!is.null(st$use_cov_lca))    updateCheckboxInput(session, "use_cov_lca", value = st$use_cov_lca)
          if (!is.null(st$cov_lca))        updateSelectInput(session, "cov_lca", selected = st$cov_lca)
          if (!is.null(st$vars_lca))       lca_restored_vars(st$vars_lca)
        }
        showNotification("LCA Workspace restored successfully!", type = "message")
      } else {
        df <- res
      }
    } else {
      df <- switch(
        ext,
        "csv"  = data.table::fread(input$datafile_lca$datapath,data.table = FALSE),
        "xls"  = readxl::read_excel(input$datafile_lca$datapath),
        "xlsx" = readxl::read_excel(input$datafile_lca$datapath),
        "sav"  = haven::read_sav(input$datafile_lca$datapath),
        stop("Unsupported file type. Please upload CSV, Excel, SPSS (.sav), or RDS file.")
      )
    }
    removeModal()
    if (!"id_auto" %in% names(df)) {
      df <- df %>% mutate(across(everything(), ~ifelse(.x=="", NA, .x)),
                          id_auto = paste0("id_", sprintf("%04d", 1:n())))
    }
  }
  return(df)
})
data_lca <- reactive({
  df <- data_source()
  # Jika pakai covariat → buang baris NA di covariat
  if (isTRUE(input$use_cov_lca) && length(input$cov_lca) > 0) {
    df <- df %>% tidyr::drop_na(all_of(input$cov_lca))
  }
  
  df
})

# ==== Pilih ID ====
output$id_select_ui_lca <- renderUI({
  req(data_source())
  selectInput(
    "id_lca",
    label = "Select ID Columns (Optional):",
    choices = names(data_lca()),
    selected = names(data_source())[str_detect(names(data_source()), "id")],
    multiple = TRUE
    )
})

# ==== Pilih variabel ====
output$var_select_ui_lca <- renderUI({
  req(data_source())
  restored <- lca_restored_vars()
  sel <- if (!is.null(restored)) {
    intersect(restored, names(data_source()))
  } else {
    names(data_source()%>% dplyr::select(-c(id_auto)))[1:min(2,ncol(data_source()))]
  }
  selectInput(
    "vars_lca",
    label = "Select Variables for LCA:",
    choices = names(data_source()),
    selected = sel,
    multiple = TRUE  )
})

output$cov_lca_ui <- renderUI({
  req(data_source())
  
  tagList(
    checkboxInput(
      "use_cov_lca",
      label = "Include Covariate?",
      value = FALSE
    ),
    
    conditionalPanel(
      condition = "input.use_cov_lca == true",
      selectInput(
        "cov_lca",
        label = "Select Covariate (Numeric):",
        choices = names(data_source()),
        multiple = TRUE
      )
    )
  )
})


observeEvent(c(input$data_source_lca, input$datafile_lca), {
  # Keep the selection that came from a restored workspace
  if (is.null(lca_restored_vars())) {
    updateSelectInput(session, "vars_lca", selected = "")
  }
}, ignoreInit = TRUE)

# ==== Preview Data ====
output$data_preview_lca <- DT::renderDT({
  req(data_lca(), input$vars_lca)
  df <- data_lca() %>% dplyr::select(input$vars_lca)
  numeric_cols <- which(sapply(df, function(x) is.numeric(x)))
  
  DT::datatable(df,#extensions = 'Buttons',
                options = list(scrollX = TRUE, dom = 'rtp'
                               # buttons = list(
                               #   list(
                               #     extend = 'csv',
                               #     text = 'Export CSV',
                               #     filename = paste0('Data LCA')  
                               #   ),
                               #   list(
                               #     extend = 'excel',
                               #     text = 'Export Excel',
                               #     filename = paste0('Data LCA')
                               #   ))
                               ),
                rownames = TRUE)
}, server = TRUE)


output$data_summary_lca <- DT::renderDT({
  req(data_lca(), input$vars_lca)
  df <- data_lca() %>% dplyr::select(input$vars_lca)
  # Pastikan semuanya character (aman untuk join)
  df_chr <- df %>% dplyr::mutate(across(everything(), as.character))
  # Buat kolom join (kombinasi respons)
  df_chr$Respon <- apply(df_chr, 1, paste0, collapse = "-")
  
  # Tabel frekuensi
  freq_table <- df_chr %>%
    dplyr::count(Respon, name = "Freq") %>%
    dplyr::mutate(
      Percent = round(100 * Freq / sum(Freq), 2)
    ) %>%
    dplyr::arrange(dplyr::desc(Freq))
  
  DT::datatable(freq_table,extensions = 'Buttons',
                options = list(scrollX = TRUE, dom = 'Brtp',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('Data LCA')  
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('Data LCA')
                                 ))),
                rownames = TRUE)
}, server = FALSE)

output$data_description <- renderUI({
  desc_html <- if (input$data_source_lca == "cheat") {
    desc_cheating
  } else if (input$data_source_lca == "lazar") {
    desc_lazar
  } else if (input$data_source_lca == "gss82") {
    desc_gss82
  } else {
    ""
  }
  
  div(
    class = "badge-info",
    HTML(desc_html)
  )
})

# ==== Fit LCA ====
observeEvent(input$run_lca, {
  req(data_lca(), input$vars_lca)
  # A fresh run invalidates any restored workspace selection
  lca_restored_vars(NULL)
  df <- data_lca()
  showModal(modalDialog(title = NULL, "Please wait, (Running LCA)...", footer = NULL, easyClose = FALSE))
  
  # =========================
  # 1. Tentukan kolom yang dipakai
  # =========================
  vars_ind  <- input$vars_lca
  vars_cov  <- if (isTRUE(input$use_cov_lca)) input$cov_lca else NULL

  used_vars <- unique(c(vars_ind, vars_cov))

  dat <- df[, used_vars, drop = FALSE]

  # =========================
  # 2. Konversi indikator → factor
  # =========================
  dat <- dat %>%
    mutate(across(all_of(vars_ind), as.factor))

  # =========================
  # 3. Kovariat: JANGAN dipaksa factor
  #    (biarkan numeric tetap numeric)
  # =========================
  # OPTIONAL: jika mau auto-detect kovariat nominal
  if (!is.null(vars_cov)) {
    dat <- dat %>%
      mutate(across(
        all_of(vars_cov),
        ~ if (is.numeric(.x) && dplyr::n_distinct(.x) <= 3) {
          as.factor(.x)
        } else {
          .x
        }
      ))
  }

  # formula <- as.formula(paste("cbind(", paste(input$vars_lca, collapse=","), ") ~ 1"))
  # =========================
  # FORMULA (dengan / tanpa covariat)
  # =========================
  if (isTRUE(input$use_cov_lca) && length(input$cov_lca) > 0) {

    formula_polca <- as.formula(
      paste("cbind(", paste(input$vars_lca, collapse = ","), ") ~ ",
        paste(input$cov_lca)
      )
    )
    formula_glca <- as.formula(
      paste("item(", paste(input$vars_lca, collapse = ","), ") ~ ",
            paste(input$cov_lca)
      )
    )
  } else {
    formula_polca <- as.formula(
      paste("cbind(", paste(input$vars_lca, collapse = ","), ") ~ 1")
    )
    formula_glca <- as.formula(
      paste("item(", paste(input$vars_lca, collapse = ","), ") ~ 1")
    )
  }

  min_k <- input$min_class_lca
  max_k <- input$max_class_lca
  # model_list <- list()
  polca_models <- list()
  glca_models  <- list()
  withProgress(message = "Estimating LCA models...", {
    for (k in min_k:max_k) {
      
      incProgress(1 / (max_k - min_k + 1),
                  detail = paste("Classes:", k))
      
      polca_models[[as.character(k)]] <- tryCatch(
        poLCA::poLCA(formula = formula_polca, data= dat, nclass = k, verbose = FALSE),
        error = function(e) NULL
      )
      
      glca_models[[as.character(k)]] <- tryCatch(
        glca::glca(formula=formula_glca, data=dat, nclass = k, seed = 1, verbose = FALSE),
        error = function(e) NULL
      )
    }
  })
  # =========================
  # GOFGLCA (SEMUA MODEL SEKALIGUS)
  # =========================
  withProgress(message = "Comparing Multiple Model...", {
  glca_valid <- glca_models[!sapply(glca_models, is.null)]
  gof <- if (length(glca_valid) >= 2) {
    do.call(
      glca::gofglca,
      c(
        unname(glca_valid), 
        list(test = "boot", seed = 1)
      )
    )
  } else {
    NULL
  }
  if (is.null(gof)) {
    # Fewer than two comparable glca models (for example a range of only 1 to 2 classes):
    # show what poLCA reports for each class count and leave the bootstrap tests empty.
    ks <- names(polca_models)[!vapply(polca_models, is.null, logical(1))]
    gof_glca <- do.call(rbind, lapply(ks, function(k) {
      m <- polca_models[[k]]
      data.frame(loglik = m$llik, AIC = m$aic, BIC = m$bic, entropy = NA_real_, df = m$resid.df,
                 Gsq = m$Gsq, p_Gsq = NA_real_, Deviance = NA_real_, p_Deviance = NA_real_)
    }))
  } else {
    gtable <- as.data.frame(gof$gtable) %>% dplyr::rename(p_Gsq = `Boot p-value`)
    dtable <- as.data.frame(gof$dtable) %>% dplyr::rename(p_Deviance = `Boot p-value`) %>% dplyr::select(Deviance,p_Deviance)
    gof_glca <- cbind(gtable,dtable)
    if (input$min_class_lca == 1) {
      fit_1_class <- data.frame(
        loglik     = polca_models[["1"]]$llik,
        AIC        = polca_models[["1"]]$aic,
        BIC        = polca_models[["1"]]$bic,
        entropy    = NA,
        df         = polca_models[["1"]]$resid.df,
        Gsq        = polca_models[["1"]]$Gsq,
        p_Gsq      = NA,
        Deviance   = NA,
        p_Deviance = NA)
      gof_glca <- rbind(fit_1_class, gof_glca)
    }
  }
  })
  removeModal()
  lca_models(list(
    polca = polca_models,
    glca   = gof_glca
  ))
})


# FUNCTION TO CALCULATION OF LCA -----
APCP_poLCA <- function(fit) {
  post <- fit$posterior
  cls  <- fit$predclass
  K <- ncol(post)
  APCP_class <- sapply(1:K, function(k) {
    mean(post[cls == k, k], na.rm=TRUE) })
  APCP_overall <- mean(post[cbind(1:nrow(post), cls)])
  APCP_byclass <- mean(APCP_class,  na.rm = TRUE)
  min_Prob <- min(post[cbind(1:nrow(post), cls)])
  list(
    APCP_per_class = APCP_class,
    APCP_overall  = APCP_overall,
    APCP_byClass = APCP_byclass,
    min_Prob = min_Prob
  )
}
# Smallest Class Size ===
smallest_class <- reactive({ 
  req(lca_models())
  purrr::map_df(names(lca_models()$polca), function(k) {
    model_k <- lca_models()$polca[[k]]
    membership <- model_k$predclass
    class_table <- as.data.frame(table(membership))
    names(class_table) <- c("Class", "N")
    
    class_table$Percent <- round(100 * class_table$N / sum(class_table$N), 1)
    min_class <- class_table %>% slice_min(N, n = 1)
    min_class$N_class <- as.numeric(k)
    min_class
  })
})


# ==== Fit Table ====

# fit_lca <- reactive({
#   req(lca_models())
#   
#   fit <- data.frame(
#     N_class = integer(),
#     Smallest_class_size = numeric(),
#     Av_Prob = numeric()
#   )
#   
#   gof <- lca_models()$glca
#   model_polca <- lca_models()$polca
#   smallest_df <- smallest_class()
#   
#   for (k in names(model_polca)) {
#     n <- model_polca[[k]]
#     if (!is.null(n)) {
#       
#       scs <- smallest_df %>%
#         dplyr::filter(N_class == as.numeric(k)) %>%
#         dplyr::pull(Percent)
#       
#       fit <- rbind(
#         fit,
#         data.frame(
#           N_class = as.numeric(k),
#           Av_Prob = round(APCP_poLCA(n)$APCP_byClass, 2),
#           Smallest_class_size = scs
#         )
#       )
#     }
#   }
#   
#   cbind(fit, gof)
# })

fit_lca <- reactive({
  req(lca_models())
  
  fit <- data.frame(
    N_class = integer(),
    Smallest_class_size = numeric(),
    Av_Prob = numeric()
  )
  
  gof <- lca_models()$glca
  model_polca <- lca_models()$polca
  smallest_df <- smallest_class()
  
  for (k in names(model_polca)) {
    n <- model_polca[[k]]
    if (!is.null(n)) {
      
      scs <- smallest_df %>%
        dplyr::filter(N_class == as.numeric(k)) %>%
        dplyr::pull(Percent)
      
      fit <- rbind(
        fit,
        data.frame(
          N_class = as.numeric(k),
          Av_Prob = round(APCP_poLCA(n)$APCP_byClass, 2),
          Smallest_class_size = scs
        )
      )
    }
  }
  
  cbind(fit, gof)
})


# ==== Fit Table ====
output$fit_table_lca <- renderDT({
    df <- fit_lca() %>% 
      dplyr::select(N_class, loglik:p_Deviance, Smallest_class_size, Av_Prob) %>% 
      dplyr::mutate(
        dplyr::across(
          .cols = -c(N_class,df),
          .fns  = ~ round(.x, 2)
        )
      )
    aic_vals <- sort(unique(df$AIC))
    bic_vals <- sort(unique(df$BIC))
    ent_vals <- sort(unique(df$entropy), decreasing = TRUE)
    apcp_vals  <- sort(unique(df$Av_Prob), decreasing = TRUE)

    DT::datatable(df,extensions = 'Buttons',
                  options = list(scrollX = TRUE, dom = 'B',
                                 buttons = list(
                                   list(
                                     extend = 'csv',
                                     text = 'Export CSV',
                                     filename = paste0('Fit Comparison LCA')  
                                   ),
                                   list(
                                     extend = 'excel',
                                     text = 'Export Excel',
                                     filename = paste0('Fit Comparison LCA')
                                   ))), 
                  rownames = FALSE) %>% 
      formatStyle('AIC',
                  backgroundColor = styleEqual(
                    c(aic_vals[1], aic_vals[2]),
                    c('#d9f1ee', '#fbefd2')
                  ),
                  fontWeight = styleEqual(aic_vals[1], 'bold')
      ) %>%
      formatStyle('BIC',
                  backgroundColor = styleEqual(
                    c(bic_vals[1], bic_vals[2]),
                    c('#d9f1ee', '#fbefd2')
                  ),
                  fontWeight = styleEqual(bic_vals[1], 'bold')
      ) %>%
      formatStyle('entropy',
                  backgroundColor = styleInterval(c(0.7, 0.8), c('#fbe0e3', '#fbefd2', '#d9f1ee')),
                  fontWeight      = styleInterval(c(0.7, 0.8), c('bold', 'bold',''))
      ) %>%
      formatStyle('Av_Prob',
                    backgroundColor = styleInterval(c(0.8, 0.9), c('#fbe0e3', '#fbefd2', '#d9f1ee')),
                    fontWeight      = styleInterval(c(0.8, 0.9), c('bold', 'bold',''))
      ) %>% 
      formatStyle(
        'p_Gsq',
        backgroundColor = styleInterval(0.05, c('#fbe0e3', '#d9f1ee')),
        fontWeight      = styleInterval(0.05, c('bold', ''))
      ) %>%
      formatStyle(
        'p_Deviance',
        backgroundColor = styleInterval(0.05, c('#d9f1ee', '#fbe0e3')),
        fontWeight      = styleInterval(0.05, c('', 'bold'))
      ) %>% 
      formatStyle(
        'Smallest_class_size',
        backgroundColor = styleInterval(c(5, 7), c('#fbe0e3', '#fbefd2', '#d9f1ee')),
        fontWeight      = styleInterval(c(5, 7), c('bold', 'bold',''))
      )
}, server = FALSE)

# ==== Fit Plot (AIC/BIC) ====
fit_plot_lca_reactive <- reactive({
  req(lca_models())
  fit <- fit_lca() %>% dplyr::select(N_class, BIC, AIC)
  fit_long <- fit %>% pivot_longer(-N_class, names_to="Index", values_to="Value")
  pl_compare_plot(fit_long, "N_class", "Value", group = "Index", title = "BIC and AIC", xlab = "Number of classes", ylab = "Fit index", palette = "purple")
})
# Render Plot
output$fit_plot_lca <- renderPlot({
  fit_plot_lca_reactive()
})

# ==== Download Buttons ====
output$download_plot_AicBic_LCA <- make_download_plot(
  plot_reactive = fit_plot_lca_reactive,
  filename_prefix = "Fit BIC & AIC Plot_LCA"
)

# ==== Entropy Plot  ====
entropy_plot_lca_reactive <- reactive({
  req(lca_models())
  
  fit <- fit_lca() %>%
    dplyr::select(N_class, entropy, Av_Prob) %>%
    tidyr::pivot_longer(-N_class, names_to = "Index", values_to = "Value")
  
  pl_compare_plot(fit, "N_class", "Value", group = "Index", title = "Entropy and average posterior probability", xlab = "Number of classes", ylab = "Value", palette = "amber")
})

# Render Plot
output$entropy_plot_lca <- renderPlot({
  entropy_plot_lca_reactive()
})

# ==== Download Buttons ====
output$download_plot_entropy <- make_download_plot(
  plot_reactive = entropy_plot_lca_reactive,
  filename_prefix = "Entropy & Av. Posterior Probability Plot_LCA"
)

observeEvent(input$best_class_lca, {
  req(input$best_class_lca)
  best_c_r(as.numeric(input$best_class_lca))
}, ignoreInit = TRUE)

# ==== Input nama class ====
output$class_name_inputs_lca <- renderUI({
  req(input$best_class_lca)
  lapply(1:input$best_class_lca, function(i) {
    textInput(paste0("class_name_", i), paste("Class", i, "Name:"), value = paste("Class", i))
  })
})

# === 1. Plot of the selected model ===
# type: "auto" (profile lines when every item has two categories, stacked bars otherwise),
#       "profile" (probability of the second category, one line per class) or "stack"
build_best_model_plot_lca <- function(lca_models, k, vars_lca, class_names, interactive = TRUE,
                                      type = "auto", palette = "brand", custom = NULL, show_values = FALSE, mono = FALSE) {
  model_k <- lca_models[[as.character(k)]]
  probs <- model_k$probs
  k <- as.numeric(k)
  all_binary <- all(vapply(probs, ncol, integer(1)) == 2L)
  if (identical(type, "auto") || is.null(type)) type <- if (all_binary) "profile" else "stack"
  if (identical(type, "profile") && !all_binary) type <- "stack"      # lines need two categories per item

  prob <- data.frame(matrix("", ncol = 2 + k, nrow = 0))
  names(prob) <- c("Var", "Level", paste0("Prob.class", 1:k))
  for (j in seq_along(vars_lca)) {
    a <- data.frame(Var = vars_lca[j], Level = colnames(probs[[j]]), t(probs[[j]]))
    a <- setNames(a, names(prob))
    prob <- rbind(prob, a)
  }
  prob_gathered <- prob %>%
    tidyr::gather(key = "class", value = "probability", 3:ncol(prob)) %>%
    dplyr::mutate(class = gsub("Prob.", "", class), probability = round(as.numeric(probability), 3), Var = factor(Var, levels = vars_lca))
  prob_gathered$class <- factor(prob_gathered$class, levels = paste0("class", 1:k), labels = class_names)

  if (identical(type, "profile")) {
    # the probability of the second category (e.g. "yes", "agree") of every item, one line per class
    d <- prob_gathered
    keep <- vapply(seq_along(vars_lca), function(j) colnames(probs[[j]])[2], character(1))
    d <- d[d$Level == keep[match(as.character(d$Var), vars_lca)], , drop = FALSE]
    cols <- pl_lca_palette(palette, k, custom)
    d$tip <- paste0("Class: ", d$class, "<br>Item: ", d$Var, "<br>Category: ", d$Level, "<br>Probability: ", formatC(d$probability, format = "f", digits = 3))
    # lines differ by colour, line type and marker, so the figure still reads in black and white
    ltys <- rep_len(c("solid", "22", "42", "13", "solid", "22"), k); shps <- rep_len(c(21, 22, 24, 23, 25, 21), k)
    if (isTRUE(mono)) cols <- rep("black", k)
    gg <- ggplot(d, aes(x = Var, y = probability, colour = class, group = class, linetype = class)) +
      geom_line(linewidth = 0.8, alpha = 0.9)
    gg <- if (interactive) gg + geom_point_interactive(aes(tooltip = tip, data_id = class, shape = class), size = 2.2, fill = "white", stroke = 0.9)
          else gg + geom_point(aes(shape = class), size = 2.2, fill = "white", stroke = 0.9)
    if (isTRUE(show_values)) gg <- gg + geom_text(aes(label = formatC(probability, format = "f", digits = 2)), vjust = -1, size = 2.6, show.legend = FALSE)
    return(gg +
      scale_colour_manual(values = cols, name = NULL) +
      scale_linetype_manual(values = ltys, name = NULL) +
      scale_shape_manual(values = shps, name = NULL) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = expansion(mult = c(0.02, 0.08))) +
      labs(x = NULL, y = "Probability of the higher category") +
      theme_pl(11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8.5),
            panel.grid.major.x = element_line(color = "#eef0f6", linewidth = 0.3), plot.margin = margin(6, 10, 4, 8)))
  }

  # stacked bars: one panel per class, categories as fill
  lv <- unique(prob_gathered$Level)
  cols <- if (isTRUE(mono)) pl_grey_set(length(lv)) else pl_lca_palette(palette, length(lv), custom)
  if (interactive) {
    gg <- ggplot(prob_gathered, aes(x = Var, y = probability, fill = Level,
                 tooltip = paste0("Class: ", class, "<br>Variable: ", Var, "<br>Level: ", Level, "<br>Probability: ", probability),
                 data_id = class)) +
      geom_bar_interactive(stat = "identity", position = "stack", width = 0.8)
  } else {
    gg <- ggplot(prob_gathered, aes(x = Var, y = probability, fill = Level)) + geom_col(position = "stack", width = 0.8, colour = "white", linewidth = 0.4)
  }
  gg +
    facet_grid(~ class) +
    scale_fill_manual(values = setNames(cols, lv), name = NULL) +
    labs(x = NULL, y = "Probability") +
    theme_pl(11) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), panel.grid.major.x = element_blank(),
          strip.text = element_text(face = "bold", size = 10.5, color = "#1e1b4b"), plot.margin = margin(6, 10, 4, 8))
}

# the plot options chosen in the sidebar
lca_plot_args <- function() list(type = input$lca_plot_type %||% "auto", palette = input$lca_palette %||% "brand", mono = identical(input$fig_mode, "mono"),
                                 custom = {k <- as.numeric(input$best_class_lca %||% 2); v <- vapply(seq_len(max(k, 8)), function(i) input[[paste0("lca_col_", i)]] %||% NA_character_, ""); v[!is.na(v)]},
                                 show_values = isTRUE(input$lca_show_values))

output$lca_custom_cols <- renderUI({
  req(input$lca_palette == "custom", input$best_class_lca)
  k <- as.numeric(input$best_class_lca)
  base <- pl_palettes$brand
  pl_grid(lapply(seq_len(k), function(i) colourpicker::colourInput(paste0("lca_col_", i), paste("Class", i), value = base[(i - 1) %% length(base) + 1], showColour = "background", palette = "square")), cols = 2)
})

# === 2. Reactive for the interactive plot ===
best_model_plot_lca_reactive <- reactive({
  req(lca_models(), input$best_class_lca, input$vars_lca)
  k <- input$best_class_lca
  class_names <- sapply(1:k, function(i) input[[paste0("class_name_", i)]])
  if (length(class_names) == 0) class_names <- paste("Class", 1:k)
  a <- lca_plot_args()
  build_best_model_plot_lca(lca_models()$polca, k, input$vars_lca, class_names, interactive = TRUE,
                            type = a$type, palette = a$palette, custom = a$custom, show_values = a$show_values, mono = a$mono)
})

# === 3. Render plot interaktif ===
output$best_model_plot_lca <- renderGirafe({
  k <- input$best_class_lca
  filename_plot <- paste0("LCA_best_model_", k, "class")
  
  ggiraph::girafe(
    ggobj = best_model_plot_lca_reactive(),
    width_svg = 8, height_svg = 3.8,
    options = list(
      opts_hover(css = "fill-opacity:1;"),
      opts_tooltip(css = "background-color:white;color:black;
                    border:1px solid #ccc;padding:6px;
                    font-size:11px;border-radius:5px;"),
      opts_toolbar(saveaspng = TRUE, pngname = filename_plot)
    )
  )
})

# === 4. Static version (reports, copying) ===
best_model_plot_lca_static <- reactive({
  req(lca_models(), input$best_class_lca, input$vars_lca)
  k <- input$best_class_lca
  class_names <- sapply(1:k, function(i) input[[paste0("class_name_", i)]])
  if (length(class_names) == 0) class_names <- paste("Class", 1:k)
  a <- lca_plot_args()
  g <- build_best_model_plot_lca(lca_models()$polca, k, input$vars_lca, class_names, interactive = FALSE,
                                 type = a$type, palette = a$palette, custom = a$custom, show_values = a$show_values, mono = FALSE)
  if (isTRUE(a$mono)) pl_mono(g) else g          # hatching for bars, black lines for the rest
})


# === 3. Download handler panggil plot statis ===
output$download_plot_best_LCA <- make_download_plot(
  plot_reactive = best_model_plot_lca_static,
  filename_prefix = paste0("LCA_best_model_", input$best_class_lca, "class")
)

# ==== Summary table ====
summary_data_lca <- reactive({
  req(lca_models(), input$best_class_lca, input$vars_lca)
  
  k <- as.numeric(input$best_class_lca)
  model_k <- lca_models()$polca[[as.character(k)]]
  req(model_k)
  
  # Ambil nama class dari input (fallback jika belum diisi)
  class_names <- sapply(1:k, function(i) {
    nm <- input[[paste0("class_name_", i)]]
    if (is.null(nm) || nm == "") paste0("Class ", i) else nm
  })
  
  # === 1. Ukuran kelas ===
  class_size <- data.frame(
    base::table(model_k$predclass)) %>%
    dplyr::rename(Class = Var1, N = Freq) %>%
    dplyr::mutate(Percent=round(100*N/sum(N),2))
  class_size$Class <- class_names[class_size$Class]
  
  # === 2. Probabilitas kategori (adaptasi dari plot Girafe) ===
  probs <- model_k$probs
  listvar <- input$vars_lca
  
  prob <- data.frame(matrix("", ncol = 2 + k, nrow = 0))
  names(prob) <- c("Variable", "Category", paste0("Prob.", class_names))
  
  for (j in seq_along(listvar)) {
    a <- data.frame(
      Variable = listvar[j],
      Category = colnames(probs[[j]]),
      t(probs[[j]])
    )
    names(a) <- names(prob)
    prob <- rbind(prob, a)
  }
  
  prob <- prob %>%
    mutate(across(starts_with("Prob."), as.numeric)) %>%
    arrange(Variable)
  
  # Gabungkan class size + probabilitas (tidak merge, tapi list agar bisa dua tabel)
  list(
    class_size = class_size,
    probability = prob
  )
})


# ==== Render table ====
output$summary_table_lca <- renderUI({
  req(summary_data_lca())
  dat <- summary_data_lca()
  
  tagList(
    tags$h5("Class Size"),
    DTOutput("class_size_table_lca"),
    tags$h5("Item-Categories Probabilities"),
    DTOutput("probability_table_lca")
  )
})

output$class_size_table_lca <- renderDT({
  df <- summary_data_lca()$class_size
  DT::datatable(df,extensions = 'Buttons',
                options = list(scrollX = TRUE, dom = 'B',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('Class Size')
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('Class Size')
                                 ))),
                rownames = FALSE)
})

output$probability_table_lca <- renderDT({
  df <- summary_data_lca()$probability
  numeric_cols <- which(sapply(df, function(x) is.numeric(x)))
  DT::datatable(df,extensions = 'Buttons',
                options = list(scrollX = TRUE, pageLength=25,
                               dom = 'Brtp',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('Class Probability')
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('Class Probability')
                                 ))),
                rownames = FALSE) %>%
    formatRound(columns = numeric_cols, digits = 2)
})

# df_out <- reactive({
#   req(lca_models(), input$best_class_lca)
#   k <- as.numeric(input$best_class_lca)
#   # Ambil nama class dari input (fallback jika belum diisi)
#   class_names <- sapply(1:k, function(i) {
#     nm <- input[[paste0("class_name_", i)]]
#     if (is.null(nm) || nm == "") paste0("Class ", i) else nm
#   })
#   model_k <- lca_models()$polca[[as.character(input$best_class_lca)]]
#   req(model_k)
#   
#   df <- data_lca()
#   df$Class <- model_k$predclass
#   df$Class <- class_names[df$Class]
#   df$Respon <- df %>%
#     tidyr::unite("Respon", all_of(input$vars_lca), sep = "-", remove = FALSE) %>%
#     dplyr::pull(Respon)
#   
#   # --- Posterior probabilities ---
#   posterior_df <- as.data.frame(round(model_k$posterior,2))
#   colnames(posterior_df) <- paste0("Posterior_Prob_", class_names)
#   numeric_cols <- which(sapply(posterior_df, is.numeric))
#   
#   # Gabungkan: ID + vars + posterior + Class
#   df <- cbind(
#     df,
#     posterior_df
#   )
#   df
#   
# })
df_out <- reactive({
  req(lca_models(), input$best_class_lca)
  
  k <- as.numeric(input$best_class_lca)
  
  # --- Class names ---
  class_names <- sapply(1:k, function(i) {
    nm <- input[[paste0("class_name_", i)]]
    if (is.null(nm) || nm == "") paste0("Class ", i) else nm
  })
  
  model_k <- lca_models()$polca[[as.character(input$best_class_lca)]]
  req(model_k)
  
  df <- data_lca()
  n <- nrow(df)
  
  # --- Tentukan baris yang benar-benar dipakai LCA ---
  vars_used <- input$vars_lca
  valid_idx <- complete.cases(df[, vars_used])
  
  # --- Inisialisasi kolom hasil (panjang = data asli) ---
  df$Class <- NA_character_
  
  # Isi hanya untuk baris valid
  df$Class[valid_idx] <- class_names[model_k$predclass]
  
  # --- Respon pattern (aman walau ada NA) ---
  df$Respon <- df |>
    tidyr::unite("Respon", all_of(vars_used), sep = "-", remove = FALSE) |>
    dplyr::pull(Respon)
  
  # --- Posterior probabilities ---
  posterior_mat <- model_k$posterior
  posterior_df <- matrix(NA, nrow = n, ncol = k)
  posterior_df[valid_idx, ] <- round(posterior_mat, 2)
  
  posterior_df <- as.data.frame(posterior_df)
  colnames(posterior_df) <- paste0("Posterior_Prob_", class_names)
  
  # --- Gabungkan ke data asli ---
  df <- cbind(df, posterior_df)
  
  df
})

# ---- follow-up analyses (classification quality, comparisons, cross-tabs, predictors, export) ----
lca_assign <- reactive({
  req(lca_models(), input$best_class_lca)
  df <- df_out(); k <- as.numeric(input$best_class_lca)
  model_k <- lca_models()$polca[[as.character(k)]]; req(model_k)
  idx <- complete.cases(data_lca()[, input$vars_lca, drop = FALSE])
  P <- matrix(NA_real_, nrow(df), k); P[idx, ] <- model_k$posterior
  list(data = df, cls = "Class", post = P, skip = c("Respon"))
})
server_followup(input, output, session, "lca", lca_assign, "class")

# Data Summary Respon_With Class
output$data_summary_lca_with_class <- DT::renderDT({
  req(df_out(), input$best_class_lca)
  df <- df_out()
  # Tabel frekuensi
  freq_table <- df %>%
    dplyr::count(Respon,Class, name = "Freq") %>%
    dplyr::mutate(
      Percent = round(100 * Freq / sum(Freq), 2)
    ) %>%
    dplyr::arrange(dplyr::desc(Freq))
  
  DT::datatable(freq_table,extensions = 'Buttons',
                options = list(scrollX = TRUE, pageLength=25,
                               dom = 'Brtp',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('Data LCA')  
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('Data LCA')
                                 ))),
                rownames = TRUE)
}, server = FALSE)


# ==== Summary Table & Class per ID ====
output$profile_table_lca <- renderDT({
  req(lca_models(), input$best_class_lca)
  df <- df_out()
  DT::datatable(df,extensions = 'Buttons',
                options = list(scrollX = TRUE, pageLength=10,
                               dom = 'Brtp',
                               buttons = list(
                                 list(
                                   extend = 'csv',
                                   text = 'Export CSV',
                                   filename = paste0('LCA Result with ', input$best_class_lca, ' Classes')
                                 ),
                                 list(
                                   extend = 'excel',
                                   text = 'Export Excel',
                                   filename = paste0('LCA Result with', input$best_class_lca, ' Classes')
                                 ))),
                rownames = TRUE)
  
}, server = FALSE)

output$var_x_ui <- renderUI({
  req(df_out())
  selectInput(
    "var_x",
    label = "X Absis",
    choices = names(df_out()),
    selected = "Class",
    multiple = FALSE 
  )
})
output$var_y_ui <- renderUI({
  req(df_out())
  selectInput(
    "var_y",
    label = "Y Absis",
    choices = setdiff(names(df_out()), "Class"),
    selected = "",
    multiple = TRUE
  )
})
output$cross_latent <- renderPlot({
  req(lca_models(), input$best_class_lca, input$var_x, input$var_y)
  df <- df_out()
  # =========================
  # 1. RINGKAS DATA
  # =========================
  Summary_long <- df %>%
    dplyr::select(all_of(c(input$var_x, input$var_y))) %>%
    tidyr::pivot_longer(
      cols = all_of(input$var_y),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    dplyr::group_by(
      Class  = .data[[input$var_x]],
      Variable
    ) %>%
    dplyr::summarise(
      Mean = mean(Value, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    dplyr::mutate(
      Variable = factor(Variable, levels = input$var_y)
    )
  
  # =========================
  # 2. BASE PLOT
  # =========================
  p <- ggplot(
    Summary_long,
    aes(
      x = Variable,
      y = Mean,
      fill = Class,
      color = Class,
      group = Class
    )
  )
  # =========================
  # 3. SWITCH BAR / LINE
  # =========================
  if (input$plot_type == "bar") {
    
    p <- p +
      geom_col(
        position = position_dodge(width = 0.7),
        width = 0.6
      )
  } else {
    p <- p +
      geom_line(
        aes(linetype = Class),
        size = 1.2
      ) +
      geom_point(
        aes(shape = Class),
        size = 3
      )
  }
  
  # =========================
  # 4. FINAL STYLE
  # =========================
  p +
    geom_text(
      aes(label = round(Mean, 2)),
      position = position_dodge(width = 0.7),
      vjust = -0.7,
      size = 4.5,
      show.legend = FALSE
    ) +
    labs(
      title = paste0("Class Plot for ", input$best_class_lca, " Classes"),
      x = "Variable",
      y = "Mean Value"
    ) +
    ylim(0, max(Summary_long$Mean, na.rm = TRUE) * 1.18) +
    
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
      legend.position = "bottom",
      axis.line = element_line(color = "black")
    )
  })

output$reg_lca_summary <- renderTable ({
  req(lca_models(), input$best_class_lca, input$use_cov_lca)
  
  model_k <- lca_models()$polca[[as.character(input$best_class_lca)]]
  req(model_k$coeff)
  
  coef_mat <- model_k$coeff
  coef_df <- as.data.frame(coef_mat)
  coef_df$Covariate <- rownames(coef_df)
  
  coef_df <- coef_df %>%
    relocate(Covariate) %>%
    mutate(across(where(is.numeric), round, 3))
  coef_df
})
output$reg_lca_plot <- renderPlot({
  req(
    lca_models(),
    input$best_class_lca,
    input$use_cov_lca,
    input$cov_lca
  )
  
  # === HANYA UNTUK 2 KELAS ===
  if (as.numeric(input$best_class_lca) != 2) {
    return(NULL)
  }
  
  model_k <- lca_models()$polca[[as.character(input$best_class_lca)]]
  beta <- model_k$coeff
  coef_names <- rownames(beta)
  
  # === Pilih kovariat ===
  cov_name <- input$cov_lca[1]
  cov_vals <- sort(unique(data_lca()[[cov_name]]))
  
  # === Design matrix X ===
  X <- data.frame(matrix(0, nrow = length(cov_vals), ncol = length(coef_names)))
  colnames(X) <- coef_names
  
  if ("(Intercept)" %in% coef_names) {
    X[["(Intercept)"]] <- 1
  }
  if (cov_name %in% coef_names) {
    X[[cov_name]] <- cov_vals
  }
  
  # === Probabilities ===
  eta <- as.matrix(X) %*% beta
  p_class2 <- as.vector(exp(eta) / (1 + exp(eta)))
  p_class1 <- 1 - p_class2
  
  prob_df <- data.frame(
    Covariate = cov_vals,
    `Class 1` = p_class1,
    `Class 2` = p_class2
  )
  
  prob_long <- tidyr::pivot_longer(
    prob_df,
    cols = c("Class 1", "Class 2"),
    names_to = "Class",
    values_to = "Probability"
  )
  
  # === Plot ===
  ggplot(prob_long, aes(Covariate, Probability, color = Class)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      x = cov_name,
      y = "Predicted Class Membership Probability",
      title = "Effect of Covariate on Latent Class Membership (2-Class Model)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      axis.line = element_line(color = "black")
    )
})
output$anova_lca_ui <- renderUI({
  req(lca_models(), input$best_class_lca, input$var_x, input$var_y)
  
  if (isTRUE(input$check_anova)) {
    tagList(
      tags$h5(
        paste0(
          "Latent Class Differences on ",
          paste(input$var_y, collapse = ", ")
        )
      ),
      tags$h6("Analysis of Variance"),
      tableOutput("anova_lca_table"),
      uiOutput("posthoc_lca_ui")
    )
  }
})
output$anova_lca_table <- renderTable({
  req(df_out(), input$var_x, input$var_y)
  
  df <- df_out()
  x  <- input$var_x
  ys <- input$var_y
  
  # =========================
  # CASE 1: ONE-WAY ANOVA
  # =========================
  if (length(ys) == 1) {
    
    fml <- as.formula(paste(ys, "~", x))
    fit <- aov(fml, data = df)
    tbl <- summary(fit)[[1]]
    
    pval <- tbl["Pr(>F)"][1,1]
    
    out <- data.frame(
      Source = rownames(tbl),
      round(tbl, 4),
      row.names = NULL
    )
    
    out$Interpretation <- c(
      ifelse(
        pval < 0.05,
        "Significant difference",
        "No significant difference"
      ),
      rep("", nrow(out) - 1)
    )
    
    out
    
  } else {
    
    # =========================
    # CASE 2: MANOVA
    # =========================
    
    fml <- as.formula(
      paste(
        "cbind(",
        paste(ys, collapse = ","),
        ") ~ ",
        x
      )
    )
    
    fit <- manova(fml, data = df)
    tbl <- summary(fit, test = "Wilks")$stats
    pval <- tbl[1, "Pr(>F)"]
    
    out <- data.frame(
      Effect = rownames(tbl),
      round(tbl, 4),
      row.names = NULL
    )
    
    out$Interpretation <- ifelse(
      out$Effect == x,
      ifelse(
        pval < 0.05,
        "Significant difference",
        "No significant difference"
      ),
      ""
    )
    
    out
  }
})
output$posthoc_lca_ui <- renderUI({
  req(df_out(), input$var_x, input$var_y)
  
  if (length(input$var_y) == 1) {
    tagList(
      tags$h6("Post-hoc Test (Tukey HSD)"),
      tableOutput("posthoc_lca_table")
    )
  }
})
output$posthoc_lca_table <- renderTable({
  req(df_out(), input$var_x, input$var_y)
  
  # Post-hoc hanya untuk ANOVA
  if (length(input$var_y) != 1) return(NULL)
  
  df <- df_out()
  x  <- input$var_x
  y  <- input$var_y
  
  fml <- as.formula(paste(y, "~", x))
  fit <- aov(fml, data = df)
  
  # p-value ANOVA utama
  p_main <- summary(fit)[[1]]["Pr(>F)"][1, 1]
  
  # Jika tidak signifikan → tampilkan pesan
  if (is.na(p_main) || p_main >= 0.05) {
    return(
      data.frame(
        Note = "Post-hoc test was not conducted because the ANOVA result was not statistically significant."
      )
    )
  }
  
  tuk <- TukeyHSD(fit)[[1]]
  tuk <- round(as.data.frame(tuk), 4)
  
  data.frame(
    Comparison = rownames(tuk),
    tuk,
    Interpretation = ifelse(
      tuk$`p adj` < 0.05,
      "Significant difference",
      "Not significant"
    ),
    row.names = NULL
  )
})
output$reg_lca_ui <- renderUI({
  req(input$use_cov_lca)
  
  if (isTRUE(input$use_cov_lca) && length(input$cov_lca) > 0) {
    tagList(
      tags$h5("Covariate Effects on Class Membership"),
      tableOutput("reg_lca_summary"),
      br(),
      plotOutput("reg_lca_plot", height = "400px")
    )
  }
})

# CrossTab====
output$crosstab_ui <- renderUI({
  req(input$crosstab_result)
  if (!input$crosstab_result) return(NULL)
  
  req(df_out())
  vars <- setdiff(names(df_out()), "Class")
  fluidRow(
    column(
      12,
      h5("Cross Tabulation Settings"),
      selectInput(
        "crosstab_vars",
        "Select Variables:",
        choices = vars,
        multiple = TRUE
      )
    ),
    column(
      6,
      h5("Cross Tabulation For Categorical Variables"),
      tableOutput("crosstab_cat"),
      plotOutput("crosstab_cat_plot")
    ),
    column(
      6,
      h5("Mean & SD for Continuous Variables"),
      tableOutput("crosstab_cont"),
      plotOutput("crosstab_cont_plot")
    ),
    br(), br(), br()
  )
})
output$crosstab_cat <- renderTable({
  req(input$crosstab_result, input$crosstab_vars)
  if (!input$crosstab_result) return(NULL)
  
  df <- df_out()
  
  vars_cat <- input$crosstab_vars[
    sapply(df[input$crosstab_vars], function(x)
      is.factor(x) || is.character(x))
  ]
  
  if (length(vars_cat) == 0) return(NULL)
  
  do.call(rbind, lapply(vars_cat, function(v) {
    tab <- as.data.frame(
      table(Class = df$Class, Category = df[[v]])
    )
    
    tab <- tab |>
      dplyr::group_by(Category) |>
      dplyr::mutate(
        Percent = round(Freq / sum(Freq) * 100, 2)
      ) |>
      dplyr::ungroup()
    
    data.frame(
      Class  = tab$Class,
      Variable = v,
      Category = tab$Category,
      N        = tab$Freq,
      Percent  = tab$Percent
    )
  }))
})
output$crosstab_cont <- renderTable({
  req(df_out(), input$crosstab_vars)
  
  df <- df_out()
  
  vars_num <- input$crosstab_vars[
    sapply(df[input$crosstab_vars], is.numeric)
  ]
  
  if (length(vars_num) == 0) return(NULL)
  
  res <- lapply(vars_num, function(v) {
    aggregate(
      df[[v]],
      by = list(Class = df$Class),
      FUN = function(x)
        paste0(round(mean(x, na.rm = TRUE), 2),
               " (", round(sd(x, na.rm = TRUE), 2), ")")
    ) |>
      setNames(c("Class", v))
  })
  
  Reduce(function(x, y) merge(x, y, by = "Class"), res)
})
output$crosstab_cat_plot <- renderPlot({
  req(input$crosstab_result, input$crosstab_vars)
  if (!input$crosstab_result) return(NULL)
  
  df <- df_out()
  
  vars_cat <- input$crosstab_vars[
    sapply(df[input$crosstab_vars], function(x)
      is.factor(x) || is.character(x))
  ]
  
  if (length(vars_cat) == 0) return(NULL)
  
  plot_data <- do.call(rbind, lapply(vars_cat, function(v) {
    tab <- as.data.frame(table(Class = df$Class, Category = df[[v]]))
    
    tab <- tab |>
      dplyr::group_by(Category) |>
      dplyr::mutate(
        Percent = round(Freq / sum(Freq) * 100, 2)
      ) |>
      dplyr::ungroup()
    
    tab$Variable <- v
    tab
  }))
  
  ggplot(plot_data,
         aes(x = Category, y = Percent, fill = Class)) +
    geom_col(
      width = 0.6,
      position = position_dodge(width = 0.7)
    ) +
    geom_text(
      aes(label = paste0(Percent, "%")),
      position = position_dodge(width = 0.7),
      vjust = -0.25,
      size = 3
    ) +
    facet_wrap(~ Variable, scales = "free_x") +
    labs(
      title = "Categorical Cross-Tabulation by Class",
      y = "Percentage (%)",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
      legend.position = "bottom",
      axis.line = element_line(color = "black")
    )
})
output$crosstab_cont_plot <- renderPlot({
  req(input$crosstab_result, input$crosstab_vars)
  if (!input$crosstab_result) return(NULL)
  
  df <- df_out()
  
  vars_num <- input$crosstab_vars[
    sapply(df[input$crosstab_vars], is.numeric)
  ]
  
  if (length(vars_num) == 0) return(NULL)
  
  plot_data <- df |>
    dplyr::select(Class, dplyr::all_of(vars_num)) |>
    tidyr::pivot_longer(
      cols = -Class,
      names_to = "Variable",
      values_to = "Value"
    ) |>
    dplyr::group_by(Class, Variable) |>
    dplyr::summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD   = sd(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(plot_data, aes(x = Class, y = Mean, fill = Class)) +
    geom_col(width = 0.6) +
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      width = 0.2
    ) +
    facet_wrap(~ Variable, scales = "free_y") +
    labs(
      title = "Continuous Variables by Class",
      y = "Mean (± SD)",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
      legend.position = "none",
      axis.line = element_line(color = "black")
    )
})


# ==== R Console Output ====
observeEvent(lca_models(), {
  req(lca_models(), input$best_class_lca)
  model_k <- lca_models()$polca[[as.character(input$best_class_lca)]]
  if (!is.null(model_k)) {
    console_context$text <- paste(capture.output(print(model_k)), collapse = "\n")
  }
})

# ==== AI Assistant context ====
observe({
  res_text <- ""
  if (!is.null(lca_models())) {
    gof <- lca_models()$glca
    if (!is.null(gof)) {
      res_text <- paste0(
        "=== LCA MODEL FIT COMPARISON ===\n",
        paste(capture.output(print(as.data.frame(gof))), collapse = "\n"), "\n\n"
      )
    }
    if (!is.null(input$best_class_lca)) {
      model_k <- lca_models()$polca[[as.character(input$best_class_lca)]]
      if (!is.null(model_k)) {
        res_text <- paste0(
          res_text,
          "=== SELECTED MODEL (", input$best_class_lca, " CLASSES) ===\n",
          paste(capture.output(print(model_k)), collapse = "\n"), "\n\n",
          "=== CLASS PREVALENCE ===\n",
          paste(capture.output(print(round(model_k$P, 4))), collapse = "\n"), "\n\n",
          "=== ITEM-RESPONSE PROBABILITIES ===\n",
          paste(capture.output(print(lapply(model_k$probs, function(x) round(x, 4)))), collapse = "\n"), "\n"
        )
      }
    }
  }
  ai_context$results_text <- res_text
  ai_context$module <- "Latent Class Analysis (LCA)"
})

# ==== Save Project (workspace .rds) ====
output$export_lca_rds <- downloadHandler(
  filename = function() paste0("LCA_Workspace_", Sys.Date(), ".rds"),
  content = function(file) {
    req(lca_models())
    workspace <- list(
      type = "projectLSA_workspace",
      module = "LCA",
      version = "0.1.2",
      saved_at = Sys.time(),
      raw_data = data_source(),
      lca_models = lca_models(),
      input_state = list(
        vars_lca = input$vars_lca,
        cov_lca = input$cov_lca,
        use_cov_lca = input$use_cov_lca,
        min_class_lca = input$min_class_lca,
        max_class_lca = input$max_class_lca,
        best_class_lca = input$best_class_lca
      )
    )
    saveRDS(workspace, file)
  }
)

# ==== HTML Report ====
# Per-session directory, so concurrent users never share a report file
lca_report_res <- session_report_dir(session, "lca")
lca_report_path <- reactiveVal(NULL)

generate_lca_report <- function(progress_label = "Generating Report...") {
  if (is.null(lca_models())) return(NULL)

  # The template that sits next to the app wins; an installed copy is only a fallback
  report_path <- "lca_report.Rmd"
  if (!file.exists(report_path)) report_path <- file.path(system.file("app", package = "projectLSA"), "lca_report.Rmd")
  tempReport <- file.path(lca_report_res$path, "lca_report.Rmd")
  file.copy(report_path, tempReport, overwrite = TRUE)

  out_html <- file.path(lca_report_res$path, "lca_report_out.html")

  k <- input$best_class_lca
  model_k <- lca_models()$polca[[as.character(k)]]

  showModal(modalDialog(progress_label, footer = NULL))
  ok <- tryCatch({
    rmarkdown::render(tempReport,
      output_file = out_html,
      params = list(
        fit_compare = tryCatch(as.data.frame(fit_lca()), error = function(e) NULL),
        best_k = k,
        model_best = model_k,
        class_plot = tryCatch(best_model_plot_lca_static(), error = function(e) NULL),
        summary_tbl = tryCatch(summary_data_lca(), error = function(e) NULL),
        selected_vars = input$vars_lca,
        console_out = console_context$text,
        notes = report_notes(),
          ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text
      ),
      envir = new.env(parent = globalenv())
    )
    lca_report_path(out_html)
    TRUE
  }, error = function(e) {
    showNotification(paste("Error rendering report:", e$message), type = "error")
    FALSE
  }, finally = {
    removeModal()
  })

  if (isTRUE(ok)) out_html else NULL
}

observeEvent(input$lca_generate_preview, {
  req(lca_models())
  generate_lca_report("Generating Report Preview...")
})

output$lca_report_preview_frame <- renderUI({
  req(lca_report_path())
  tags$iframe(
    src = paste0(lca_report_res$prefix, "/lca_report_out.html?v=", as.integer(Sys.time())),
    width = "100%", height = "800px", style = "border: none;"
  )
})

output$download_report_lca <- downloadHandler(
  filename = function() paste0("LCA_Report_", Sys.Date(), ".html"),
  content = function(file) {
    req(lca_models())
    path <- lca_report_path()
    if (is.null(path) || !file.exists(path)) {
      path <- generate_lca_report("Generating Report...")
    }
    req(path)
    file.copy(path, file, overwrite = TRUE)
  },
  contentType = "text/html"
)

output$download_word_lca <- downloadHandler(
  filename = function() paste0("LCA_Report_", Sys.Date(), ".docx"),
  content = function(file) {
    req(lca_models())
    withProgress(message = "Building the Word report", value = 0.4, {
      k <- input$best_class_lca
      plots <- list(`BIC and AIC across the number of classes` = fit_plot_lca_reactive,
                    `Entropy and average posterior probability across the number of classes` = entropy_plot_lca_reactive)
      mono <- identical(input$fig_mode, "mono")
      plots <- lapply(plots, function(f) { p <- tryCatch(f(), error = function(e) NULL); if (mono && inherits(p, "ggplot")) pl_mono(p) else p })
      render_word_report("lca_report_word.Rmd", list(
        fit_compare = tryCatch(as.data.frame(fit_lca()), error = function(e) NULL), best_k = k,
        model_best = lca_models()$polca[[as.character(k)]],
        class_plot = tryCatch(best_model_plot_lca_static(), error = function(e) NULL),
        fit_plots = Filter(Negate(is.null), plots),
        summary_tbl = tryCatch(summary_data_lca(), error = function(e) NULL),
        selected_vars = input$vars_lca, console_out = console_context$text, notes = report_notes(),
        ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text,
        dec = (input$dec_sep %||% ".")
      ), file, file.path(lca_report_res$path, "word"))
    })
  },
  contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
)
}
