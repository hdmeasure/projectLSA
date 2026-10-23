# IRT -----
# ==== Reactive Data IRT ====
server_lta <- function(input, output, session, ai_context, console_context) {
  # text typed (or drafted by the assistant) in the Report form
  report_notes <- server_report_form(input, output, session, "lta", ai_context, method_ctx = function() { req(selected_fit()); list(res = selected_fit()) })

  library(mirt)
  library(plotly)
  library(psych)
  library(shinycssloaders)

  # Set while a saved workspace is being loaded, so the "reset results when the
  # data changes" observer below does not wipe the models we just restored.
  lta_restoring <- reactiveVal(FALSE)

  data_user <- reactive({
    req(input$dimension, input$data_source_lta)
    if (input$dimension == "uni" && input$data_source_lta == "diko") {
      a <- matrix(runif(15, 0.8, 2), ncol = 1)      # discrimination 1 dimension
      d <- matrix(rnorm(15, 0, 1), ncol = 1)       # difficulty
      theta <- matrix(rnorm(750), ncol = 1)       # ability 1 dimension
      resp <- simdata(a = a, d = d, itemtype = "2PL", Theta = theta)
      df <- as.data.frame(resp) %>% tidyr::drop_na()
      df$Group <- sample(c("Group_A", "Group_B"), nrow(df), replace = TRUE)
      
    } else if (input$dimension == "uni" && input$data_source_lta == "poli") {
      a <- matrix(rlnorm(20, .2, .3))  
      diffs <- t(apply(matrix(runif(20*4, .3, 1), 20), 1, cumsum))
      diffs <- -(diffs - rowMeans(diffs))
      d <- diffs + rnorm(20)
      resp <- simdata(a, d, 300, itemtype = 'graded')
      df <- as.data.frame(resp) %>% tidyr::drop_na()
      df$Group <- sample(c("Group_A", "Group_B"), nrow(df), replace = TRUE)
      
    } else if (input$dimension == "multi" && input$data_source_lta == "diko") {
      N <- 750     
      n_items <- 15
      n_factors <- 2
      a <- matrix(0, nrow = n_items, ncol = n_factors)
      a[1:8,1] <- runif(8, 0.5, 1.5)   # loading ke F1
      a[9:15,2] <- runif(7, 0.5, 1.5)  # loading ke F2
      d <- matrix(rnorm(n_items, 0, 1), ncol = 1)
      sigma <- matrix(c(1,0.3,0.3,1), nrow = 2)
      resp <- simdata(a = a, d = d, N = N, itemtype = '2PL', sigma = sigma)
      df <- as.data.frame(resp) %>% tidyr::drop_na()
      df$Group <- sample(c("Group_A", "Group_B"), nrow(df), replace = TRUE)
      
    } else if (input$dimension == "multi" && input$data_source_lta == "poli") {
      df <- as.data.frame(psych::bfi) %>% dplyr::select(A1:E2, gender) %>% tidyr::drop_na()
      df$gender <- factor(df$gender, levels = c(1, 2), labels = c("Male", "Female"))
      colnames(df)[ncol(df)] <- "Gender"
    } else {
      req(input$datafile_lta)
      ext <- tolower(tools::file_ext(input$datafile_lta$name))
      showModal(modalDialog(title = NULL, "Reading Your File, Please wait...", footer = NULL, easyClose = FALSE))
      if (ext == "rds") {
        res <- readRDS(input$datafile_lta$datapath)
        if (is.list(res) && !is.data.frame(res) && identical(res$type, "projectLSA_workspace")) {
          if (identical(res$module, "LTA")) {
            # Restore Workspace State
            lta_restoring(TRUE)
            lta_fit_list(res$lta_fit_list)
            lta_fit_compare(res$lta_fit_compare)
            lta_current_id(res$lta_current_id)
            df <- res$raw_data
            
            # Restore UI Inputs
            if (!is.null(res$input_state)) {
              updateSelectInput(session, "data_source_lta", selected = res$input_state$data_source_lta)
              updateSelectInput(session, "datatype", selected = res$input_state$datatype)
              updateSelectInput(session, "dimension", selected = res$input_state$dimension)
              updateSelectInput(session, "fit_stats", selected = res$input_state$fit_stats)
              if (!is.null(res$input_state$selected_vars)) {
                 updateSelectInput(session, "selected_vars", selected = res$input_state$selected_vars)
              }
              if (!is.null(res$input_state$itemtype)) {
                 updateSelectInput(session, "itemtype", selected = res$input_state$itemtype)
              }
              if (!is.null(res$input_state$lta_model_text)) {
                 updateTextAreaInput(session, "lta_model_text", value = res$input_state$lta_model_text)
              }
            }
            
            showNotification("LTA Workspace restored successfully!", type = "message")
          } else {
            stop("Uploaded workspace belongs to a different module: ", res$module)
          }
        } else if (inherits(res, "SingleGroupClass") || inherits(res, "MultipleGroupClass")) {
          lta_fit_list(list(list(id = "model_1", model = res, type = "Uploaded Model", timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
          lta_current_id("model_1")
          df <- as.data.frame(res@Data$data)
          showModal(modalDialog("LTA Model uploaded successfully! Please navigate to the Summary or Plot tab.", easyClose = TRUE))
        } else {
          df <- res
        }
      } else {
        df <- switch(
          ext,
          "csv"  = data.table::fread(input$datafile_lta$datapath, data.table = FALSE),
          "xls"  = readxl::read_excel(input$datafile_lta$datapath),
          "xlsx" = readxl::read_excel(input$datafile_lta$datapath),
          "sav"  = haven::read_sav(input$datafile_lta$datapath),
          stop("Unsupported file type. Please upload CSV, Excel, SPSS (.sav), or RDS file.")
        )
      }
      removeModal()
      
      if (!"id_auto" %in% names(df)) {
        df <- df %>% mutate(across(everything(), ~ifelse(.x == "", NA, .x)),
                            id_auto = paste0("id_", sprintf("%04d", 1:n())))
      }
    }
    return(df)
  })
  output$id_select_ui <- renderUI({
    req(data_user(), input$dimension, input$data_source_lta)
    selectizeInput("id_cols", "ID columns (optional)", choices = names(data_user()), multiple = TRUE,
                   selected = names(data_user())[grepl("id", names(data_user()), ignore.case = TRUE)])
  })
  output$var_select_ui <- renderUI({
    req(data_user(), input$dimension, input$data_source_lta)
    all_vars <- names(data_user())
    id_cols <- input$id_cols
    choices <- setdiff(all_vars, id_cols)
    selectInput("selected_vars", "Select Variables (used for IRT):", choices = choices, multiple = TRUE, selected = choices)
  })
  
  data_lta <- reactive({
    req(input$dimension, input$data_source_lta, data_user(), input$selected_vars, input$fit_stats)
    df <- data_user() %>% dplyr::select(all_of(input$selected_vars))
    # Exclude non-numeric columns for IRT model fitting
    df %>% dplyr::select(where(is.numeric))
  })
  
  # ==== Preview Data ====
  output$data_preview_lta <- DT::renderDT({
    req(input$dimension, input$data_source_lta, data_user(), input$selected_vars, input$fit_stats,data_lta())
    df <- data_lta() 
    numeric_cols <- which(sapply(df, is.numeric))
    DT::datatable(df, extensions = 'Buttons',
                  options = list(dom='Brtp',scrollX = TRUE, pageLength = 25,  
                                 buttons = list(
                                   list(extend = 'csv',
                                        text = 'Export CSV',
                                        filename = 'Data LTA'
                                   ),
                                   list(extend = 'excel',
                                        text = 'Export Excel',
                                        filename = 'Data LTA'
                                   ))),
                  rownames = TRUE) %>% 
      formatRound(columns = numeric_cols, digits = 0)
  }, server = FALSE)
  output$itemtype_ui <- renderUI({
    req(input$datatype, input$data_source_lta)
    source_type <- if(input$data_source_lta == "upload") {
      input$datatype
    } else {
      input$data_source_lta
    }
    choices <- switch(source_type,
                      "diko" = c("Rasch" = "Rasch",
                                 "2-Parameter Logistic (2-PL)" = "2PL",
                                 "3-Parameter Logistic (3-PL)" = "3PL"),
                      "poli" = c("Partial Credit Model (PCM)" = "Rasch",
                                 "Graded Response Model (GRM)" = "graded",
                                 "Generalized PCM (GPCM)" = "gpcm")
    )
    
    div(
      class = "select-large", # styling selector
      selectInput(
        inputId = "itemtype",
        label = NULL,
        choices = choices,
        selected = choices[[1]],
        width = "100%"
      )
    )
  })
  
  # ==== Reset all stored IRT results when data_user changes ====
  observeEvent(data_user(), {
    # ...unless the new data came from a restored workspace, which carries its
    # own models with it.
    if (isTRUE(lta_restoring())) {
      lta_restoring(FALSE)
      return(invisible(NULL))
    }
    # Reset stored models and current selection
    lta_fit_list(list())
    lta_fit_compare(NULL)
    lta_current_id(NULL)
  })
  # ===== IRT / MIRT MODEL =====
  output$lta_model_ui <- renderUI({
    req(input$data_source_lta, input$dimension=="multi")
    model_text <- switch(input$data_source_lta,
                         # === 1. Multidimensional - Dichotomous ===
                         "diko" = "
# Example 1: Multidimensional - Dichotomous (2 dimensions)
model <- '
F1 = 1-5
F2 = 6-10
'
",
# === 2. Multidimensional - Polytomous ===
"poli" = "
# Example 2: Multidimensional - Polytomous (2 dimensions)
model <- '
F1 = 1-5
F2 = 6-10 
'
"
    )
    tags$textarea(
      id = "lta_model_text",
      rows = 10,
      style = "width:100%; font-family: monospace; font-size:11px;",
      model_text
    )
  })
  
  # ==== IRT storage ====
  lta_fit_list <- reactiveVal(list())
  lta_fit_compare <- reactiveVal(NULL)
  lta_current_id <- reactiveVal(NULL)
  
  # ==== Fungsi untuk menjalankan model ====
  run_mirt_model <- function(data, model_def, itemtype) {
    tryCatch({
      mirt(data, model = model_def, itemtype = itemtype, verbose = FALSE)
    }, error = function(e) {
      showNotification(paste("Model", itemtype, "error:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # ==== IRT Main Run ====
  observeEvent(input$run_lta, {
    req(data_lta(), input$selected_vars, input$fit_stats)
    # Tambahkan kondisi khusus untuk upload
    if (input$data_source_lta == "upload") {
      req(input$datatype)
    }
    updateTabsetPanel(session, "main_tab_lta", selected = "summary_tab")
    df <- data_lta()
    # Model
    source_type <- if(input$data_source_lta == "upload") {
      req(input$datatype)   # pastikan user sudah pilih
      input$datatype
    } else {
      input$data_source_lta
    }
    
    itemtypes <- switch(source_type,
                        "diko" = c("Rasch","2PL","3PL"),
                        "poli" = c("Rasch","graded","gpcm"))
    # Define Model Syntax
    model_def <- if (input$dimension == "multi") {
      mirt.model(input$lta_model_text,itemnames = df)
    }  else {1}
    showModal(modalDialog("Running Item Response Theory ...", footer = NULL))
    
    fit_results <- list()
    for (item in itemtypes) {
      mod <- run_mirt_model(df, model_def, item)
      if (is.null(mod)) next
      measures <- tryCatch({
        as.data.frame(anova(mod))
      }, error = function(e) data.frame())
      
      N_itemfit <- tryCatch({
        itemfit_df <- as.data.frame(mirt::itemfit(mod, fit_stats=input$fit_stats))
        p_cols <- grep("^p\\.", names(itemfit_df), value = TRUE)
        sum(itemfit_df[[p_cols]] >= 0.05, na.rm = TRUE)
      }, error = function(e) 0)
      
      LD <- tryCatch({
        Q3_mat <- residuals(mod, type = "Q3", verbose = FALSE)
        # ambil item names
        items <- colnames(Q3_mat)
        # ekstrak upper triangle
        idx <- which(upper.tri(Q3_mat), arr.ind = TRUE)
        q3_long <- data.frame(
          item  = items[idx[,2]],
          item2 = items[idx[,1]],
          Q3    = Q3_mat[upper.tri(Q3_mat)]
        )
        violations <- q3_long %>% 
          dplyr::filter(abs(Q3) > 0.2)
        unique_items <- unique(c(violations$item, violations$item2))
        N_LD <- length(unique_items)
        list(N_LocalDependency = N_LD,
             item_LocalDep = unique_items
        )
      }, error = function(e) {
        list(N_violations = NULL, itemLD = NA)}
      )
      fit_results[[item]] <- list(
        id = item,
        model_type = item,
        fit = mod,
        measures = measures,
        N_itemfit = N_itemfit,
        LD = LD,
        time = Sys.time()
      )
    }
    # ---- Model comparison (if possible) ----
    models_for_anova <- lapply(fit_results, function(x) x$fit)
    models_for_anova <- models_for_anova[!sapply(models_for_anova, is.null)]
    lta_fit_compare(
      switch(source_type,
             "diko" = as.data.frame(anova(models_for_anova[['Rasch']],
                                          models_for_anova[['2PL']],
                                          models_for_anova[['3PL']])),
             "poli" = rbind(
               as.data.frame(anova(models_for_anova[['Rasch']],
                                   models_for_anova[['graded']]
               )),
               as.data.frame(anova(models_for_anova[['Rasch']],
                                   models_for_anova[['gpcm']]))[2,]
             )
      ))
    removeModal()
    
    cur <- lta_fit_list()
    cur <- modifyList(cur, fit_results)
    best_name <- input$itemtype
    
    all_model_types <- sapply(cur, function(x) x$model_type)
    match_idx <- which(tolower(all_model_types) == tolower(best_name))
    
    if (length(match_idx) == 0) {
      best_id <- names(cur)[1]
    } else {
      best_id <- names(cur)[match_idx[1]]
    }
    lta_fit_list(cur)
    lta_current_id(best_id)
    
    showNotification(paste0("LTA finished. Selected model: ", best_name), type = "message")
    
  })
  
  output$fit_comparison <- renderUI({
    req(data_lta(), input$selected_vars, input$fit_stats, lta_fit_list())
    source_type <- if(input$data_source_lta == "upload") {
      input$datatype
    } else {
      input$data_source_lta
    }
    all_measures <- do.call(rbind, lapply(lta_fit_list(), function(x) x$measures)) %>% 
      tibble::rownames_to_column("Model") %>%
      dplyr::mutate(
        Model = dplyr::case_when(
          source_type == 'poli' & Model == 'Rasch'   ~ 'PCM',
          source_type == 'poli' & Model == 'graded'  ~ 'GRM',
          source_type == 'poli' & Model == 'gpcm'    ~ 'GPCM',
          TRUE                                    ~ Model
        )
      ) %>% 
      dplyr::select(Model)
    
    N_itemfit <- do.call(rbind, lapply(lta_fit_list(), function(x) x$N_itemfit))
    N_itemLD <- do.call(rbind, lapply(lta_fit_list(), function(x) x$LD$N_LocalDependency))
    item_LocalDep <- do.call(rbind, lapply(lta_fit_list(), function(x) x$LD$item_LocalDep))
    comparison <- cbind(all_measures,lta_fit_compare(),
                        N_itemfit, N_itemLD) %>% dplyr::rename(`p (χ²)`=p, 'χ²'=X2)
    comparison <- comparison %>% 
      dplyr::mutate(
        AIC   = round(AIC, 2),
        SABIC = round(SABIC, 2),
        BIC   = round(BIC, 2),
        HQ    = round(HQ, 2),
        logLik = round(logLik,2),
        `χ²` = round(`χ²`,2),
        df=df,
        `p (χ²)` = case_when(
          is.na(p (`p (χ²)`)) ~ NA_character_,
          `p (χ²)` < 0.01 ~ "p <.01",
          `p (χ²)` < 0.05  ~ "p <.05",
          TRUE      ~ "p >.05"
        ),
        N_itemfit =  N_itemfit,
        N_itemLD = N_itemLD     
      )
    min_AIC   <- min(comparison$AIC, na.rm = TRUE)
    min_SABIC <- min(comparison$SABIC, na.rm = TRUE)
    min_BIC   <- min(comparison$BIC, na.rm = TRUE)
    min_HQ    <- min(comparison$HQ, na.rm = TRUE)
    max_Nfit  <- max(comparison$N_itemfit , na.rm = TRUE)
    minLD  <- min(comparison$N_itemLD, na.rm = TRUE)
    tbl <- (DT::datatable(comparison,extensions = 'Buttons',
                         options = list(scrollX = TRUE, pageLength = 10,
                                        dom = 'B',
                                        buttons = list(
                                          list(
                                            extend = 'csv',
                                            text = 'Export CSV',
                                            filename = 'Fit Comparison'  # <-- nama file CSV
                                          ),
                                          list(
                                            extend = 'excel',
                                            text = 'Export Excel',
                                            filename = 'Fit Comparison'  # <-- nama file Excel
                                          ))),
                         rownames = FALSE) %>%
             # === Highlight ===
             formatStyle(
               columns = 'AIC',
               color = styleEqual(min_AIC, '#065f58'),
               backgroundColor = styleEqual(min_AIC, '#d9f1ee'),
               fontWeight = styleEqual(min_AIC, 'bold')
             ) %>%
             formatStyle(
               columns = 'BIC',
               color = styleEqual(min_BIC, '#065f58'),
               backgroundColor = styleEqual(min_BIC, '#d9f1ee'),
               fontWeight = styleEqual(min_BIC, 'bold')
             ) %>%
             formatStyle(
               columns = 'SABIC',
               color = styleEqual(min_SABIC, '#065f58'),
               backgroundColor = styleEqual(min_SABIC, '#d9f1ee'),
               fontWeight = styleEqual(min_SABIC, 'bold')
             ) %>%
             formatStyle(
               columns = 'HQ',
               color = styleEqual(min_HQ, '#065f58'),
               backgroundColor = styleEqual(min_HQ, '#d9f1ee'),
               fontWeight = styleEqual(min_HQ, 'bold')
             ) %>%
             # === Highlight untuk kolom N_itemfit (nilai terbesar) ===
             formatStyle(
               columns = 'N_itemfit',
               color = styleEqual(max_Nfit, '#065f58'),
               backgroundColor = styleEqual(max_Nfit, '#d9f1ee'),
               fontWeight = styleEqual(max_Nfit, 'bold')
             )  %>% 
             formatStyle(
               columns = 'N_itemLD',
               color = styleEqual(minLD, '#065f58'),
               backgroundColor = styleEqual(minLD, '#d9f1ee'),
               fontWeight = styleEqual(minLD, 'bold')
             ) %>% 
             formatStyle(
               columns = 'p (χ²)',
               backgroundColor = styleEqual(
                 c('p <.01', 'p <.05'),
                 c('#d9f1ee', '#d9f1ee')
               )
             ))
    chip <- function(k, v) tags$span(class = "pl-chip-kv", tags$small(k), tags$b(v))
    best_of <- function(x, fun) paste(comparison$Model[which(x == fun(x, na.rm = TRUE))], collapse = ", ")
    tagList(
      tags$div(class = "pl-chips",
               chip("Lowest AIC", best_of(comparison$AIC, min)), chip("Lowest BIC", best_of(comparison$BIC, min)),
               chip("Most items fitting", best_of(comparison$N_itemfit, max))),
      tbl,
      tags$p(class = "pl-stat-note",
             "Green marks the best value in a column. Local dependence is flagged when |Q3| > .20 (Yen, 1984). ",
             "p < .05 in the last test means the more complex model fits better than the simpler one (Desjardins & Bulut, 2018).")
    )
  })
  # === Helper: Get the currently selected model based on input$itemtype ===
  selected_fit <- reactive({
    req(lta_fit_list(), input$itemtype)
    all_models <- lta_fit_list()
    match_idx <- which(
      tolower(sapply(all_models, function(x) x$model_type)) == tolower(input$itemtype)
    )
    if (length(match_idx) == 0) {
      showNotification(paste0("Model '", input$itemtype, "' not found."), type = "warning")
      return(NULL)
    }
    all_models[[match_idx[1]]]$fit
  })
  
  # === Item Summary (reactive to selected model) ===
  output$item_summary <- renderUI({
    req(selected_fit())
    itemparam <- as.data.frame(mirt::coef(selected_fit(), IRTpars = TRUE, simplify = TRUE)$items)
    itemfit_all <- as.data.frame(mirt::itemfit(selected_fit(), fit_stats = input$fit_stats))
    itemfit <- itemfit_all %>% 
      dplyr::select (paste0(input$fit_stats), paste0('p.',input$fit_stats)) %>% 
      dplyr::rename(`χ²_itemfit` = paste0(input$fit_stats), `p(χ²)_itemfit` = paste0('p.',input$fit_stats))
    rm_col <- paste0("RMSEA.", input$fit_stats)
    if (rm_col %in% names(itemfit_all)) itemfit$RMSEA <- itemfit_all[[rm_col]]
    
    item_summary <- cbind(itemparam,itemfit) %>% tibble::rownames_to_column("item")
    numeric_cols <- which(sapply(item_summary, is.numeric))
    
    DT::datatable(item_summary,extensions = 'Buttons',
                  options = list(scrollX = TRUE, dom = 'Brtp',
                                 buttons = list(
                                   list(
                                     extend = 'csv',
                                     text = 'Export CSV',
                                     filename = paste0('Item Summary-',input$itemtype)  
                                   ),
                                   list(
                                     extend = 'excel',
                                     text = 'Export Excel',
                                     filename = paste0('Item Summary-',input$itemtype)
                                   ))), 
                  rownames = FALSE) %>%
      formatRound(columns = numeric_cols, digits = 2) %>% 
      DT::formatStyle(columns = "p(χ²)_itemfit",
                      fontWeight = 'bold',
                      color = styleInterval(0.04999999, c('#8f1d2a', '#065f58'))
      )
  })
  
  
  # ---- chips under the model selector: what is being inspected ----
  output$lta_model_chips <- renderUI({
    req(selected_fit())
    m <- selected_fit()
    chip <- function(k, v) tags$span(class = "pl-chip-kv", tags$small(k), tags$b(v))
    nf <- tryCatch(m@Model$nfact, error = function(e) NA)
    it <- tryCatch(paste(unique(mirt::extract.mirt(m, "itemtype")), collapse = ", "), error = function(e) "")
    tags$div(class = "pl-chips",
             chip("Items", tryCatch(ncol(m@Data$data), error = function(e) "")),
             chip("Respondents", tryCatch(nrow(m@Data$data), error = function(e) "")),
             chip("Dimensions", if (is.na(nf)) "" else nf),
             chip("Item type", it),
             chip("log-likelihood", tryCatch(sprintf("%.1f", mirt::extract.mirt(m, "logLik")), error = function(e) "")),
             chip("BIC", tryCatch(sprintf("%.1f", mirt::extract.mirt(m, "BIC")), error = function(e) "")))
  })

  # ---- views that depend on the dimensionality of the selected model ----
  output$lta_curves_ui <- renderUI({
    req(selected_fit())
    nf <- tryCatch(selected_fit()@Model$nfact, error = function(e) 1)
    if (nf > 2) return(tags$p(class = "pl-stat-note", "Item curves are drawn for models with one or two dimensions. See Loadings and Information for this model."))
    tagList(uiOutput("select_icc"), plotOutput("icc", height = "340px"))
  })
  output$lta_info_ui <- renderUI({
    req(selected_fit())
    nf <- tryCatch(selected_fit()@Model$nfact, error = function(e) 1)
    if (identical(input$dimension, "uni") || nf == 1) plotOutput("infose", height = "340px") else uiOutput("info_ui_multi")
  })

  # ---- loadings of a multidimensional model (oblimin rotation) ----
  output$lta_loadings <- renderUI({
    req(selected_fit())
    nfx <- tryCatch(selected_fit()@Model$nfact, error = function(e) 1)
    sm <- tryCatch(suppressWarnings(if (nfx > 1) mirt::summary(selected_fit(), rotate = "oblimin", verbose = FALSE) else mirt::summary(selected_fit(), verbose = FALSE)), error = function(e) NULL)
    Lraw <- sm$rotF
    if (is.null(sm) || is.null(Lraw)) return(tags$p(class = "pl-stat-note", "The loadings could not be computed for this model."))
    L <- as.matrix(Lraw); max.idx <- apply(abs(L), 1, which.max)
    hdr <- tags$tr(tags$th("Item"), lapply(colnames(L), tags$th), tags$th("h\u00b2"))
    h2 <- if (!is.null(sm$h2)) as.numeric(sm$h2) else rep(NA_real_, nrow(L))
    body <- lapply(seq_len(nrow(L)), function(i) tags$tr(tags$td(class = "pl-lbl", rownames(L)[i]),
      lapply(seq_len(ncol(L)), function(j) tags$td(class = if (j == max.idx[i]) "is-max" else NULL, sprintf("%.3f", L[i, j]))),
      tags$td(if (is.na(h2[i])) "" else sprintf("%.3f", h2[i]))))
    fc <- if (nfx > 1 && !is.null(sm$fcor)) as.matrix(sm$fcor) else NULL
    tagList(
      tags$p(class = "pl-stat-note", if (nfx > 1) "Standardized loadings after an oblimin rotation; " else "Standardized loadings; ", "the largest loading of each item is shaded. h\u00b2 is the communality."),
      tags$div(class = "pl-table-wrap", tags$table(class = "pl-table", tags$thead(hdr), tags$tbody(body))),
      if (!is.null(fc)) tagList(
        tags$h5(class = "pl-sub", "Factor correlations"),
        tags$div(class = "pl-table-wrap", tags$table(class = "pl-table",
          tags$thead(tags$tr(tags$th(""), lapply(colnames(fc), tags$th))),
          tags$tbody(lapply(seq_len(nrow(fc)), function(i) tags$tr(tags$td(class = "pl-lbl", rownames(fc)[i]), lapply(fc[i, ], function(x) tags$td(sprintf("%.3f", x)))))))))
    )
  })

  # ---- local dependence: Q3 residual correlations, flagged beyond |.20| ----
  output$lta_q3 <- renderUI({
    req(selected_fit())
    q <- tryCatch(suppressWarnings(mirt::residuals(selected_fit(), type = "Q3", digits = 3, verbose = FALSE)), error = function(e) NULL)
    if (is.null(q)) return(tags$p(class = "pl-stat-note", "Q3 could not be computed for this model."))
    q <- as.matrix(q); q[upper.tri(q, diag = TRUE)] <- NA
    flag <- !is.na(q) & abs(q) > 0.2
    hdr <- tags$tr(tags$th(""), lapply(colnames(q), tags$th))
    body <- lapply(seq_len(nrow(q)), function(i) tags$tr(tags$td(class = "pl-lbl", rownames(q)[i]),
      lapply(seq_len(ncol(q)), function(j) tags$td(class = if (flag[i, j]) "is-flag" else NULL, if (is.na(q[i, j])) "" else sub("^(-?)0\\.", "\\1.", sprintf("%.2f", q[i, j]))))))
    tagList(
      tags$p(class = "pl-stat-note", sprintf("%d item pair%s above |.20| (shaded). Pairs flagged here share variance beyond the latent trait (Yen, 1984).", sum(flag), if (sum(flag) == 1) "" else "s")),
      tags$div(class = "pl-table-wrap", tags$table(class = "pl-table pl-q3", tags$thead(hdr), tags$tbody(body)))
    )
  })

  output$icc_info <- renderUI({
    req(input$dimension, selected_fit() )
    nfactors <- selected_fit()@Model$nfact
    if (input$dimension =='uni')
    {
      column(12,
             column(6,
                    plotOutput('icc'),
                    uiOutput('select_icc'),    
             ),
             column(6, plotOutput('infose')))
    } else if (input$dimension =='multi' && nfactors == 2 ) {
      column(12,
             plotOutput('icc'),
             uiOutput('select_icc'),  
             uiOutput("info_ui_multi")
      )
    }
    else if (input$dimension =='multi' && nfactors >= 3 )
    {
      column(12,
             uiOutput("info_ui_multi")
      )
    }
    else{return(NULL)}
  })
  
  # ---- which item curves to show: a toolbar above the plot, with one-at-a-time browsing ----
  output$select_icc <- renderUI({
    req(data_lta())
    nm <- names(data_lta())
    tags$div(
      class = "pl-icc-bar",
      tags$span(class = "pl-icc-label", icon("list-check"), "Items shown"),
      tags$div(class = "pl-icc-select",
               selectizeInput("item_select_icc", NULL, choices = nm, selected = nm, multiple = TRUE, width = "100%",
                              options = list(plugins = list("remove_button"), placeholder = "Choose the items to draw"))),
      tags$div(class = "pl-icc-btns",
               actionButton("icc_prev", icon("chevron-left"), class = "pl-icc-btn", title = "Previous item"),
               actionButton("icc_next", icon("chevron-right"), class = "pl-icc-btn", title = "Next item"),
               actionButton("icc_all", "All items", class = "pl-icc-btn pl-icc-all")),
      tags$small(class = "pl-icc-hint", "Pick one item to see its curve alone, or use the arrows to go through the items one by one.")
    )
  })
  observeEvent(input$icc_all, { updateSelectizeInput(session, "item_select_icc", selected = names(data_lta())) })
  step_icc <- function(by) {
    nm <- names(data_lta()); sel <- input$item_select_icc
    cur <- if (length(sel) == 1) match(sel, nm) else if (by > 0) 0L else 2L
    nxt <- ((cur - 1 + by) %% length(nm)) + 1
    updateSelectizeInput(session, "item_select_icc", selected = nm[nxt])
  }
  observeEvent(input$icc_next, step_icc(1))
  observeEvent(input$icc_prev, step_icc(-1))

  # === ICC Plot (reactive to itemtype) ===
  output$icc <- renderPlot({
    req(selected_fit())
    all_names <- names(data_lta())
    sel <- input$item_select_icc
    which_idx <- if (length(sel) == 0) seq_along(all_names) else which(all_names %in% sel)
    if (length(which_idx) == 0) return()
    sub <- if (length(which_idx) == 1) paste0(" - ", all_names[which_idx]) else ""
    mirt::plot(selected_fit(), type = "trace", which.items = which_idx, main = paste0("ICC", sub, " (", input$itemtype, ")"))
  })
  
  # === IIF Plot (reactive to itemtype, using custom function) ===
  output$infose <- renderPlot({
    req(selected_fit(), input$dimension == "uni")
    plot_info_se(selected_fit())
  })
  
  observeEvent(input$dimension, {
    output$info_ui_multi <- renderUI({
      if (input$dimension == "multi") {
        infoVisUI("info1")
      } else {
        NULL
      }
    })
  })
  
  # Jalankan modul server hanya jika dimensi = multi
  observeEvent(input$dimension, {
    req(input$dimension == "multi")
    infoVisServer("info1", model_reactive = reactive({ selected_fit() }))
  })
  
  # ==== Factor Score ====
  fscore_data <- reactive({
    req(selected_fit())
    
    tryCatch({
      as.data.frame(fscores(selected_fit(), method = "EAP", full.scores = TRUE))
    }, error = function(e) data.frame())
  })
  
  output$fscoreLTA <- renderUI({
    df <- fscore_data()
    req(nrow(df) > 0)
    
    numeric_cols <- which(sapply(df, is.numeric))
    
    tags$div(
      class = "pl-fs",
      tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Factor scores (EAP)"), DTOutput("fscore_table")),
      tags$section(
        class = "pl-flat",
        tags$h4(class = "pl-flat-title", "Score categories"),
        tags$div(class = "pl-fs-ctl",
                 numericInput("n_kat", "Number of categories", value = 3, min = 2, max = 10, width = "120px"),
                 uiOutput("cutoff_inputs")),
        tags$p(class = "pl-hint", "Leave the cut-offs empty to split by quantiles, or type your own."),
        tags$div(class = "pl-fs-donuts",
                 lapply(numeric_cols, function(i) plotOutput(paste0("fscore_donut_", i), height = 340)))
      )
    )
  })
  output$fscore_table <- DT::renderDT({
    df <- fscore_data()
    req(nrow(df) > 0)
    numeric_cols <- which(sapply(df, is.numeric))
    DT::datatable(
      df,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE, scrollY = "520px", scrollCollapse = TRUE, paging = FALSE,
        dom = "Brt",
        buttons = list(list(extend = "excel", text = "Export Excel", filename = paste0("Factor Scores-", input$itemtype),
                            exportOptions = list(modifier = list(page = "all"))))
      ),
      rownames = TRUE
    ) %>%
      formatRound(columns = numeric_cols, digits = 2)
  }, server = FALSE)
  output$cutoff_inputs <- renderUI({
    req(input$n_kat)
    
    n_cut <- input$n_kat - 1
    
    tags$div(class = "pl-fs-cuts", lapply(1:n_cut, function(i) {
        
        numericInput(paste0("cut_", i), label = paste("Cut-off", i), value = NA, step = 0.01, width = "90px")
        
      })
    )
  })
  observe({
    df <- fscore_data()
    req(nrow(df) > 0)
    
    numeric_cols <- which(sapply(df, is.numeric))
    
    n_kat <- input$n_kat   # default 3 kategori
    
    lapply(seq_along(numeric_cols), function(idx) {
      local({
        col_index <- numeric_cols[idx]
        plotname  <- paste0("fscore_donut_", col_index)
        output[[plotname]] <- renderPlot({
          
          scores <- df[[col_index]]
          n_kat <- input$n_kat
          n_cut <- n_kat - 1
          
          # Ambil cut-off manual
          cutoffs <- numeric(0)
          for (i in 1:n_cut) {
            val <- input[[paste0("cut_", i)]]
            if (!is.null(val) && !is.na(val)) cutoffs <- c(cutoffs, val)
          }
          
          # Jika cut-off belum lengkap → fallback quantile
          if (length(cutoffs) != n_cut) {
            cuts <- quantile(scores, probs = seq(0, 1, length.out = n_kat + 1))
          } else {
            cuts <- c(min(scores), sort(cutoffs), max(scores))
          }
          
          # Kategorisasi
          kategori <- cut(scores, breaks = cuts, include.lowest = TRUE, labels = FALSE)
          
          # Frekuensi
          freq <- table(kategori)
          df_plot <- data.frame(
            kategori = factor(names(freq)),
            freq = as.numeric(freq)
          )
          
          df_plot$percent <- df_plot$freq / sum(df_plot$freq) * 100
          df_plot$label <- paste0(df_plot$freq, " (", round(df_plot$percent), "%)")
          
          # ===== Legend label (θ ranges) =====
          legend_labels <- c()
          for (i in 1:n_kat) {
            low  <- cuts[i]
            high <- cuts[i+1]
            
            if (i == 1) {
              legend_labels[i] <- paste0("\u03b8 \u2264 ", round(high, 2))
            } else if (i == n_kat) {
              legend_labels[i] <- paste0("\u03b8 > ", round(low, 2))
            } else {
              legend_labels[i] <- paste0(round(low, 2), " < \u03b8 \u2264 ", round(high, 2))
            }
          }
          
          # Quiet sequential palette: light to deep brand colour, low to high scores
          colors <- grDevices::colorRampPalette(c("#dcd8f6", "#8a7fe0", "#3b2fa6"))(max(n_kat, 2))[seq_len(n_kat)]
          lum <- function(h) { v <- grDevices::col2rgb(h) / 255; sum(c(.2126, .7152, .0722) * ifelse(v <= .03928, v / 12.92, ((v + .055) / 1.055)^2.4)) }
          ord <- as.integer(as.character(df_plot$kategori))
          txt <- ifelse(vapply(colors, lum, numeric(1)) > .35, "#172033", "#ffffff")[ord]

          # ====== DONUT: thin ring with rounded ends, share in the ring, counts in the legend ======
          lg <- paste0(legend_labels, "  (n = ", df_plot$freq[match(seq_len(n_kat), ord)], ")")
          pl_donut(df_plot$freq, colors[ord], lg[ord], title = paste0("Factor score: ", colnames(df)[col_index]),
                   center = paste0("F", which(numeric_cols == col_index)), center_sub = "score", text_cols = txt)
        })
        
      })
    })
  })
  
  # ====== INFORMATION & RELIABILITY TAB ======
  output$info_rel_ui <- renderUI({
    req(selected_fit())
    mod <- selected_fit()
    
    # Calculate Marginal Reliability
    marg_rel <- tryCatch({
      mirt::marginal_rxx(mod)
    }, error = function(e) NA)
    
    # Generate TIF table
    theta_seq <- matrix(seq(-4, 4, by = 0.5))
    tif_vals <- tryCatch({
      mirt::testinfo(mod, theta_seq)
    }, error = function(e) rep(NA, length(theta_seq)))
    
    tif_df <- data.frame(
      Theta = as.numeric(theta_seq),
      Information = tif_vals
    )
    
    # Render
    tagList(
      h4(icon("check-circle"), "Marginal Reliability"),
      tags$p("Marginal reliability estimate(s) for the factors in this model:"),
      verbatimTextOutput("marg_rel_out"),
      tags$hr(),
      h4(icon("table"), "Test Information Function (TIF) Table"),
      tags$p("Test information values across different levels of the latent trait (\u03B8):"),
      DTOutput("tif_table")
    )
  })
  
  output$marg_rel_out <- renderPrint({
    req(selected_fit())
    mod <- selected_fit()
    mirt::marginal_rxx(mod)
  })
  
  output$tif_table <- renderDT({
    req(selected_fit())
    mod <- selected_fit()
    theta_seq <- matrix(seq(-4, 4, by = 0.5))
    tif_vals <- mirt::testinfo(mod, theta_seq)
    
    df <- data.frame(
      Theta = as.numeric(theta_seq),
      Information = round(tif_vals, 3)
    )
    
    datatable(df, options = list(pageLength = 10, dom = 'rtip'))
  })
  
  # ====== DIF ANALYSIS TAB ======
  output$dif_ui <- renderUI({
    req(data_user())
    
    # Find categorical variables for grouping
    df <- data_user()
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) || length(unique(x)) <= 5)]
    
    if (length(cat_vars) == 0) {
      return(tags$p("No suitable grouping variable found in the dataset for DIF analysis. Please ensure your dataset includes a categorical column (e.g., Gender, Region)."))
    }
    
    tagList(
      fluidRow(
        column(4,
               h4(icon("cogs"), "DIF Settings"),
               selectInput("dif_group_var", "Select Grouping Variable:", choices = cat_vars),
               selectInput("dif_anchor_items", "Anchor Items (optional):", 
                           choices = c("None", input$selected_vars), selected = "None", multiple = TRUE),
               actionButton("run_dif", "Run DIF Analysis", class = "btn-primary", icon = icon("play"))
        ),
        column(8,
               h4(icon("table"), "DIF Results (mirt)"),
               withSpinner(DTOutput("dif_results_table"))
        )
      )
    )
  })
  
  dif_results <- eventReactive(input$run_dif, {
    req(selected_fit(), input$dif_group_var, data_user())
    
    df <- data_user()
    group_var <- df[[input$dif_group_var]]
    if (length(unique(na.omit(group_var))) < 2) {
      showNotification("Grouping variable must have at least 2 categories.", type = "error")
      return(NULL)
    }
    
    # The selected_fit() model provides the variable names
    mod <- selected_fit()
    vars <- mirt::extract.mirt(mod, "itemnames")
    mod_syntax <- NULL # If custom, wait we don't have model_syntax easily here. Let's just fit standard multiple group.
    
    # We need to fit a multiple group model for DIF
    # Use mirt::multipleGroup
    item_data <- df[, vars, drop = FALSE]
    
    # Simplified approach: If we can't easily refit, we might just use Wald test or likelihood ratio.
    # To use mirt::DIF, we first fit a multipleGroup model where all items are constrained to be equal across groups.
    showNotification("Fitting multiple group model... this may take a while.", id = "dif_msg", duration = NULL)
    
    tryCatch({
      # Base constrained model
      if (is.null(mod_syntax) || mod_syntax == "") {
         mg_model <- mirt::multipleGroup(item_data, 1, group = as.character(group_var), invariance = c('free_means', 'free_var', 'slopes', 'intercepts'))
      } else {
         model_def <- mirt::mirt.model(mod_syntax)
         mg_model <- mirt::multipleGroup(item_data, model_def, group = as.character(group_var), invariance = c('free_means', 'free_var', 'slopes', 'intercepts'))
      }
      
      items2test <- vars
      if (!is.null(input$dif_anchor_items) && !("None" %in% input$dif_anchor_items)) {
        items2test <- setdiff(vars, input$dif_anchor_items)
      }
      
      dif_res <- mirt::DIF(mg_model, which.par = c('a1', 'd'), items2test = items2test, p.adjust = "fdr")
      removeNotification("dif_msg")
      return(dif_res)
    }, error = function(e) {
      removeNotification("dif_msg")
      showNotification(paste("DIF Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # ====== PARAMETER INVARIANCE ACROSS GROUPS (multiple-group IRT) ======
  output$inv_ui <- renderUI({
    req(data_user())
    df <- data_user()
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) || length(unique(stats::na.omit(x))) <= 5)]
    if (length(cat_vars) == 0)
      return(tags$p(class = "pl-stat-note", "No grouping variable found. Add a categorical column (for example gender or region) to the data to test invariance."))
    tagList(
      tags$section(class = "pl-flat",
        tags$h4(class = "pl-flat-title", icon("equals"), " Parameter invariance across groups"),
        tags$div(class = "pl-selbar-row",
                 selectInput("inv_group_var", "Grouping variable", choices = cat_vars, width = "260px"),
                 actionButton("run_inv", tagList(icon("play"), " Run invariance tests"), class = "btn btn-success btn-sm")),
        tags$p(class = "pl-hint", "Three nested models are fitted with the item type of the model you selected: configural (all item parameters free in every group), metric (equal slopes) and scalar (equal slopes and intercepts, group means and variances free). Only unidimensional models are tested here.")
      ),
      uiOutput("inv_results")
    )
  })

  inv_fit <- eventReactive(input$run_inv, {
    req(selected_fit(), input$inv_group_var, data_user())
    mod <- selected_fit()
    shiny::validate(shiny::need(mod@Model$nfact == 1, "Invariance testing is available for unidimensional models."))
    vars <- mirt::extract.mirt(mod, "itemnames"); df <- data_user()
    g <- as.character(df[[input$inv_group_var]]); keep <- !is.na(g)
    shiny::validate(shiny::need(length(unique(g[keep])) >= 2, "The grouping variable needs at least two groups."))
    it <- mirt::extract.mirt(mod, "itemtype"); it <- if (length(unique(it)) == 1) unique(it) else it
    dat <- df[keep, vars, drop = FALSE]; g <- g[keep]
    withProgress(message = "Fitting the invariance models", value = 0, {
      fit_one <- function(inv) mirt::multipleGroup(dat, 1, group = g, itemtype = it, invariance = inv, verbose = FALSE)
      incProgress(0.1, detail = "Configural")
      m1 <- fit_one(character(0))
      incProgress(0.3, detail = "Metric")
      m2 <- fit_one(c("free_var", "slopes"))
      incProgress(0.3, detail = "Scalar")
      m3 <- fit_one(c("free_means", "free_var", "slopes", "intercepts"))
      incProgress(0.3)
    })
    list(m1 = m1, m2 = m2, m3 = m3, groups = sort(unique(g)), n = table(g), vars = vars, itemtype = it)
  })

  output$inv_results <- renderUI({
    if (is.null(input$run_inv) || input$run_inv < 1) return(NULL)
    f <- tryCatch(inv_fit(), error = function(e) e)
    if (inherits(f, "error")) return(tags$p(class = "pl-stat-note", conditionMessage(f)))
    ms <- list(Configural = f$m1, `Metric (equal slopes)` = f$m2, `Scalar (equal slopes and intercepts)` = f$m3)
    fm <- function(x, d = 1) ifelse(is.na(x), "", formatC(x, format = "f", digits = d, big.mark = ""))
    npar <- vapply(ms, function(m) as.numeric(mirt::extract.mirt(m, "nest")), numeric(1))
    cmp <- function(a, b) tryCatch(as.data.frame(stats::anova(b, a))[2, ], error = function(e) NULL)   # constrained model first, so the difference is positive
    c12 <- cmp(f$m1, f$m2); c23 <- cmp(f$m2, f$m3)
    row <- function(nm, m, cc) data.frame(Model = nm, Parameters = as.integer(mirt::extract.mirt(m, "nest")), AIC = fm(mirt::extract.mirt(m, "AIC")), BIC = fm(mirt::extract.mirt(m, "BIC")),
      `log-lik` = fm(mirt::extract.mirt(m, "logLik")), dchi = if (is.null(cc)) "" else fm(cc$X2, 2), df = if (is.null(cc)) "" else as.character(cc$df),
      p = if (is.null(cc)) "" else ifelse(cc$p < .001, "< .001", sub("^0", "", formatC(cc$p, format = "f", digits = 3))), check.names = FALSE, stringsAsFactors = FALSE)
    tab <- rbind(row("Configural", f$m1, NULL), row("Metric (equal slopes)", f$m2, c12), row("Scalar (equal slopes and intercepts)", f$m3, c23))
    names(tab)[names(tab) == "dchi"] <- "\u0394\u03c7\u00b2"
    bic <- vapply(ms, function(m) mirt::extract.mirt(m, "BIC"), numeric(1)); best <- names(bic)[which.min(bic)]
    pv <- c(NA, if (is.null(c12)) NA else c12$p, if (is.null(c23)) NA else c23$p)
    tn <- list(NULL, NULL, ifelse(seq_along(bic) == which.min(bic), "good", NA), ifelse(seq_along(bic) == which.min(bic), "good", NA), NULL, NULL, NULL,
               ifelse(is.na(pv), NA, ifelse(pv < .05, "bad", "good")))
    verdict <- paste0("The lowest BIC belongs to the ", tolower(best), " model. ",
                      if (!is.na(pv[2]) && !is.na(pv[3])) paste0("The likelihood-ratio tests ", if (pv[2] >= .05) "do not reject" else "reject", " equal slopes and ", if (pv[3] >= .05) "do not reject" else "reject", " equal intercepts.") else "")
    # item parameters per group (configural model)
    cf <- tryCatch(mirt::coef(f$m1, IRTpars = TRUE, simplify = TRUE), error = function(e) NULL)
    par_tbl <- NULL; p_a <- NULL
    if (!is.null(cf)) {
      gs <- names(cf)
      get <- function(g) as.data.frame(cf[[g]]$items)
      d1 <- get(gs[1]); items <- rownames(d1)
      bcols <- grep("^b", names(d1), value = TRUE)
      par_tbl <- data.frame(Item = items, stringsAsFactors = FALSE)
      for (g in gs) { dg <- get(g); par_tbl[[paste0("a (", g, ")")]] <- fm(dg$a, 2); par_tbl[[paste0("b (", g, ")")]] <- fm(rowMeans(dg[, bcols, drop = FALSE]), 2) }
      if (length(gs) == 2) {
        d2 <- get(gs[2])
        da <- d2$a - d1$a; db <- rowMeans(d2[, bcols, drop = FALSE]) - rowMeans(d1[, bcols, drop = FALSE])
        par_tbl[["\u0394a"]] <- fm(da, 2); par_tbl[["\u0394b"]] <- fm(db, 2)
        flag_b <- abs(db) > 0.5; flag_a <- abs(da) > 0.5
      }
    }
    chip <- function(k, v) tags$span(class = "pl-chip-kv", tags$small(k), tags$b(v))
    tagList(
      tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Nested model comparison"),
        tags$div(class = "pl-chips", lapply(seq_along(f$n), function(i) chip(names(f$n)[i], as.integer(f$n)[i]))),
        fu_table(tab, tones = tn, align_left = 1),
        tags$p(class = "pl-stat-note", verdict, " Green marks the lowest BIC and a likelihood-ratio test that does not reject the added constraints (p \u2265 .05); red marks one that does. ",
               "Invariance is supported when constraining slopes and intercepts does not worsen the fit meaningfully."),
        if (identical(unique(f$itemtype), "Rasch")) tags$p(class = "pl-stat-note", "Rasch-type items already have equal slopes, so the metric model equals the configural one.")),
      if (!is.null(par_tbl)) tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Item parameters by group (configural model)"),
        fu_table(par_tbl, tones = if (length(cf) == 2) c(rep(list(NULL), ncol(par_tbl) - 2), list(ifelse(flag_a, "warn", NA), ifelse(flag_b, "warn", NA))) else NULL),
        tags$p(class = "pl-stat-note", "a is the discrimination and b the difficulty (the mean of the thresholds for polytomous items). Differences larger than 0.5 are shaded: they point to items that behave differently across the groups; the DIF Analysis tab tests this item by item.")),
      tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Item parameters: the other group(s) against the first"),
        tags$div(class = "pl-two pl-two-flat", plotOutput("inv_plot_b", height = "320px"), plotOutput("inv_plot_a", height = "320px")),
        tags$p(class = "pl-stat-note", "Invariant items lie on the solid diagonal; the dashed lines mark a difference of 0.5. Items outside them are labelled."))
    )
  })

  # ====== ITEM INVARIANCE: plots of the item parameters by group ======
  inv_pars <- reactive({
    f <- inv_fit(); cf <- mirt::coef(f$m1, IRTpars = TRUE, simplify = TRUE); gs <- names(cf); req(length(gs) >= 2)
    get <- function(g) { d <- as.data.frame(cf[[g]]$items); data.frame(item = rownames(d), a = d$a, b = rowMeans(d[, grep("^b", names(d)), drop = FALSE])) }
    ref <- get(gs[1]); out <- lapply(gs[-1], function(g) { x <- get(g); data.frame(item = ref$item, group = g, a_ref = ref$a, a = x$a, b_ref = ref$b, b = x$b) })
    list(ref = gs[1], d = do.call(rbind, out))
  })
  inv_scatter <- function(xr, yr, lab, d, what) {
    d$x <- d[[xr]]; d$y <- d[[yr]]; d$diff <- d$y - d$x; d$flag <- abs(d$diff) > 0.5
    rng <- range(c(d$x, d$y), finite = TRUE); pad <- diff(rng) * 0.06; lim <- c(rng[1] - pad, rng[2] + pad)
    ggplot2::ggplot(d, ggplot2::aes(x, y, colour = group, shape = group)) +
      ggplot2::geom_abline(slope = 1, intercept = c(-0.5, 0.5), linetype = "22", colour = "#c9cdda", linewidth = 0.4) +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#8a93a6", linewidth = 0.5) +
      ggplot2::geom_point(size = 2.1, stroke = 0.7, fill = "white") +
      ggplot2::geom_text(data = d[d$flag, ], ggplot2::aes(label = item), size = 2.7, vjust = -0.9, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = pl_palettes$brand, name = NULL) + ggplot2::scale_shape_manual(values = c(21, 22, 24, 23, 25), name = NULL) +
      ggplot2::coord_equal(xlim = lim, ylim = lim) +
      ggplot2::labs(title = paste0(what, ": group vs reference"), x = paste0(what, " in ", lab, " (reference)"), y = paste0(what, " in the other group")) +
      theme_pl(11) + ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold"), legend.position = "bottom")
  }
  output$inv_plot_b <- renderPlot({ p <- inv_pars(); inv_scatter("b_ref", "b", p$ref, p$d, "Difficulty (b)") })
  output$inv_plot_a <- renderPlot({ p <- inv_pars(); inv_scatter("a_ref", "a", p$ref, p$d, "Discrimination (a)") })

  # ====== THETA INVARIANCE: do two halves of the test give the same person estimates? ======
  output$thinv_ui <- renderUI({
    req(selected_fit())
    tagList(
      tags$section(class = "pl-flat",
        tags$h4(class = "pl-flat-title", icon("people-arrows-left-right"), " Invariance of the person estimates (theta)"),
        tags$div(class = "pl-selbar-row",
                 selectInput("thinv_split", "Split the items", width = "300px",
                             choices = c("Easy items vs hard items (by difficulty)" = "diff", "Odd items vs even items" = "oddeven", "First half vs second half" = "half")),
                 actionButton("run_thinv", tagList(icon("play"), " Compare the estimates"), class = "btn btn-success btn-sm")),
        tags$p(class = "pl-hint", "Theta is estimated twice, each time from one half of the items, with the item parameters of the selected model. If the item parameters are invariant, the two estimates agree apart from measurement error, whichever items are used.")
      ),
      uiOutput("thinv_results")
    )
  })
  thinv_res <- eventReactive(input$run_thinv, {
    mod <- selected_fit(); req(mod)
    shiny::validate(shiny::need(mod@Model$nfact == 1, "This comparison is available for unidimensional models."))
    vars <- mirt::extract.mirt(mod, "itemnames"); n <- length(vars)
    shiny::validate(shiny::need(n >= 6, "At least six items are needed to split the test."))
    cf <- mirt::coef(mod, IRTpars = TRUE, simplify = TRUE)$items; b <- rowMeans(cf[, grep("^b", colnames(cf)), drop = FALSE])
    k <- ceiling(n / 2)
    idx <- switch(input$thinv_split %||% "diff",
      diff = { o <- order(b); list(A = o[seq_len(k)], B = o[(k + 1):n]) },
      oddeven = list(A = seq(1, n, 2), B = seq(2, n, 2)),
      half = list(A = seq_len(k), B = (k + 1):n))
    dat <- as.data.frame(mirt::extract.mirt(mod, "data")); names(dat) <- vars
    est <- function(keep) { p <- dat; p[, setdiff(seq_len(n), keep)] <- NA; as.numeric(mirt::fscores(mod, method = "EAP", response.pattern = p)[, "F1"]) }
    A <- est(idx$A); B <- est(idx$B); ok <- is.finite(A) & is.finite(B)
    list(A = A[ok], B = B[ok], nA = length(idx$A), nB = length(idx$B), split = input$thinv_split %||% "diff", labA = switch(input$thinv_split %||% "diff", diff = "easy items", oddeven = "odd items", half = "first half"),
         labB = switch(input$thinv_split %||% "diff", diff = "hard items", oddeven = "even items", half = "second half"))
  })
  output$thinv_results <- renderUI({
    if (is.null(input$run_thinv) || input$run_thinv < 1) return(NULL)
    r <- tryCatch(thinv_res(), error = function(e) e)
    if (inherits(r, "error")) return(tags$p(class = "pl-stat-note", conditionMessage(r)))
    d <- r$A - r$B; N <- length(d); rr <- stats::cor(r$A, r$B); rs <- stats::cor(r$A, r$B, method = "spearman")
    tt <- stats::t.test(r$A, r$B, paired = TRUE)
    rmsd <- sqrt(mean(d^2)); within <- mean(abs(d) <= 0.5) * 100
    # agreement by ability level (average of the two estimates)
    avg <- (r$A + r$B) / 2; lev <- cut(avg, stats::quantile(avg, c(0, 1/3, 2/3, 1)), include.lowest = TRUE, labels = c("Lower third", "Middle third", "Upper third"))
    by <- do.call(rbind, lapply(levels(lev), function(l) { x <- d[lev == l]; data.frame(Level = l, n = length(x), `Mean difference` = fu_fmt(mean(x), 2), SD = fu_fmt(stats::sd(x), 2), RMSD = fu_fmt(sqrt(mean(x^2)), 2), check.names = FALSE) }))
    tab <- data.frame(Statistic = c("Pearson r", "Spearman rho", paste0("Mean difference (", r$labA, " \u2212 ", r$labB, ")"), "SD of the differences", "RMSD", "Within \u00b10.5 logits", "Paired t test"),
                      Value = c(sub("^0", "", fu_fmt(rr, 3)), sub("^0", "", fu_fmt(rs, 3)), fu_fmt(mean(d), 3), fu_fmt(stats::sd(d), 3), fu_fmt(rmsd, 3), sprintf("%d%%", round(within)),
                                paste0("t(", round(tt$parameter), ") = ", fu_fmt(tt$statistic, 2), ", p ", ifelse(tt$p.value < .001, "< .001", paste0("= ", sub("^0", "", fu_fmt(tt$p.value, 3)))))),
                      Reading = c("Agreement between the two sets of estimates; the shorter the halves, the lower it is.", "The same for the rank order.", "Close to zero when neither half is systematically easier.", "Spread of the disagreement.", "Typical size of the disagreement, in theta units.", "Share of respondents whose two estimates differ by 0.5 or less.", "Tests whether the mean difference is zero."),
                      check.names = FALSE, stringsAsFactors = FALSE)
    tn <- list(NULL, c(fu_tone(rr, .8, .6), fu_tone(rs, .8, .6), ifelse(abs(mean(d)) < .1, "good", ifelse(abs(mean(d)) < .25, "warn", "bad")), NA, ifelse(rmsd < .5, "good", ifelse(rmsd < 1, "warn", "bad")), fu_tone(within / 100, .7, .5), ifelse(tt$p.value < .05, "warn", "good")), NULL)
    chip <- function(k, v) tags$span(class = "pl-chip-kv", tags$small(k), tags$b(v))
    tagList(
      tags$section(class = "pl-flat",
        tags$h4(class = "pl-flat-title", "Agreement of the two theta estimates"),
        tags$div(class = "pl-chips", chip("Respondents", N), chip(r$labA, paste(r$nA, "items")), chip(r$labB, paste(r$nB, "items")), chip("r", sub("^0", "", fu_fmt(rr, 2))), chip("RMSD", fu_fmt(rmsd, 2))),
        fu_table(tab, tones = tn, class = "pl-table-text", align_left = c(1, 3))),
      tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Agreement by ability level"), fu_table(by, align_left = 1),
        fu_note("If the differences are larger in one third, the items measure that part of the scale differently.")),
      tags$div(class = "pl-two pl-two-flat",
               tags$section(class = "pl-flat", plotOutput("thinv_scatter", height = "300px")),
               tags$section(class = "pl-flat", plotOutput("thinv_ba", height = "300px")))
    )
  })
  output$thinv_scatter <- renderPlot({
    r <- thinv_res(); d <- data.frame(A = r$A, B = r$B); rng <- range(c(d$A, d$B)); pad <- diff(rng) * 0.05; lim <- c(rng[1] - pad, rng[2] + pad)
    ggplot2::ggplot(d, ggplot2::aes(A, B)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#8a93a6", linewidth = 0.5) +
      ggplot2::geom_point(colour = "#5748d9", alpha = 0.35, size = 1.5) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "#14b8a6", linewidth = 0.7) +
      ggplot2::coord_equal(xlim = lim, ylim = lim) +
      ggplot2::labs(title = "Theta from each half", x = paste("Theta from the", r$labA), y = paste("Theta from the", r$labB)) +
      theme_pl(11) + ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold"))
  })
  output$thinv_ba <- renderPlot({
    r <- thinv_res(); d <- data.frame(m = (r$A + r$B) / 2, diff = r$A - r$B); md <- mean(d$diff); sdv <- stats::sd(d$diff)
    ggplot2::ggplot(d, ggplot2::aes(m, diff)) +
      ggplot2::geom_hline(yintercept = 0, colour = "#8a93a6", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = c(md - 1.96 * sdv, md + 1.96 * sdv), linetype = "22", colour = "#b4414b", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = md, colour = "#14b8a6", linewidth = 0.7) +
      ggplot2::geom_point(colour = "#5748d9", alpha = 0.35, size = 1.5) +
      ggplot2::labs(title = "Difference against the average (Bland-Altman)", x = "Average of the two estimates", y = paste0("Difference (", r$labA, " \u2212 ", r$labB, ")")) +
      theme_pl(11) + ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold"))
  })

  output$dif_results_table <- renderDT({
    req(dif_results())
    res <- dif_results()
    
    if (is.null(res)) return(NULL)
    
    df_res <- as.data.frame(res)
    df_res$Item <- rownames(df_res)
    df_res <- df_res[, c("Item", setdiff(names(df_res), "Item"))]
    
    datatable(df_res, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatRound(columns = 2:ncol(df_res), digits = 3) %>%
      formatStyle(
        'p',
        color = styleInterval(0.05, c('#8f1d2a', 'black')),
        fontWeight = styleInterval(0.05, c('bold', 'normal'))
      )
  })

  # ==== R Console Output & Model Export ====
  observeEvent(selected_fit(), {
    req(selected_fit())
    out <- paste(capture.output(print(selected_fit())), collapse = "\n")
    console_context$text <- out
  })
  
  output$export_lta_rds <- downloadHandler(
    filename = function() { paste0("LTA_Workspace_", Sys.Date(), ".rds") },
    content = function(file) {
      req(selected_fit())
      workspace <- list(
        type = "projectLSA_workspace",
        module = "LTA",
        raw_data = data_user(),
        lta_fit_list = lta_fit_list(),
        lta_fit_compare = lta_fit_compare(),
        lta_current_id = lta_current_id(),
        input_state = list(
          data_source_lta = input$data_source_lta,
          datatype = input$datatype,
          dimension = input$dimension,
          fit_stats = input$fit_stats,
          selected_vars = input$selected_vars,
          itemtype = input$itemtype,
          lta_model_text = input$lta_model_text
        )
      )
      saveRDS(workspace, file)
    }
  )

  # ==== Score New Data ====
  output$download_lta_template <- downloadHandler(
    filename = function() { "LTA_template.xlsx" },
    content = function(file) {
      req(selected_fit())
      items <- mirt::extract.mirt(selected_fit(), "itemnames")
      df <- data.frame(matrix(ncol = length(items), nrow = 0))
      colnames(df) <- items
      writexl::write_xlsx(df, file)
    }
  )

  lta_newscores_reactive <- eventReactive(input$lta_score_newdata_btn, {
    req(selected_fit(), input$lta_newdata)
    ext <- tools::file_ext(input$lta_newdata$name)
    df <- switch(
      ext,
      "csv" = read.csv(input$lta_newdata$datapath),
      "xlsx" = readxl::read_excel(input$lta_newdata$datapath),
      "xls" = readxl::read_excel(input$lta_newdata$datapath),
      stop("Invalid file format")
    )
    
    # ensure only columns from model are used
    items <- mirt::extract.mirt(selected_fit(), "itemnames")
    df_used <- df[, intersect(colnames(df), items), drop = FALSE]
    
    # Calculate scores
    scores <- mirt::fscores(selected_fit(), response.pattern = df_used)
    as.data.frame(scores)
  })

  output$lta_newscores_table <- DT::renderDataTable({
    req(lta_newscores_reactive())
    datatable(round(lta_newscores_reactive(), 3), options = list(scrollX = TRUE))
  })

  output$download_lta_newscores <- downloadHandler(
    filename = function() { "LTA_New_Scores.csv" },
    content = function(file) {
      req(lta_newscores_reactive())
      write.csv(lta_newscores_reactive(), file, row.names = FALSE)
    }
  )

  # ==== AI Assistant ====
  # Update global AI context whenever results change
  observe({
    res_text <- ""
    if (length(lta_fit_list()) > 0 && !is.null(input$itemtype)) {
      fit <- try(selected_fit(), silent = TRUE)
      if (!inherits(fit, "try-error") && !is.null(fit)) {
        res_text <- paste0(
          "=== MODEL COMPARISON ===\n",
          paste(capture.output(print(lta_fit_compare())), collapse = "\n"), "\n\n",
          "=== SELECTED MODEL (", input$itemtype, ") ===\n",
          paste(capture.output(summary(fit)), collapse = "\n"), "\n\n"
        )
        itempar <- try(
          as.data.frame(mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)$items),
          silent = TRUE
        )
        if (!inherits(itempar, "try-error")) {
          res_text <- paste0(
            res_text,
            "=== ITEM PARAMETERS (IRT metric) ===\n",
            paste(capture.output(print(round(itempar, 3))), collapse = "\n"), "\n\n"
          )
        }
        itemfit <- try(
          as.data.frame(mirt::itemfit(fit, fit_stats = input$fit_stats)),
          silent = TRUE
        )
        if (!inherits(itemfit, "try-error")) {
          res_text <- paste0(
            res_text,
            "=== ITEM FIT (", input$fit_stats, ") ===\n",
            paste(capture.output(print(itemfit)), collapse = "\n"), "\n"
          )
        }
      }
    }
    ai_context$results_text <- res_text
    ai_context$module <- "Latent Trait Analysis (LTA / IRT)"
  })

  # Per-session directory, so concurrent users never share a report file
  lta_report_res <- session_report_dir(session, "lta")
  lta_report_path <- reactiveVal(NULL)
  
  observeEvent(input$lta_generate_preview, {
    req(selected_fit())
    
    # The template that sits next to the app wins; an installed copy is only a fallback
    report_path <- "lta_report.Rmd"
    if (!file.exists(report_path)) report_path <- file.path(system.file("app", package = "projectLSA"), "lta_report.Rmd")
    
    tempReport <- file.path(lta_report_res$path, "lta_report.Rmd")
    file.copy(report_path, tempReport, overwrite = TRUE)
    
    out_html <- file.path(lta_report_res$path, "lta_report_out.html")
    
    showModal(modalDialog("Generating Report Preview...", footer = NULL))
    tryCatch({
      rmarkdown::render(tempReport, output_file = out_html,
        params = list(
          lta_res = selected_fit(),
          console_out = console_context$text,
          notes = report_notes(),
          ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text
        )
      )
      lta_report_path(out_html)
    }, error = function(e) {
      showNotification(paste("Error rendering report:", e$message), type = "error")
    }, finally = {
      removeModal()
    })
  })
  
  output$lta_report_preview_frame <- renderUI({
    req(lta_report_path())
    tags$iframe(src = paste0(lta_report_res$prefix, "/lta_report_out.html?v=", as.integer(Sys.time())), width = "100%", height = "800px", style = "border: none;")
  })

  output$download_word_lta <- downloadHandler(
    filename = function() paste0("LTA_Report_", Sys.Date(), ".docx"),
    content = function(file) {
      req(selected_fit())
      withProgress(message = "Building the Word report", value = 0.4, {
        render_word_report("lta_report_word.Rmd", list(
          lta_res = selected_fit(), console_out = console_context$text, notes = report_notes(),
          ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text,
          dec = (input$dec_sep %||% ".")
        ), file, file.path(lta_report_res$path, "word"))
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )

  output$download_report_lta <- downloadHandler(
    filename = function() {
      paste0("LTA_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(lta_report_path())
      file.copy(lta_report_path(), file, overwrite = TRUE)
    }
  )
}
