# IRT -----
# ==== Reactive Data IRT ====
server_lta <- function(input, output, session, ai_context, console_context) {
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
    column(12,
           DT::datatable(comparison,extensions = 'Buttons',
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
               color = styleEqual(min_AIC, 'black'),
               backgroundColor = styleEqual(min_AIC, 'lightgreen'),
               fontWeight = styleEqual(min_AIC, 'bold')
             ) %>%
             formatStyle(
               columns = 'BIC',
               color = styleEqual(min_BIC, 'black'),
               backgroundColor = styleEqual(min_BIC, 'lightgreen'),
               fontWeight = styleEqual(min_BIC, 'bold')
             ) %>%
             formatStyle(
               columns = 'SABIC',
               color = styleEqual(min_SABIC, 'black'),
               backgroundColor = styleEqual(min_SABIC, 'lightgreen'),
               fontWeight = styleEqual(min_SABIC, 'bold')
             ) %>%
             formatStyle(
               columns = 'HQ',
               color = styleEqual(min_HQ, 'black'),
               backgroundColor = styleEqual(min_HQ, 'lightgreen'),
               fontWeight = styleEqual(min_HQ, 'bold')
             ) %>%
             # === Highlight untuk kolom N_itemfit (nilai terbesar) ===
             formatStyle(
               columns = 'N_itemfit',
               color = styleEqual(max_Nfit, 'black'),
               backgroundColor = styleEqual(max_Nfit, 'lightgreen'),
               fontWeight = styleEqual(max_Nfit, 'bold')
             )  %>% 
             formatStyle(
               columns = 'N_itemLD',
               color = styleEqual(minLD, 'black'),
               backgroundColor = styleEqual(minLD, 'lightgreen'),
               fontWeight = styleEqual(minLD, 'bold')
             ) %>% 
             formatStyle(
               columns = 'p (χ²)',
               backgroundColor = styleEqual(
                 c('p <.01', 'p <.05'),
                 c('lightgreen', 'lightgreen')
               )
             ),
           tags$div(
             style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
             tags$b("Interpretation Guidelines:"),
             tags$ul(
               tags$li(tags$span(style = "color: green;", "Best Model:"),"Smaller AIC/BIC/SABIC/HQ/N_ItemLD; Higher N_itemfit"),
               tags$li(tags$span(style = "color: red;", "ItemLD (Violation of Local Independency Assumption):"),  "|Q3| > 0.2 (Yen, 1984)"),
               tags$li(tags$span(style = "color: green;", "p.value <.05 / p.value <.01:"), 
                       "The more complex model fits better than the simpler model (Desjardin & Bulut, 2018)")
               
             ),
             tags$hr()
           )
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
    itemfit <- as.data.frame(mirt::itemfit(selected_fit(), fit_stats = input$fit_stats)) %>% 
      dplyr::select (paste0(input$fit_stats), paste0('p.',input$fit_stats)) %>% 
      dplyr::rename(`χ²_itemfit` = paste0(input$fit_stats), `p(χ²)_itemfit` = paste0('p.',input$fit_stats))
    
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
                      color = styleInterval(0.04999999, c('red', 'green'))
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
  
  output$select_icc <- renderUI({
    req(data_lta())
    choices <- c('SELECT ALL',names(data_lta()))
    div(class = "select-mini", 
        selectInput("item_select_icc", NULL, choices = choices, multiple = TRUE, selected = 'SELECT ALL', width = '100%'))
  }) 
  observeEvent(input$item_select_icc, {
    req(input$item_select_icc)
    selected <- input$item_select_icc
    # Jika "All_Item" dipilih bersama dengan yang lain, sisakan hanya "All_Item"
    if ("SELECT ALL" %in% selected && length(selected) > 1) {
      # Jika "All_Item" baru saja diklik, hapus item lain
      if (tail(selected, 1) == "SELECT ALL") {
        updateSelectInput(session, "item_select_icc", selected = "SELECT ALL")
      } else {
        # Jika item lain baru diklik saat All_Item aktif, hapus All_Item
        updateSelectInput(session, "item_select_icc", selected = setdiff(selected, "SELECT ALL"))
      }
    }
  })
  
  # === ICC Plot (reactive to itemtype) ===
  output$icc <- renderPlot({
    req(selected_fit())
    req(input$item_select_icc)
    
    all_names <- names(data_lta())         # urutan nama sesuai choices
    if ("SELECT ALL" %in% input$item_select_icc) {
      which_idx <- seq_along(all_names)
    } else {
      # ambil indeks dari nama yang dipilih (preserve urutan as in all_names)
      which_idx <- which(all_names %in% input$item_select_icc)
      # optional: jika pengguna memilih nama yang tidak ada (safety), bail out
      if (length(which_idx) == 0) return()
    }
    # plot mirt: gunakan which.items untuk menyebutkan indeks item
    mirt::plot(selected_fit(), type = "trace", which.items = which_idx,
               main = paste("ICC -", input$itemtype))
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
    
    tagList(
      column(
        6,
        DTOutput("fscore_table")
      ),
      
      column(
        6,
        br(),
        numericInput("n_kat", "Number of Factor Score Category:", value = 3, min = 2, max = 10),
        uiOutput("cutoff_inputs"),
        lapply(numeric_cols, function(i) {
          plotname <- paste0("fscore_donut_", i)
          div(
            style = "margin-bottom: 20px;",   # jarak antar-plot
            plotOutput(plotname, height = 280)
          )
        })
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
        scrollX = TRUE,
        pageLength = nrow(df),
        dom = "Brtp",
        buttons = list(
          list(
            extend = "csv",
            text = "Export CSV",
            filename = paste0("Factor Scores-", input$itemtype),
            exportOptions = list(modifier = list(page = "all"))
          ),
          list(
            extend = "excel",
            text = "Export Excel",
            filename = paste0("Factor Scores-", input$itemtype),
            exportOptions = list(modifier = list(page = "all"))
          )
        )
      ),
      rownames = TRUE
    ) %>%
      formatRound(columns = numeric_cols, digits = 2) %>%
      DT::formatStyle(
        columns = numeric_cols,
        fontWeight = "bold"      )
  }, server = FALSE)
  output$cutoff_inputs <- renderUI({
    req(input$n_kat)
    
    n_cut <- input$n_kat - 1
    
    tagList(
      lapply(1:n_cut, function(i) {
        
        div(
          style = "display: inline-block; margin-right: 10px;",
          numericInput(
            paste0("cut_", i),
            label = paste("Cut-off", i),
            value = NA,
            step = 0.01,
            width = "70px"   # small width
          )
        )
        
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
              legend_labels[i] <- paste0("θ ≤ ", round(high, 2))
            } else if (i == n_kat) {
              legend_labels[i] <- paste0("θ > ", round(low, 2))
            } else {
              legend_labels[i] <- paste0(round(low, 2), " < θ ≤ ", round(high, 2))
            }
          }
          
          # Warna kategori
          colors <- scales::hue_pal()(n_kat)
          
          # ====== PLOT DONUT ======
          ggplot(df_plot, aes(x = 2, y = freq, fill = kategori)) +
            
            # Donut bar
            geom_col(width = 1, color = "white") +
            
            # ===== LABEL DI LUAR DONUT =====
          geom_text(
            aes(
              x = 2.35,   # posisi label lebih keluar dari donut
              label = label
            ),
            position = position_stack(vjust = 0.5),
            size = 4,
            fontface = "bold"
          ) +
            
            coord_polar(theta = "y") +
            scale_fill_manual(
              values = colors,
              labels = legend_labels,
              name = "θ Range"
            ) +
            # ===== LABEL DI TENGAH DONUT =====
          annotate(
            "text",
            x = 0.5, 
            y = 0,
            label = paste0("F", which(numeric_cols == col_index), "-Score"),
            size = 6,
            fontface = "bold"
          ) +
            # Maintain the strcuture
            xlim(0.5, 2.5) +
            theme_void() +
            ggtitle(paste0("Factor Score : ", colnames(df)[col_index])) +
            theme(
              plot.title = element_text(hjust = 0.5),
              legend.position = "bottom"
            )
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
        color = styleInterval(0.05, c('red', 'black')),
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
    
    report_path <- file.path(system.file("app", package = "projectLSA"), "lta_report.Rmd")
    if (report_path == "" || !file.exists(report_path)) {
      report_path <- "lta_report.Rmd"
    }
    
    tempReport <- file.path(lta_report_res$path, "lta_report.Rmd")
    file.copy(report_path, tempReport, overwrite = TRUE)
    
    out_html <- file.path(lta_report_res$path, "lta_report_out.html")
    
    showModal(modalDialog("Generating Report Preview...", footer = NULL))
    tryCatch({
      rmarkdown::render(tempReport, output_file = out_html,
        params = list(
          lta_res = selected_fit(),
          console_out = console_context$text,
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
