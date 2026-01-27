server_cfa <- function(input, output, session) {
  library(lavaan)
  library(semPlot)
  library(readxl)
  library(psych)
  library(tibble)
  library(semptools)
  library(semTools)
  library(data.table)
  
  data_user <- reactive({
    if (input$data_source == "bfi") {
      df <- psych::bfi %>% dplyr::select(A1:O5) %>% tibble::rownames_to_column("id_auto")
    }
    if (input$data_source == "HolzingerSwineford1939") {
      df <- lavaan::HolzingerSwineford1939 %>% dplyr::select(x1:x9) %>% tibble::rownames_to_column("id_auto")
    }
    
    if (input$data_source == "upload") {
      req(input$datafile)
      ext <- tolower(tools::file_ext(input$datafile$name))
      showModal(modalDialog(title = NULL, "Reading Your File, Please wait...", footer = NULL, easyClose = FALSE))
      df <- switch(
        ext,
        "csv"  = data.table::fread(input$datafile$datapath,data.table = FALSE),
        "xls"  = readxl::read_excel(input$datafile$datapath),
        "xlsx" = readxl::read_excel(input$datafile$datapath),
        "sav"  = haven::read_sav(input$datafile$datapath),
        "rds"  = readRDS(input$datafile$datapath),
        stop("Unsupported file type. Please upload CSV, Excel, SPSS (.sav), or RDS file.")
      )
      removeModal()
      df <- df %>% mutate(across(everything(), ~ifelse(.x == "", NA, .x)),
                          id_auto = paste0("id_", sprintf("%04d", 1:n())))
    }
    return(df)
  })
  
  output$id_select_ui <- renderUI({
    req(data_user())
    selectizeInput("id_cols", "ID columns (optional)", choices = names(data_user()), multiple = TRUE,
                   selected = names(data_user())[grepl("id", names(data_user()), ignore.case = TRUE)])
  })
  
  output$var_select_ui <- renderUI({
    req(data_user())
    all_vars <- names(data_user())
    id_cols <- input$id_cols
    choices <- setdiff(all_vars, id_cols)
    selectInput("selected_vars", "Select Variables (used for CFA):", choices = choices, multiple = TRUE, selected = choices)
  })
  
  output$data_preview <- renderDT({
    req(data_user(), input$selected_vars)
    df <- data_user()[, input$selected_vars, drop = FALSE]
    # Identifikasi kolom numeric 
    numeric_cols <- which(sapply(df, is.numeric))
    #df <- df %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
    datatable(df, extensions = 'Buttons',
              options = list(scrollX = TRUE, dom = 'Brtp', pageLength = 25,
                             buttons = list(
                               list(
                                 extend = 'csv',
                                 text = 'Export CSV',
                                 filename = paste0('Data CFA')
                               ),
                               list(
                                 extend = 'excel',
                                 text = 'Export Excel',
                                 filename = paste0('Data CFA')
                               ))), rownames = T) %>% 
      formatRound(columns = numeric_cols, digits = 2)
  }, server = FALSE)
  
  
  # ===== CFA MODEL =====
  output$cfa_model_ui <- renderUI({
    model_text <- switch(input$data_source,
                         "bfi" = "A =~ A1 + A2 + A3 + A4 + A5\nC =~ C1 + C2 + C3 + C4 + C5\nE =~ E1 + E2 + E3 + E4 + E5\nN =~ N1 + N2 + N3 + N4 + N5\nO =~ O1 + O2 + O3 + O4 + O5",
                         "HolzingerSwineford1939" = "Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6\nSpeed =~ x7 + x8 + x9\nG =~ Visual+Textual+Speed",
                         "# Enter your lavaan model syntax here\n# Example:\n# factor1 =~ item1 + item2 + item3\n# factor2 =~ item4 + item5 + item6"
    )
    
    tags$textarea(id = "cfa_model_text", rows = 8, style = "width:100%; font-family: monospace; font-size:11.5px", model_text)
  })
  
  # ---- CFA storage ----
  cfa_fit_list <- reactiveVal(list())
  cfa_current_id <- reactiveVal(NULL)
  run_lavaan <- function(model_text, data, estimator = "ML", missing = "listwise") {
    tryCatch({
      data_numeric <- data %>% dplyr::select(where(is.numeric))
      
      if (estimator %in% c("DWLS", "WLSMV","MLR","GLS", "ULS" )) 
        {
        lavaan::cfa(model = model_text, data = data_numeric, estimator = estimator, ov.order = "model",
                    missing = missing)
      } else {
        if (missing == "fiml") {
          lavaan::cfa(model = model_text, data = data_numeric, estimator = estimator, ov.order = "model", missing = "fiml")
        } else if (missing == "pairwise") {
          lavaan::cfa(model = model_text, data = data_numeric, estimator = estimator, ov.order = "model", missing = "pairwise")
        } else {
          lavaan::cfa(model = model_text, data = data_numeric, estimator = estimator, ov.order = "model", missing = "listwise")
        }
      }
    }, error = function(e) {
      showNotification(paste("Model error:", e$message), type = "error")
      return(NULL)
    })
  }
  observeEvent(input$run_cfa, {
    req(data_user(), input$cfa_model_text, input$selected_vars)
    updateTabsetPanel(session, "main_tab_cfa", selected = "fit_tab_cfa")
  })
  # Main run CFA
  observeEvent(input$run_cfa, {
    req(data_user(), input$cfa_model_text, input$selected_vars)
    
    if (nchar(trimws(input$cfa_model_text)) == 0) {
      showNotification("Please enter a CFA model", type = "error")
      return()
    }
    
    df <- data_user()[, input$selected_vars, drop = FALSE]
    df <- df %>% mutate(across(everything(), as.numeric))
    
    showModal(modalDialog("Running CFA ...", footer = NULL))
    
    fit <- run_lavaan(
      model_text = input$cfa_model_text, 
      data = df, 
      estimator = input$cfa_estimator, 
      missing = input$cfa_missing
    )
    
    removeModal()
    
    if (is.null(fit)) return()
    
    measures <- tryCatch({
      lavaan::fitMeasures(fit, c("chisq","df","pvalue","rmsea","cfi","tli","srmr", "gfi", "agfi", "nfi", "nnfi"))
    }, error = function(e) {
      c(chisq = NA, df = NA, pvalue = NA, rmsea = NA, cfi = NA, tli = NA, srmr = NA, gfi = NA, agfi = NA, nfi = NA, nnfi = NA)
    })
    
    AVE <- tryCatch({
      semTools::AVE(object = fit)
      }, error = function(e) {
      '<NA>'
    })
    RelSEM <- tryCatch({
      semTools::compRelSEM(object = fit)
    }, error = function(e) {
      '<NA>'
    })
    HTMT <- tryCatch({
      semTools::htmt(model = input$cfa_model_text, data = df)
    }, error = function(e) {
      '<NA>'
    })
    
    scoreCfa <- tryCatch({
      lavaan::predict(fit)
    }, error = function(e) {
      '<NA>'
    })
    
    cur <- cfa_fit_list()
    new_index <- if (length(cur) == 0) 1 else (length(cur) + 1)
    new_id <- paste0("Model_", new_index)
    
    cur[[new_id]] <- list(
      id = new_id, 
      spec = input$cfa_model_text, 
      fit = fit, 
      measures = measures, 
      ave = AVE,
      relsem = RelSEM,
      htmt = HTMT,
      scoreCfa = as.data.frame(scoreCfa),
      time = Sys.time()
    )
    
    cfa_fit_list(cur)
    cfa_current_id(new_id)
    showNotification(paste0("CFA finished: ", new_id), type = "message")
  })
  
  # ====== Fit Comparison Table =======
  output$fit_comparison <- renderUI({
    req(cfa_fit_list())
    lst <- cfa_fit_list()
    if (length(lst) == 0) return(tags$p("No models run yet. Run CFA to see results."))
    df <- do.call(rbind, lapply(lst, function(x){
      m <- x$measures
      status_info <- get_fit_status(m["chisq"], m["df"], m["pvalue"], 
                                    m["rmsea"], m["cfi"], m["srmr"], m["gfi"])
      data.frame(
        Model = x$id,
        chisq = round(m["chisq"], 2),
        df = m["df"],
        chisq_df_ratio = ifelse(is.na(m["chisq"]), NA, sprintf("%.2f",m["chisq"]/m["df"])),
        pvalue = ifelse(is.na(m["pvalue"]), NA,
                         sprintf("%.3f", m["pvalue"])),
        RMSEA = ifelse(is.na(m["rmsea"]), NA, sprintf("%.3f", m["rmsea"])),
        CFI = ifelse(is.na(m["cfi"]), NA, sprintf("%.3f", m["cfi"])),
        TLI = ifelse(is.na(m["tli"]), NA, sprintf("%.3f", m["tli"])),
        GFI = ifelse(is.na(m["gfi"]), NA, sprintf("%.3f", m["gfi"])),
        SRMR = ifelse(is.na(m["srmr"]), NA, sprintf("%.3f", m["srmr"])),
        NFI = ifelse(is.na(m["nfi"]), NA, sprintf("%.3f", m["nfi"])),
        Status = status_info$status,
       
        StatusColor = status_info$color,  # tetap: warna background status
        stringsAsFactors = FALSE
      )
    }))
    # Buat tabel HTML manual
    column(12,
           tags$div(
             style = "margin-top: 0px; font-size: 13px;",
             tags$b("Fit Model Comparison")),
           # ---Render table DT =====
           datatable(df[,1:12],  # tampilkan kolom tanpa StatusColor
                     rownames = FALSE,
                     extensions = 'Buttons',
                     options = list(
                       dom = 'Brt',
                       buttons = list(
                         list(
                           extend = 'csv',
                           text = 'Export CSV',
                           filename = paste0('Fit Model Comparison')
                         ),
                         list(
                           extend = 'excel',
                           text = 'Export Excel',
                           filename = paste0('Fit Model Comparison')
                         )),
                       pageLength = 30,
                       columnDefs = list(list(className = 'dt-center', targets = 1:10))
                     )) %>%
             # --- Tetap: style kolom Status dengan warna ---
             formatStyle('chisq_df_ratio', color = styleInterval(c(2,5), c('green','orange','red')), fontWeight = 'bold', textAlign = 'center') %>% 
             # formatStyle('df', color = styleInterval(c(2,5), c('green','orange','red'))[df$chi_sq_ratio], fontWeight = 'bold', textAlign = 'center') %>% 
             formatStyle('pvalue', color = styleInterval(c(0.05,0.1), c('red','green','green')), fontWeight = 'bold', textAlign = 'center') %>%
             formatStyle('RMSEA', color = styleInterval(c(0.08,0.1), c('green','orange','red')), fontWeight = 'bold', textAlign = 'center') %>% 
             formatStyle('CFI', color = styleInterval(c(0.88,0.9), c('red','orange','green')), fontWeight = 'bold', textAlign = 'center') %>% 
             formatStyle('TLI', color = styleInterval(c(0.88,0.9), c('red','orange','green')), fontWeight = 'bold', textAlign = 'center') %>%
             formatStyle('GFI', color = styleInterval(c(0.88,0.9), c('red','orange','green')), fontWeight = 'bold', textAlign = 'center') %>% 
             formatStyle('SRMR', color = styleInterval(c(0.05,0.08), c('green','orange','red')), fontWeight = 'bold', textAlign = 'center') %>% 
             formatStyle('NFI', color = styleInterval(c(0.88,0.9), c('red','orange','green')), fontWeight = 'bold', textAlign = 'center') %>% 
             formatStyle('Status', backgroundColor = styleEqual(df$Status, df$StatusColor), fontWeight = 'bold', color = 'black', textAlign = 'center'),
           tags$div(
             style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
             tags$b("Interpretation Guidelines:"),
             tags$ul(
               tags$li(tags$span(style = "color: green;", "Good:"),"χ² < 2 df; pvalue ≥ 0.05; RMSEA < 0.08; CFI/TLI/NFI ≥ 0.9; SRMR ≤ 0.05;"),
               tags$li(tags$span(style = "color: orange;", "Acceptable:"), "2 df ≤ χ² < 5 df; RMSEA < 0.1; CFI/TLI/NFI ≥ 0.89; SRMR ≤ 0.08;"), 
               tags$li(tags$span(style = "color: red;", "Poor:"), "Outside acceptable ranges")
             )
           ),
           tags$hr(),
           column(6,
                  tags$div(
                    style = "margin-top: 0px; font-size: 13px;",
                    tags$b("Average Variance Explained (AVE):"),
                    DTOutput("ave_table")  # Table AVE
                  )), 
           column(6,
                  tags$div(
                    style = "margin-top: 1px; font-size: 13px;",
                    tags$b("Composite Reliability (CR):"),
                    DTOutput("reliability_table"),  # Table Reliability
                  )),
           tags$div(
             style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
             tags$b("Interpretation Guidelines:"),
             tags$ul(
               tags$li(tags$span(style = "color: green;", "Good:"),"AVE ≥ 0.5; CR ≥ 0.7"),
               tags$li(tags$span(style = "color: orange;", "Acceptable:"), "AVE ≥ 0.45; CR ≥ 0.6"), 
               tags$li(tags$span(style = "color: red;", "Poor:"), "Outside acceptable ranges")
             ),
             tags$p(tags$span(style = "color: blue;", "References:"), "Alamer (2025); Hu & Bentler (1999); Kline (2015); & Schumacker & Lomax (2016)")
           ),
         
           conditionalPanel(
             condition = "input.htmt_opt == true",
             tags$hr(),
             tags$div(
               style = "margin-top: 0px; font-size: 13px;",
               tags$b('Heterotrait–Monotrait Ratio (HTMT) for Current Model')), 
             DTOutput("htmt_table"),  
             tags$div(
               style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
               tags$b("Interpretation Guidelines:"),
               tags$ul(
                 tags$li(tags$span(style = "color: green;", "Good discriminant validity:"),"HTMT < 0.7 "),
                 tags$li(tags$span(style = "color: orange;", "Acceptable discriminant validity:"), "HTMT < 0.8"), 
                 tags$li(tags$span(style = "color: red;", "Potential discriminant validity issues:"), "HTMT ≥ 0.8")
               )
             )
           ),
           tags$hr(),
           column(12,
           tags$div(
             style = "margin-top: 0px; font-size: 13px;",
             tags$b('Modification Indices')),    
           DTOutput("mi_table")
           )
    )
    
  })
  
  # === Output: HTMT Table =====
  output$htmt_table <- renderDT({
    lst <- cfa_fit_list()
    if (length(lst) == 0) return(NULL)
    
    # Ambil model terakhir
    last_model <- lst[[length(lst)]]
    
    if (!is.null(last_model$htmt) && !identical(last_model$htmt, '<NA>')) {
      # Konversi HTMT matrix ke data frame
      htmt_matrix <- as.matrix(last_model$htmt)
      htmt_df <- as.data.frame(htmt_matrix)
      
      # Tambahkan nama baris sebagai kolom pertama
      htmt_df <- cbind(Variable = rownames(htmt_df), htmt_df)
      rownames(htmt_df) <- NULL
      
      # Identifikasi kolom numeric (HTMT values)
      numeric_cols <- which(sapply(htmt_df, is.numeric))
      
      datatable(htmt_df, rownames = FALSE, extensions = 'Buttons',
                options = list(
                  dom = 'Bt',
                  pageLength = nrow(htmt_df),
                  buttons = list(
                    list(
                      extend = 'csv',
                      text = 'Export CSV',
                      filename = paste0('HTMT')
                    ),
                    list(
                      extend = 'excel',
                      text = 'Export Excel',
                      filename = paste0('HTMT')
                    ))
                )) %>%
        formatStyle(columns = numeric_cols,
                    color = styleInterval(c(0.7, 0.8), c('green', 'orange', 'red'))) %>%
        formatStyle(columns = 'Variable',
                    backgroundColor = '#f8f9fa',
                    fontWeight = 'bold') %>%
        formatRound(columns = numeric_cols, digits = 3)
    } else {
      retu(data.frame(Message = "HTMT not available for the last model"))
    }
  })
 
  # ===== TABEL AVE =====
  output$ave_table <- renderDT({
    lst <- cfa_fit_list()
    if (length(lst) == 0) return(datatable(data.frame(Message = "No models available")))
    
    # Pastikan dplyr tersedia
    if (!require(dplyr, quietly = TRUE)) {
      stop("Package dplyr is required")
    }
    
    ave_data <- lapply(lst, function(x) {
      if (!is.null(x$ave) && !identical(x$ave, '<NA>')) {
        # Konversi AVE ke data frame
        ave_df <- as.data.frame(as.list(round(x$ave, 3)))
        ave_df$Model <- x$id
        return(ave_df)
      } else {
        data.frame(Model = x$id, Note = "AVE not available")
      }
    })
    
    # Gabungkan dengan full_join berantai
    combined_data <- Reduce(function(x, y) {
      dplyr::full_join(x, y, by = intersect(names(x), names(y)))
    }, ave_data)
    
    # Pastikan Model ada di kolom pertama
    if ("Model" %in% names(combined_data)) {
      model_col <- combined_data$Model
      combined_data$Model <- NULL
      combined_data <- data.frame(Model = model_col, combined_data)
    }
    
    datatable(combined_data, extensions = "Buttons",
              rownames = FALSE, 
              options = list(dom = 'Bt',
                             buttons = list(
                               list(
                                 extend = 'csv',
                                 text = 'Export CSV',
                                 filename = paste0('AVE')
                               ),
                               list(
                                 extend = 'excel',
                                 text = 'Export Excel',
                                 filename = paste0('AVE')
                               )))) %>%
      formatStyle(
        columns = which(!names(combined_data) %in% c("Model", "Note")),
        color = styleInterval(c(0.45, 0.5), c('red', 'orange', 'green'))
      ) %>%
      formatRound(columns = which(sapply(combined_data, is.numeric)), digits = 3)
  })

  # ===== Table  Reliability =====
  output$reliability_table <- renderDT({
    lst <- cfa_fit_list()
    if (length(lst) == 0) return(datatable(data.frame(Message = "No models available")))
    
    rel_data <- lapply(lst, function(x) {
      if (!is.null(x$relsem) && !identical(x$relsem, '<NA>')) {
        rel_df <- as.data.frame(as.list(round(x$relsem, 3)), stringsAsFactors = FALSE)
        rel_df$Model <- x$id
        return(rel_df)
      } else {
        data.frame(Model = x$id, Note = "Reliability not available", stringsAsFactors = FALSE)
      }
    })
    
    combined_data <- Reduce(function(x, y) {
      common_cols <- intersect(names(x), names(y))
      if (length(common_cols) > 0) {
        dplyr::full_join(x, y, by = common_cols)
      } else {
        # Jika tidak ada kolom common, gabungkan secara manual
        cbind(x, y[, setdiff(names(y), names(x)), drop = FALSE])
      }
    }, rel_data)
    
    # Pastikan Model ada di kolom pertama
    if ("Model" %in% names(combined_data)) {
      model_col <- combined_data$Model
      combined_data$Model <- NULL
      combined_data <- data.frame(Model = model_col, combined_data, stringsAsFactors = FALSE)
    }
    
    # Identifikasi kolom numeric (Reliability values)
    numeric_cols <- which(sapply(combined_data, is.numeric))
    
    datatable(combined_data, rownames = FALSE, extensions = 'Buttons',
              options = list(dom = 'Bt',
                             buttons = list(
                               list(
                                 extend = 'csv',
                                 text = 'Export CSV',
                                 filename = paste0('Composite Realiability')
                               ),
                               list(
                                 extend = 'excel',
                                 text = 'Export Excel',
                                 filename = paste0('Composite Reliability')
                               ))
                             )) %>%
      formatStyle(columns = numeric_cols,
                  color = styleInterval(c(0.6, 0.7), c('red', 'orange', 'green'))) %>%
      formatStyle(columns = 'Model',
                  backgroundColor = '#f8f9fa') %>%
      formatRound(columns = numeric_cols, digits = 3)
  })
  
  # ===== Table  FSCORES =====
  output$fscores_cfa <- renderDT({
    req(input$run_cfa)
    lst <- cfa_fit_list()
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) return(NULL)
    scoreCfa <- cfa_fit_list()[[cur_id]]$scoreCfa
    numeric_cols <- which(sapply(scoreCfa, is.numeric))
    datatable(scoreCfa, rownames = TRUE, extensions = 'Buttons',
              options = list(dom = 'Brtp',
                             buttons = list(
                               list(
                                 extend = 'csv',
                                 text = 'Export CSV',
                                 filename = paste0('Factor Scores')
                               ),
                               list(
                                 extend = 'excel',
                                 text = 'Export Excel',
                                 filename = paste0('Factor Scores')
                               )))) %>%
      formatRound(columns = numeric_cols, digits = 3)
  }, server = FALSE)
  
  # Helper function untuk menentukan status fit ======
  get_fit_status <- function(chisq,df, pvalue, rmsea, cfi, srmr,gfi) {
    if (is.na(chisq)||is.na(rmsea) || is.na(cfi) || is.na(srmr)|| is.na(gfi)) {
      return(list(status = "Unknown", color = "#e2e3e5"))
    }
    if (chisq/df<2 && rmsea < 0.08 && cfi >= 0.9 && gfi >= 0.9 && srmr <= 0.05) {
      return(list(status = "Excellent", color = 'lightgreen'))
    }  else if (pvalue >=0.05 && rmsea < 0.08 && cfi >= 0.9 && gfi >= 0.9 && srmr <= 0.05) {
        return(list(status = "Excellent", color = 'lightgreen'))
    } else if (pvalue < 0.05 && rmsea < 0.08 && cfi >= 0.9 && gfi >= 0.9 && srmr <= 0.05) {
      return(list(status = "Good", color = "#d4edda"))
    } else if  (
      sum(c(chisq/df<2, rmsea < 0.08,cfi >= 0.9, gfi >= 0.9,srmr <= 0.05,pvalue >= 0.05)) >= 3
    ) {
      return(list(status = "Acceptable", color = "#fff3cd"))
    } else {
      return(list(status = "Poor", color = "#f8d7da"))
    }
  }
  
  
  # Loadings table
  output$loadings_table <- renderDT({
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) return(NULL)
    
    fit <- cfa_fit_list()[[cur_id]]$fit
    
    if (input$cfa_std_est) {
      sol <- tryCatch(lavaan::standardizedSolution(fit), error = function(e) NULL)
    } else {
      sol <- tryCatch(lavaan::parameterEstimates(fit), error = function(e) NULL)
    }
    
    if (is.null(sol)) return(datatable(data.frame(Message = "No parameter estimates available")))
    
    loadings <- sol[sol$op == "=~", ]
    
    if (nrow(loadings) == 0) return(datatable(data.frame(Message = "No factor loadings found")))
    
    if (input$cfa_std_est) {
      display_df <- loadings[, c("lhs","rhs","est.std","pvalue")]
      colnames(display_df) <- c("Factor","Item","StdLoading","pvalue")
      display_df$StdLoading <- round(display_df$StdLoading, 3)
    } else {
      display_df <- loadings[, c("lhs","rhs","est","se","pvalue")]
      colnames(display_df) <- c("Factor","Item","Estimate","SE","pvalue")
      display_df$Estimate <- round(display_df$Estimate, 3)
      display_df$SE <- round(display_df$SE, 3)
    }
    
    display_df$pvalue <- format.pval(display_df$pvalue, digits = 3)
    
    datatable(display_df, extensions = 'Buttons',
              options = list(pageLength = 15, scrollX = TRUE,dom = 'Brtp',
                             buttons = list(
                               list(
                                 extend = 'csv',
                                 text = 'Export CSV',
                                 filename = paste0('Factor Loading')
                               ),
                               list(
                                 extend = 'excel',
                                 text = 'Export Excel',
                                 filename = paste0('Factor Loading')
                               )))) %>% 
      formatStyle(columns = 'StdLoading',
                  fontWeight = 'bold',
                  color = styleInterval(c(0.3, 0.5, 0.7), c('red', 'orange', 'blue', 'green')) )
  }, server = FALSE)
  
  # Modification indices
  mi_df <- reactive({
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) return(NULL)
    
    fit <- cfa_fit_list()[[cur_id]]$fit
    mi <- tryCatch(lavaan::modindices(fit, sort. = TRUE, minimum.value = 1), error = function(e) NULL)
    
    if (is.null(mi) || nrow(mi) == 0) return(NULL)
    
    if (nrow(mi) > 20) mi <- mi[1:20, ]
    
    mi
  })
  
  output$mi_table <- renderDT({
    mi <- mi_df()
    #mi <- mi %>% dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
    if (is.null(mi)) return(datatable(data.frame(Message = "No modification indices available")))
    
    mi$Action <- sapply(1:nrow(mi), function(i) {
      as.character(
        actionButton(
          class = "btn-primary sm",
          style = "font-size: 11px !important; padding: 2px 10px !important;",  
          inputId = paste0("mi_btn_", i),
          label = "Use This",
          onclick = paste0('Shiny.setInputValue("mi_selected", "', i, '", {priority: "event"})')
        )
      )
    })
    
    display_mi <- mi[, c("lhs", "op", "rhs", "mi", "epc", "Action")]
    colnames(display_mi) <- c("From", "Operator", "To", "MI", "EPC", "Action")
    
    datatable(display_mi, rownames = FALSE, escape = FALSE, 
              options = list(pageLength = 10, scrollX = TRUE,dom = 'rtp',
    # Atur ukuran font dan styling
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'font-size': '5px', 'font-weight': 'bold'});",
      "$(this.api().table().body()).css({'font-size': '5px', 'line-height': '1'});",
      "}"
    )))    %>% 
      formatRound(columns = which(sapply(display_mi, is.numeric)), digits = 3)
  }, server = FALSE)
  
  # Handle MI button clicks
  observeEvent(input$mi_selected, {
    req(input$mi_selected)
    
    mi <- mi_df()
    if (is.null(mi)) return()
    
    idx <- as.numeric(input$mi_selected)
    if (idx > nrow(mi)) return()
    
    row <- mi[idx, ]
    
    if (row$op == "=~") {
      new_line <- paste(row$lhs, "=~", row$rhs)
    } else if (row$op == "~~") {
      new_line <- paste(row$lhs, "~~", row$rhs)
    } else if (row$op == "~") {
      new_line <- paste(row$lhs, "~", row$rhs)
    } else {
      new_line <- paste(row$lhs, row$op, row$rhs)
    }
    
    current_model <- input$cfa_model_text
    updated_model <- paste(current_model, "\n", new_line)
    
    updateTextAreaInput(session, "cfa_model_text", value = updated_model)
    
    showNotification(paste("Added MI suggestion to model:", new_line), type = "message")
  })
  
  # Update fungsi get_color_scheme
  get_color_scheme <- function(scheme_name, custom_man = NULL, custom_lat = NULL) {
    if (scheme_name == "Custom" && !is.null(custom_man) && !is.null(custom_lat)) {
      return(list(man = custom_man, lat = custom_lat))
    }
    
    switch(scheme_name,
           "Blue-Yellow" = list(
             man = "#A1E3F9",
             lat = c("#FFFFBA")
           ),
           "Ocean" = list(
             man = "#B3E5FC",
             lat = c("#81D4FA", "#4FC3F7", "#29B6F6")
           ),
           "Forest" = list(
             man = "#C8E6C9",
             lat = c("#A5D6A7", "#81C784", "#4CAF50")
           ),
           "Rainbow" = list(
             man = "#B3E5FC", 
             lat = c("#FF8A65", "#81D4FA", "#AED581", "#CE93D8","#FFF176" )
           )
    )
  }
  
  
  # Get factor names safely
  get_factor_names <- function(fit_obj) {
    tryCatch({
      if (inherits(fit_obj, "lavaan")) {
        params <- lavaan::parameterEstimates(fit_obj)
        factors <- unique(params$lhs[params$op == "=~"])
        if (length(factors) > 0) return(factors)
        
        model_text <- fit_obj@Model@model.syntax
        if (is.character(model_text)) {
          lines <- strsplit(model_text, "\n")[[1]]
          factor_lines <- grep("=~", lines, value = TRUE)
          factors <- sapply(strsplit(factor_lines, "=~"), function(x) trimws(x[1]))
          return(factors)
        }
      }
      return("Unknown")
    }, error = function(e) {
      return("Error getting factors")
    })
  }
  
  # SIMPLE PLOT RENDER - langsung di output$path_plot
  output$path_plot <- renderPlot({
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) {
      plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE, 
           main = "No CFA model available")
      text(1, 1, "Please run a CFA model first\n\n1. Select 'Built-in: HolzingerSwineford1939'\n2. Keep the example model\n3. Click 'Run CFA'", 
           cex = 1.2, col = "darkred")
      return()
    }
    
    fit_obj <- cfa_fit_list()[[cur_id]]$fit
    
    tryCatch({
      # Get colors
      tryCatch({
        # Get colors
        if (input$plot_color_scheme == "Custom") {
          colors <- list(man = input$mancolour, lat = input$latcolour)
        } else {
          colors <- get_color_scheme(input$plot_color_scheme)
        }
        
        # Create plot...
      }, error = function(e) {
        # Error handling...
      })      
      # Create plot langsung di sini
      A <- semPlot::semPaths(
        fit_obj,
        what = ifelse(input$plot_standardized, "std", "est"),
        style = input$plot_style,
        layout = input$plot_layout,
        residuals = input$plot_residuals,
        exoCov = input$plot_exoCov,
        edge.label.cex = input$plot_edge_label_size,
        nCharNodes = 6,
        nCharEdges = 6,
        sizeLat = input$plot_nodesize_lat,
        sizeMan = input$plot_nodesize_man,
        sizeMan2 = input$plot_nodesize_man2,
        rotation = input$plot_rotation,
        color = colors,
        #border.width = 1,
        label.color = "black",
        mar = c( 1, 15-input$plotwidth,20-input$plotheight,15-input$plotwidth),
        bifactor = input$bifactor,
        esize = input$edgewidth,
        edge.color = "black",       
        freeStyle = c(1, "black"),
        fixedStyle = c(2, "black"),
        fade = FALSE

      )

      plot(semptools::mark_sig(semPaths_plot = A,object = fit_obj,
                               alphas = c("*" = 0.05, "**" = 0.01, "***" = 0.001)))    
      
    }, error = function(e) {
      plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE, 
           main = "Plot Error")
      text(1, 1, paste("Error creating plot:\n", e$message), 
           cex = 1.2, col = "red")
    })
  })
  
  # Fit information below plot
  output$plot_fit_info <- renderUI({
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) return(NULL)
    
    obj <- cfa_fit_list()[[cur_id]]
    m <- obj$measures
    fit_obj <- obj$fit
    
    # Get factor names safely
    factor_names <- get_factor_names(fit_obj)
    
    fit_text <- paste0(
      "χ² = ", round(m["chisq"], 2), "; ",
      "df = ", m["df"], "; ",
      "p = ", ifelse(is.na(m["pvalue"]), "NA", format.pval(m["pvalue"], digits = 3)), "; ",
      "RMSEA = ", ifelse(is.na(m["rmsea"]), "NA", sprintf("%.3f", m["rmsea"])), "; ",
      "CFI = ", ifelse(is.na(m["cfi"]), "NA", sprintf("%.3f", m["cfi"])), "; ",
      "GFI = ", ifelse(is.na(m["gfi"]), "NA", sprintf("%.3f", m["gfi"])), "; ",
      "SRMR = ", ifelse(is.na(m["srmr"]), "NA", sprintf("%.3f", m["srmr"])),"; ",
      "TLI = ", ifelse(is.na(m["tli"]), "NA", sprintf("%.3f", m["tli"])),"; ",
      "NFI = ", ifelse(is.na(m["nfi"]), "NA", sprintf("%.3f", m["nfi"]))
      
      
    )
    
    estimator_text <- paste0(
      "Estimator = ", input$cfa_estimator, 
      " | Factors: ", paste(factor_names, collapse = ", ")
      
    )
    sig_sign <- paste0(
      '*** : p ≤ 0.001", "**  : p ≤ 0.01", "*    : p ≤ 0.05'
    )
    
    tagList(
      tags$div(style = "margin-top: 0px; padding: 5px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
               tags$p(style = "margin: 0; font-size: 11px; color: #2c3e50;", 
                      tags$b("Fit indices: "), fit_text),
               tags$p(style = "margin: 0; font-size: 11px; color: darkblue;", 
                      estimator_text),
               tags$p(style = "margin: 0; font-size: 10px; color: #6c757d;", 
                      sig_sign)
      )
    )
  })
  
  # Download handlers
  output$download_measures <- downloadHandler(
    filename = function() paste0("cfa_measures_", Sys.Date(), ".csv"),
    content = function(file) {
      lst <- cfa_fit_list()
      if (length(lst) == 0) return(NULL)
      
      df_list <- lapply(lst, function(x) {
        data.frame(Model = x$id, as.list(x$measures), stringsAsFactors = FALSE)
      })
      
      df <- do.call(rbind, df_list)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_loadings <- downloadHandler(
    filename = function() paste0("cfa_loadings_", Sys.Date(), ".csv"),
    content = function(file) {
      cur_id <- cfa_current_id()
      if (is.null(cur_id)) return(NULL)
      
      fit <- cfa_fit_list()[[cur_id]]$fit
      
      if (input$cfa_std_est) {
        sol <- lavaan::standardizedSolution(fit)
      } else {
        sol <- lavaan::parameterEstimates(fit)
      }
      
      write.csv(sol, file, row.names = FALSE)
    }
  )
  
  output$download_lavaan <- downloadHandler(
    filename = function() paste0("cfa_summary_", Sys.Date(), ".txt"),
    content = function(file) {
      cur_id <- cfa_current_id()
      if (is.null(cur_id)) return(NULL)
      
      fit <- cfa_fit_list()[[cur_id]]$fit
      
      sink(file)
      print(summary(fit, fit.measures = TRUE, standardized = TRUE))
      sink()
    }
  )
  
  output$download_scores_cfa <- downloadHandler(
    filename = function() paste0("Factor Scores_", Sys.Date(), ".csv"),
    content = function(file) {
      cur_id <- cfa_current_id()
      if (is.null(cur_id)) return(NULL)
      
      scoreCfa <- cfa_fit_list()[[cur_id]]$scoreCfa
      
      write.csv(scoreCfa, file, row.names = FALSE)
    }
  )
  

}
