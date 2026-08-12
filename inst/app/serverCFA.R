# ==== HELPER FUNCTIONS (GLOBAL IN SERVER) ====
# CR AVE & HTMT ====
calc_ave_cr <- function(fit) {
  std <- lavaan::standardizedSolution(fit)

  loading <- std[std$op == "=~", c("lhs", "rhs", "est.std")]
  loading$err <- 1 - loading$est.std^2

  result <- lapply(split(loading, loading$lhs), function(x) {
    lambda2 <- x$est.std^2
    theta <- x$err

    AVE <- sum(lambda2) / (sum(lambda2) + sum(theta))
    CR <- (sum(x$est.std))^2 /
      ((sum(x$est.std))^2 + sum(theta))

    c(AVE = AVE, CR = CR)
  })

  as.data.frame(do.call(rbind, result))
}
calc_htmt <- function(data, model_syntax,
                      missing = "listwise",
                      ordered = NULL,
                      absolute = TRUE) {
  ## -------------------------------------------------
  ## 1. Parse lavaan model syntax
  ## -------------------------------------------------
  lines <- unlist(strsplit(model_syntax, "\n"))
  fac_lines <- lines[grepl("=~", lines)]

  blocks <- lapply(fac_lines, function(x) {
    trimws(unlist(strsplit(strsplit(x, "=~")[[1]][2], "\\+")))
  })
  names(blocks) <- trimws(sub("=~.*", "", fac_lines))

  # Merge items for factors defined across multiple lines (duplicate factor names)
  unique_factors <- unique(names(blocks))
  merged_blocks <- lapply(unique_factors, function(f) {
    unique(unlist(blocks[names(blocks) == f]))
  })
  names(merged_blocks) <- unique_factors
  blocks <- merged_blocks

  ## -------------------------------------------------
  ## 2. Lavcor
  ## -------------------------------------------------
  R <- lavaan::lavCor(
    data,
    missing = missing,
    ordered = ordered,
    output  = "cor"
  )

  if (absolute) R <- abs(R)

  ## -------------------------------------------------
  ## 3. Indicator check
  ## -------------------------------------------------
  blocks <- lapply(blocks, function(x) intersect(x, colnames(R)))

  ## -------------------------------------------------
  ## 4. calculate HTMT2 (GEOMETRIC MEAN)
  ## -------------------------------------------------
  k <- length(blocks)
  htmt <- matrix(NA, k, k, dimnames = list(names(blocks), names(blocks)))

  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (i == j) {
        htmt[i, j] <- 1
        next
      }

      Xi <- blocks[[i]]
      Xj <- blocks[[j]]

      # heterotrait correlations
      r_het <- R[Xi, Xj]

      # monotrait correlations
      r_mono_i <- R[Xi, Xi][lower.tri(R[Xi, Xi])]
      r_mono_j <- R[Xj, Xj][lower.tri(R[Xj, Xj])]

      htmt[i, j] <-
        exp(mean(log(r_het))) /
          sqrt(
            exp(mean(log(r_mono_i))) *
              exp(mean(log(r_mono_j)))
          )
    }
  }

  htmt
}
# ====END of CR AVE and HTMT ====
server_cfa <- function(input, output, session, ai_context, console_context) {
  library(lavaan)
  library(semPlot)
  library(readxl)
  library(psych)
  library(tibble)
  library(semptools)
  # library(semTools)
  library(data.table)
  library(kableExtra)

  raw_data_user <- reactive({
    req(input$data_source)
    if (input$data_source == "bfi") {
      df <- psych::bfi %>%
        dplyr::select(A1:O5) %>%
        tibble::rownames_to_column("id_auto")
    }
    if (input$data_source == "HolzingerSwineford1939") {
      df <- lavaan::HolzingerSwineford1939 %>%
        dplyr::select(x1:x9, school) %>%
        tibble::rownames_to_column("id_auto")
    }
    if (input$data_source == "PoliticalDemocracy") {
      df <- lavaan::PoliticalDemocracy %>%
        dplyr::select(y1:y8, x1:x3) %>%
        tibble::rownames_to_column("id_auto")
    }
    if (input$data_source == "Demo.growth") {
      df <- lavaan::Demo.growth %>%
        dplyr::select(t1, t2, t3, t4, x1, x2, c1, c2) %>%
        tibble::rownames_to_column("id_auto")
    }

    if (input$data_source == "upload") {
      req(input$datafile)
      ext <- tolower(tools::file_ext(input$datafile$name))
      showModal(modalDialog(title = NULL, "Reading Your File, Please wait...", footer = NULL, easyClose = FALSE))

      if (ext == "rds") {
        res <- readRDS(input$datafile$datapath)
        if (is.list(res) && !is.data.frame(res) && identical(res$type, "projectLSA_workspace")) {
          if (identical(res$module, "CFA")) {
            # Restore Workspace State
            cfa_aggregations(res$cfa_aggregations)
            cfa_fit_list(res$cfa_fit_list)
            cfa_current_id(res$cfa_current_id)
            df <- res$raw_data
            
            # Restore UI Inputs
            if (!is.null(res$input_state)) {
              updateSelectInput(session, "cfa_estimator", selected = res$input_state$cfa_estimator)
              updateSelectInput(session, "cfa_missing", selected = res$input_state$cfa_missing)
              updateCheckboxInput(session, "cfa_std_est", value = res$input_state$cfa_std_est)
              updateCheckboxInput(session, "htmt_opt", value = res$input_state$htmt_opt)
              updateSelectInput(session, "cfa_analysis_mode", selected = res$input_state$cfa_analysis_mode)
              updateSelectInput(session, "mgcfa_invariance", selected = res$input_state$mgcfa_invariance)
              updateSelectInput(session, "mgcfa_group", selected = res$input_state$mgcfa_group)
              updateTextAreaInput(session, "cfa_model_text", value = res$input_state$cfa_model_text)
              updateSelectInput(session, "cfa_summary_theme", selected = res$input_state$cfa_summary_theme)
              updateSelectInput(session, "plot_model_scope", selected = res$input_state$plot_model_scope)
              updateSelectInput(session, "plot_style", selected = res$input_state$plot_style)
              updateSelectInput(session, "plot_layout", selected = res$input_state$plot_layout)
              updateSelectInput(session, "plot_color_scheme", selected = res$input_state$plot_color_scheme)
              updateCheckboxInput(session, "plot_standardized", value = res$input_state$plot_standardized)
              shinyWidgets::updatePickerInput(session, "fit_indices_selected", selected = res$input_state$fit_indices_selected)
              if (!is.null(res$input_state$cfa_agg_syntax)) updateTextAreaInput(session, "cfa_agg_syntax", value = res$input_state$cfa_agg_syntax)
              if (!is.null(res$input_state$cfa_agg_method)) updateSelectInput(session, "cfa_agg_method", selected = res$input_state$cfa_agg_method)
            }
            
            showNotification("CFA Workspace restored successfully!", type = "message")
          } else {
            stop("Uploaded workspace belongs to a different module: ", res$module)
          }
        } else {
          df <- res
        }
      } else {
        df <- switch(ext,
          "csv"  = data.table::fread(input$datafile$datapath, data.table = FALSE),
          "xls"  = readxl::read_excel(input$datafile$datapath),
          "xlsx" = readxl::read_excel(input$datafile$datapath),
          "sav"  = haven::read_sav(input$datafile$datapath),
          stop("Unsupported file type. Please upload CSV, Excel, SPSS (.sav), or RDS file.")
        )
      }
      removeModal()

      # Process df (only if it doesn't already have id_auto, e.g. newly uploaded raw data)
      if (!"id_auto" %in% names(df)) {
        df <- df %>% mutate(across(everything(), ~ ifelse(.x == "", NA, .x)),
          id_auto = paste0("id_", sprintf("%04d", 1:n()))
        )
      }
    }
    return(df)
  })

  # Reactive value to store user-defined aggregations
  cfa_aggregations <- reactiveVal(list())

  # New data_user reactive that applies aggregations on top of raw_data_user
  data_user <- reactive({
    df <- raw_data_user()
    req(df)

    aggs <- cfa_aggregations()
    if (length(aggs) > 0) {
      for (agg in aggs) {
        vars <- agg$vars
        name <- agg$name
        method <- agg$method

        valid_vars <- intersect(vars, names(df))
        if (length(valid_vars) > 0) {
          if (method == "mean") {
            df[[name]] <- rowMeans(df[, valid_vars, drop = FALSE], na.rm = TRUE)
          } else if (method == "sum") {
            df[[name]] <- rowSums(df[, valid_vars, drop = FALSE], na.rm = TRUE)
          }
        }
      }
    }
    return(df)
  })

  # Handle aggregation button click
  observeEvent(input$cfa_btn_aggregate, {
    syntax <- input$cfa_agg_syntax
    req(syntax)
    lines <- strsplit(syntax, "\n")[[1]]
    
    current_aggs <- cfa_aggregations()
    count <- 0
    for (line in lines) {
      if (grepl("=", line)) {
        parts <- strsplit(line, "=")[[1]]
        if (length(parts) == 2) {
          name <- make.names(trimws(parts[1]))
          vars_raw <- strsplit(parts[2], "[, ]+")[[1]]
          vars <- trimws(vars_raw[vars_raw != ""])
          
          if (length(vars) > 0 && name != "") {
            new_agg <- list(
              vars = vars,
              name = name,
              method = input$cfa_agg_method
            )
            current_aggs[[name]] <- new_agg
            count <- count + 1
          }
        }
      }
    }
    
    cfa_aggregations(current_aggs)
    if (count > 0) {
      showNotification(paste(count, "variables calculated using", input$cfa_agg_method, "method."), type = "message")
      shinyWidgets::updatePickerInput(session, "cfa_agg_vars_insert", selected = character(0))
    }
  })

  # Handle insert button
  observeEvent(input$cfa_btn_insert_agg, {
    vars <- input$cfa_agg_vars_insert
    if (length(vars) > 0) {
      current <- input$cfa_agg_syntax
      prefix <- ifelse(!is.null(current) && nchar(current) > 0 && !endsWith(current, "\n"), "\n", "")
      new_text <- paste0(current, prefix, "NewVar = ", paste(vars, collapse = ", "))
      updateTextAreaInput(session, "cfa_agg_syntax", value = new_text)
    }
  })

  # Handle reset aggregation button
  observeEvent(input$cfa_btn_reset_agg, {
    cfa_aggregations(list())
    showNotification("All aggregations have been reset.", type = "warning")
  })

  # Populate aggregation UI choices
  observe({
    req(raw_data_user())
    # We populate with raw data columns + existing aggregated columns
    df_names <- names(data_user())
    shinyWidgets::updatePickerInput(session, "cfa_agg_vars_insert", choices = df_names)
    updateSelectizeInput(session, "cfa_summary_vars", choices = df_names)
  })

  output$id_select_ui <- renderUI({
    req(data_user())
    shinyWidgets::pickerInput("id_cols", "ID columns (optional)",
      choices = names(data_user()), multiple = TRUE,
      selected = names(data_user())[grepl("id", names(data_user()), ignore.case = TRUE)],
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  output$var_select_ui <- renderUI({
    req(data_user())
    all_vars <- names(data_user())
    id_cols <- input$id_cols
    choices <- setdiff(all_vars, id_cols)
    shinyWidgets::pickerInput("selected_vars", "Select Variables (used for CFA):", 
                              choices = choices, multiple = TRUE, selected = choices,
                              options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 3"))
  })

  output$mgcfa_group_ui <- renderUI({
    req(data_user())
    all_vars <- names(data_user())
    # Try to find categorical columns
    cat_vars <- all_vars[sapply(data_user(), function(x) is.factor(x) || is.character(x) || length(unique(x)) <= 5)]
    selectInput("mgcfa_group_var", "Grouping Variable:", choices = cat_vars)
  })

  output$data_preview <- renderDT(
    {
      req(data_user(), input$selected_vars)
      df <- data_user()[, input$selected_vars, drop = FALSE]
      # Identifikasi kolom numeric
      numeric_cols <- which(sapply(df, is.numeric))
      # df <- df %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
      datatable(df,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE, dom = "Brtp", pageLength = 25,
          buttons = list(
            list(
              extend = "csv",
              text = "Export CSV",
              filename = paste0("Data CFA")
            ),
            list(
              extend = "excel",
              text = "Export Excel",
              filename = paste0("Data CFA")
            )
          )
        ), rownames = T
      ) %>%
        formatRound(columns = numeric_cols, digits = 2)
    },
    server = FALSE
  )


  # ===== CFA MODEL =====
  output$cfa_model_ui <- renderUI({
    model_text <- switch(input$data_source,
      "bfi" = "A =~ A1 + A2 + A3 + A4 + A5\nC =~ C1 + C2 + C3 + C4 + C5\nE =~ E1 + E2 + E3 + E4 + E5\nN =~ N1 + N2 + N3 + N4 + N5\nO =~ O1 + O2 + O3 + O4 + O5",
      "HolzingerSwineford1939" = "Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6\nSpeed =~ x7 + x8 + x9\nG =~ Visual+Textual+Speed",
      "PoliticalDemocracy" = "# Measurement model\nind60 =~ x1 + x2 + x3\ndem60 =~ y1 + y2 + y3 + y4\ndem65 =~ y5 + y6 + y7 + y8\n\n# Structural model\ndem60 ~ ind60\ndem65 ~ ind60 + dem60\n\n# Residual correlations\ny1 ~~ y5\ny2 ~~ y4 + y6\ny3 ~~ y7\ny4 ~~ y8\ny6 ~~ y8",
      "Demo.growth" = "# Intercept and slope with fixed coefficients\ni =~ 1*t1 + 1*t2 + 1*t3 + 1*t4\ns =~ 0*t1 + 1*t2 + 2*t3 + 3*t4\n\n# Regressions (optional predictors)\n# i ~ x1 + x2\n# s ~ x1 + x2",
      "# Enter your lavaan model syntax here\n# Example:\n# factor1 =~ item1 + item2 + item3\n# factor2 =~ item4 + item5 + item6"
    )

    tags$textarea(id = "cfa_model_text", rows = 8, style = "width:100%; font-family: monospace; font-size:11.5px", model_text)
  })

  # ---- CFA storage ----
  cfa_fit_list <- reactiveVal(list())
  cfa_current_id <- reactiveVal(NULL)
  cfa_model_counter <- reactiveVal(0)
  run_lavaan <- function(model_text, data, estimator = "ML", missing = "listwise",
                         mode = "standard", group = NULL, group_equal = NULL) {
    tryCatch(
      {
        # Enforce group variable inclusion if MGCFA
        if (mode == "mgcfa" && !is.null(group) && group != "") {
          data_to_use <- data %>% dplyr::select(where(is.numeric) | all_of(group))
        } else {
          data_to_use <- data %>% dplyr::select(where(is.numeric))
        }

        # Determine function
        lavaan_fun <- if (mode == "lgm") lavaan::growth else lavaan::sem

        # Additional args
        args <- list(
          model = model_text,
          data = data_to_use,
          estimator = estimator,
          ov.order = "model",
          missing = if (missing == "fiml") "fiml" else if (missing == "pairwise") "pairwise" else "listwise"
        )

        if (mode == "mgcfa" && !is.null(group) && group != "") {
          args$group <- group
          if (!is.null(group_equal) && group_equal != "configural") {
            # group_equal comes as a string like "loadings" or "loadings, intercepts"
            args$group.equal <- strsplit(group_equal, ", ")[[1]]
          }
        }

        do.call(lavaan_fun, args)
      },
      error = function(e) {
        showNotification(paste("Model error:", e$message), type = "error")
        return(NULL)
      }
    )
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

    df_subset <- data_user()[, input$selected_vars, drop = FALSE]

    # If MGCFA, we need the group column too
    if (input$cfa_analysis_mode == "mgcfa" && !is.null(input$mgcfa_group_var)) {
      df <- cbind(df_subset, data_user()[, input$mgcfa_group_var, drop = FALSE])
    } else {
      df <- df_subset
    }

    # Only items should be forced to numeric
    # df_subset <- df_subset %>% mutate(across(everything(), as.numeric))


    showModal(modalDialog("Running CFA ...", footer = NULL))

    fit <- run_lavaan(
      model_text = input$cfa_model_text,
      data = df,
      estimator = input$cfa_estimator,
      missing = input$cfa_missing,
      mode = input$cfa_analysis_mode,
      group = if (input$cfa_analysis_mode == "mgcfa") input$mgcfa_group_var else NULL,
      group_equal = if (input$cfa_analysis_mode == "mgcfa") input$mgcfa_invariance else NULL
    )

    removeModal()

    if (is.null(fit)) {
      return()
    }

    measures <- tryCatch(
      {
        fm <- lavaan::fitMeasures(fit)
        
        use_robust <- is.null(input$cfa_use_robust_fit) || input$cfa_use_robust_fit
        has_scaled <- "chisq.scaled" %in% names(fm)
        
        get_val <- function(std, scaled, robust) {
          if (use_robust) {
            if (!missing(robust) && robust %in% names(fm) && !is.na(fm[robust])) {
              return(unname(fm[robust]))
            }
            if (!missing(scaled) && scaled %in% names(fm) && !is.na(fm[scaled])) {
              return(unname(fm[scaled]))
            }
          }
          if (std %in% names(fm)) {
            return(unname(fm[std]))
          }
          return(NA)
        }

        c(
          chisq = get_val("chisq", "chisq.scaled"),
          df = get_val("df", "df.scaled"),
          pvalue = get_val("pvalue", "pvalue.scaled"),
          rmsea = get_val("rmsea", "rmsea.scaled", "rmsea.robust"),
          cfi = get_val("cfi", "cfi.scaled", "cfi.robust"),
          tli = get_val("tli", "tli.scaled", "tli.robust"),
          srmr = get_val("srmr", "srmr_nomean"),
          gfi = get_val("gfi"),
          agfi = get_val("agfi"),
          nfi = get_val("nfi", "nfi.scaled"),
          nnfi = get_val("nnfi", "nnfi.scaled", "nnfi.robust"),
          is_robust = as.numeric(use_robust && has_scaled)
        )
      },
      error = function(e) {
        c(chisq = NA, df = NA, pvalue = NA, rmsea = NA, cfi = NA, tli = NA, srmr = NA, gfi = NA, agfi = NA, nfi = NA, nnfi = NA, is_robust = 0)
      }
    )


    # AVE <- tryCatch({
    #   semTools::AVE(object = fit)
    #   }, error = function(e) {
    #   '<NA>'
    # })
    # RelSEM <- tryCatch({
    #   semTools::compRelSEM(object = fit)
    # }, error = function(e) {
    #   '<NA>'
    # })
    # HTMT <- tryCatch({
    #   semTools::htmt(model = input$cfa_model_text, data = df)
    # }, error = function(e) {
    #   '<NA>'
    # })

    ave_cr <- tryCatch(
      {
        calc_ave_cr(fit)
      },
      error = function(e) {
        NA
      }
    )
    AVE <- setNames(ave_cr$AVE, rownames(ave_cr))
    RelSEM <- setNames(ave_cr$CR, rownames(ave_cr))
    HTMT <- tryCatch(
      {
        calc_htmt(df, input$cfa_model_text)
      },
      error = function(e) {
        NA
      }
    )

    scoreCfa <- tryCatch(
      {
        fs <- as.data.frame(lavaan::predict(fit))
        for (col in names(fs)) {
          min_val <- min(fs[[col]], na.rm = TRUE)
          max_val <- max(fs[[col]], na.rm = TRUE)
          if (max_val > min_val) {
            fs[[paste0(col, "_0_100")]] <- (fs[[col]] - min_val) / (max_val - min_val) * 100
          } else {
            fs[[paste0(col, "_0_100")]] <- 100
          }
        }
        fs
      },
      error = function(e) {
        "<NA>"
      }
    )

    current_count <- cfa_model_counter() + 1
    cfa_model_counter(current_count)
    new_id <- paste0("Model_", current_count)

    cur <- cfa_fit_list()

    # Auto-generate modification note
    auto_note <- ""
    if (length(cur) == 0) {
      auto_note <- "Initial Model"
    } else {
      last_model <- cur[[length(cur)]]
      old_syntax <- last_model$spec
      new_syntax <- input$cfa_model_text

      old_lines <- trimws(unlist(strsplit(old_syntax, "\n")))
      new_lines <- trimws(unlist(strsplit(new_syntax, "\n")))
      old_lines <- old_lines[old_lines != ""]
      new_lines <- new_lines[new_lines != ""]

      added <- setdiff(new_lines, old_lines)
      removed <- setdiff(old_lines, new_lines)

      changes <- c()
      if (length(added) > 0) changes <- c(changes, paste("Added:", paste(added, collapse = ", ")))
      if (length(removed) > 0) changes <- c(changes, paste("Removed:", paste(removed, collapse = ", ")))

      if (length(changes) == 0) {
        auto_note <- "Modified formatting only"
      } else {
        auto_note <- paste(changes, collapse = " | ")
      }
    }

    # Check for Heywood Cases (Negative Variances)
    has_heywood <- FALSE
    try(
      {
        pe <- lavaan::parameterEstimates(fit)
        if (any(pe$op == "~~" & pe$lhs == pe$rhs & pe$est < 0)) {
          has_heywood <- TRUE
          auto_note <- paste0(auto_note, " | \u26A0 Heywood")
        }
      },
      silent = TRUE
    )

    cur[[new_id]] <- list(
      id = new_id,
      spec = input$cfa_model_text,
      fit = fit,
      measures = measures,
      ave = AVE,
      relsem = RelSEM,
      htmt = HTMT,
      scoreCfa = as.data.frame(scoreCfa),
      note = auto_note,
      estimator = input$cfa_estimator,
      has_heywood = has_heywood,
      time = Sys.time()
    )

    cfa_fit_list(cur)
    cfa_current_id(new_id)
    showNotification(paste0("CFA finished: ", new_id), type = "message")
  })

  # ====== Fit Comparison Table =======
  output$fit_comparison <- renderUI({
    req(cfa_fit_list())
    req(cfa_fit_list())
    lst <- cfa_fit_list()
    if (length(lst) == 0) {
      return(tags$p("No models run yet. Run CFA to see results."))
    }

    # Buat tabel HTML manual
    column(
      12,
      tags$div(
        style = "margin-top: 0px; font-size: 13px;",
        tags$b("Fit Model Comparison")
      ),
      # ---Render table DT =====
      DTOutput("fit_comparison_dt"),
      tags$div(
        style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
        tags$b("Interpretation Guidelines:"),
        tags$ul(
          tags$li(tags$span(style = "color: green;", "Excellent:"), "χ²/df < 2 AND RMSEA ≤ 0.05 AND CFI ≥ 0.95 AND TLI ≥ 0.95 AND SRMR ≤ 0.05 (Hu & Bentler, 1999)"),
          tags$li(tags$span(style = "color: green;", "Good:"), "χ²/df < 3 AND RMSEA < 0.08 AND CFI ≥ 0.90 AND TLI ≥ 0.90 AND SRMR ≤ 0.08 (Schumacker & Lomax, 2016; Kline, 2016)"),
          tags$li(tags$span(style = "color: orange;", "Acceptable:"), "χ²/df < 5 AND (RMSEA < 0.10 OR CFI ≥ 0.85 OR SRMR ≤ 0.10) OR ≥ 3 indices meet 'Good' (Hair et al., 2019; Brown, 2015)"),
          tags$li(tags$span(style = "color: red;", "Poor:"), "Fails to meet Acceptable criteria"),
          tags$li(tags$span(style = "color: red;", "⚠ Heywood:"), "Negative variance detected (Chen et al., 2001)")
        )
      ),
      tags$hr(),
      column(
        6,
        tags$div(
          style = "margin-top: 0px; font-size: 13px;",
          tags$b("Average Variance Explained (AVE):"),
          DTOutput("ave_table") # Table AVE
        )
      ),
      column(
        6,
        tags$div(
          style = "margin-top: 1px; font-size: 13px;",
          tags$b("Composite Reliability (CR):"),
          DTOutput("reliability_table"), # Table Reliability
        )
      ),
      tags$div(
        style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
        tags$b("Interpretation Guidelines:"),
        tags$ul(
          tags$li(tags$span(style = "color: green;", "Good:"), "AVE ≥ 0.5; CR ≥ 0.7 (Hair et al., 2010; Fornell & Larcker, 1981)"),
          tags$li(tags$span(style = "color: orange;", "Acceptable:"), "AVE ≥ 0.4; CR ≥ 0.6 (Fornell & Larcker, 1981)"),
          tags$li(tags$span(style = "color: red;", "Poor:"), "Outside acceptable ranges")
        )
      ),
      conditionalPanel(
        condition = "input.htmt_opt == true",
        tags$hr(),
        tags$div(
          style = "margin-top: 0px; font-size: 13px;",
          tags$b("Heterotrait–Monotrait Ratio (HTMT) for Current Model")
        ),
        DTOutput("htmt_table"),
        tags$div(
          style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
          tags$b("Interpretation Guidelines:"),
          tags$ul(
            tags$li(tags$span(style = "color: green;", "Good discriminant validity:"), "HTMT < 0.7 "),
            tags$li(tags$span(style = "color: orange;", "Acceptable discriminant validity:"), "HTMT < 0.8"),
            tags$li(tags$span(style = "color: red;", "Potential discriminant validity issues:"), "HTMT ≥ 0.8")
          )
        )
      ),
      tags$hr(),
      column(
        12,
        tags$div(
          style = "margin-top: 0px; font-size: 13px;",
          tags$b("Modification Indices")
        ),
        tags$div(
          class = "alert alert-warning", style = "padding: 10px; font-size: 12px; margin-top: 10px;",
          tags$b(icon("exclamation-triangle"), " Warning: "),
          "Data-driven specification searches using Modification Indices should not be treated as prescriptive. Over-relying on MI without substantive theoretical justification can lead to overfitted models that do not generalize well. Only apply modifications that make theoretical sense (e.g., MacCallum et al., 1992)."
        ),
        DTOutput("mi_table")
      )
    )
  })

  output$fit_comparison_dt <- renderDT({
    req(cfa_fit_list())
    lst <- cfa_fit_list()
    if (length(lst) == 0) {
      return(NULL)
    }

    df <- do.call(rbind, lapply(lst, function(x) {
      m <- x$measures
      hw <- ifelse(!is.null(x$has_heywood) && x$has_heywood, TRUE, FALSE)
      status_info <- get_fit_status(
        m["chisq"], m["df"], m["pvalue"],
        m["rmsea"], m["cfi"], m["srmr"], m["tli"], hw
      )
      data.frame(
        Model = x$id,
        Estimator = ifelse(is.null(x$estimator), "Unknown", x$estimator),
        Note = ifelse(is.null(x$note) || x$note == "", "-", x$note),
        chisq = round(m["chisq"], 2),
        df = m["df"],
        chisq_df_ratio = ifelse(is.na(m["chisq"]), NA, sprintf("%.2f", m["chisq"] / m["df"])),
        pvalue = ifelse(is.na(m["pvalue"]), NA, sprintf("%.3f", m["pvalue"])),
        RMSEA = ifelse(is.na(m["rmsea"]), NA, sprintf("%.3f", m["rmsea"])),
        CFI = ifelse(is.na(m["cfi"]), NA, sprintf("%.3f", m["cfi"])),
        TLI = ifelse(is.na(m["tli"]), NA, sprintf("%.3f", m["tli"])),
        GFI = ifelse(is.na(m["gfi"]), NA, sprintf("%.3f", m["gfi"])),
        SRMR = ifelse(is.na(m["srmr"]), NA, sprintf("%.3f", m["srmr"])),
        NFI = ifelse(is.na(m["nfi"]), NA, sprintf("%.3f", m["nfi"])),
        Heywood = ifelse(hw, "Yes", "No"),
        Status = status_info$status,
        StatusColor = status_info$color,
        stringsAsFactors = FALSE
      )
    }))

    # Add Action column
    df$Action <- sapply(seq_len(nrow(df)), function(i) {
      as.character(
        actionButton(
          inputId = paste0("del_model_btn_", i),
          label = icon("trash"),
          class = "btn-danger btn-sm",
          style = "font-size: 11px !important; padding: 2px 10px !important;",
          onclick = paste0('Shiny.setInputValue("del_model_selected", "', df$Model[i], '", {priority: "event"})')
        )
      )
    })

    # We want Note column to be editable. Columns are 0-indexed in DT when rownames=FALSE.
    # Note is the 3rd column -> index 2.
    # We disable all except index 2.
    # Columns in df: Model(0), Estimator(1), Note(2), chisq(3), df(4), chisq_df_ratio(5), pvalue(6), RMSEA(7), CFI(8), TLI(9), GFI(10), SRMR(11), NFI(12), Heywood(13), Status(14), Action(15) (we will exclude StatusColor)

    # Drop StatusColor for display
    display_df <- df[, c("Model", "Estimator", "Note", "chisq", "df", "chisq_df_ratio", "pvalue", "RMSEA", "CFI", "TLI", "GFI", "SRMR", "NFI", "Heywood", "Status", "Action")]

    datatable(display_df,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      editable = list(target = "cell", disable = list(columns = c(0, 1, 3:15))),
      extensions = "Buttons",
      options = list(
        dom = "Brt",
        scrollX = TRUE,
        pageLength = 20,
        buttons = list(
          list(
            extend = "excel",
            text = "Export Excel",
            filename = paste0("Fit Model Comparison")
          )
        ),
        columnDefs = list(list(className = "dt-center", targets = 3:12))
      )
    ) %>%
      formatStyle("chisq_df_ratio", color = styleInterval(c(2, 5), c("green", "orange", "red")), fontWeight = "bold", textAlign = "center") %>%
      formatStyle("pvalue", color = styleInterval(c(0.05, 0.1), c("red", "green", "green")), fontWeight = "bold", textAlign = "center") %>%
      formatStyle("RMSEA", color = styleInterval(c(0.08, 0.1), c("green", "orange", "red")), fontWeight = "bold", textAlign = "center") %>%
      formatStyle("CFI", color = styleInterval(c(0.88, 0.9), c("red", "orange", "green")), fontWeight = "bold", textAlign = "center") %>%
      formatStyle("TLI", color = styleInterval(c(0.88, 0.9), c("red", "orange", "green")), fontWeight = "bold", textAlign = "center") %>%
      formatStyle("GFI", color = styleInterval(c(0.88, 0.9), c("red", "orange", "green")), fontWeight = "bold", textAlign = "center") %>%
      formatStyle("SRMR", color = styleInterval(c(0.05, 0.08), c("green", "orange", "red")), fontWeight = "bold", textAlign = "center") %>%
      formatStyle("NFI", color = styleInterval(c(0.88, 0.9), c("red", "orange", "green")), fontWeight = "bold", textAlign = "center") %>%
      formatStyle("Status", backgroundColor = styleEqual(df$Status, df$StatusColor), fontWeight = "bold", color = "black", textAlign = "center") %>%
      formatStyle("Heywood", color = styleEqual(c("Yes", "No"), c("red", "green")), fontWeight = "bold", textAlign = "center")
  })

  # Handle note edit in fit comparison table
  observeEvent(input$fit_comparison_dt_cell_edit, {
    info <- input$fit_comparison_dt_cell_edit
    str(info)
    # info$row is the row number (1-indexed in R)
    # info$col is the column number (0-indexed from JS)
    # info$value is the new value
    req(info)

    cur_list <- cfa_fit_list()
    # The models are ordered the same as in the table
    if (info$row <= length(cur_list)) {
      model_names <- names(cur_list)
      model_id <- model_names[info$row]

      # Update the note
      cur_list[[model_id]]$note <- as.character(info$value)

      # Save back to reactive val
      cfa_fit_list(cur_list)
    }
  })

  # Handle model deletion
  observeEvent(input$del_model_selected, {
    req(input$del_model_selected)

    cur_list <- cfa_fit_list()
    model_id <- input$del_model_selected

    if (model_id %in% names(cur_list)) {
      cur_list[[model_id]] <- NULL
      cfa_fit_list(cur_list)

      # Update current ID
      if (length(cur_list) > 0) {
        cfa_current_id(names(cur_list)[length(cur_list)])
      } else {
        cfa_current_id(NULL)
      }

      showNotification(paste("Deleted", model_id), type = "message")
    }
  })

  # === Output: HTMT Table =====
  output$htmt_table <- renderDT({
    lst <- cfa_fit_list()
    if (length(lst) == 0) {
      return(NULL)
    }

    # Ambil model terakhir
    last_model <- lst[[length(lst)]]

    if (!is.null(last_model$htmt) && !identical(last_model$htmt, "<NA>")) {
      # Konversi HTMT matrix ke data frame
      htmt_matrix <- as.matrix(last_model$htmt)
      htmt_df <- as.data.frame(htmt_matrix)

      # Tambahkan nama baris sebagai kolom pertama
      htmt_df <- cbind(Variable = rownames(htmt_df), htmt_df)
      rownames(htmt_df) <- NULL

      # Identifikasi kolom numeric (HTMT values)
      numeric_cols <- which(sapply(htmt_df, is.numeric))

      datatable(htmt_df,
        rownames = FALSE, extensions = "Buttons",
        options = list(
          dom = "Bt",
          pageLength = nrow(htmt_df),
          buttons = list(
            list(
              extend = "csv",
              text = "Export CSV",
              filename = paste0("HTMT")
            ),
            list(
              extend = "excel",
              text = "Export Excel",
              filename = paste0("HTMT")
            )
          )
        )
      ) %>%
        formatStyle(
          columns = numeric_cols,
          color = styleInterval(c(0.7, 0.8), c("green", "orange", "red"))
        ) %>%
        formatStyle(
          columns = "Variable",
          backgroundColor = "#f8f9fa",
          fontWeight = "bold"
        ) %>%
        formatRound(columns = numeric_cols, digits = 3)
    } else {
      retu(data.frame(Message = "HTMT not available for the last model"))
    }
  })

  # ===== TABEL AVE =====
  output$ave_table <- renderDT({
    lst <- cfa_fit_list()
    if (length(lst) == 0) {
      return(datatable(data.frame(Message = "No models available")))
    }

    # Pastikan dplyr tersedia
    if (!require(dplyr, quietly = TRUE)) {
      stop("Package dplyr is required")
    }

    ave_data <- lapply(lst, function(x) {
      if (!is.null(x$ave) && !identical(x$ave, "<NA>")) {
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

    datatable(combined_data,
      extensions = "Buttons",
      rownames = FALSE,
      options = list(
        dom = "Bt",
        buttons = list(
          list(
            extend = "csv",
            text = "Export CSV",
            filename = paste0("AVE")
          ),
          list(
            extend = "excel",
            text = "Export Excel",
            filename = paste0("AVE")
          )
        )
      )
    ) %>%
      formatStyle(
        columns = which(!names(combined_data) %in% c("Model", "Note")),
        color = styleInterval(c(0.45, 0.5), c("red", "orange", "green"))
      ) %>%
      formatRound(columns = which(sapply(combined_data, is.numeric)), digits = 3)
  })

  # ===== Table  Reliability =====
  output$reliability_table <- renderDT({
    lst <- cfa_fit_list()
    if (length(lst) == 0) {
      return(datatable(data.frame(Message = "No models available")))
    }

    rel_data <- lapply(lst, function(x) {
      if (!is.null(x$relsem) && !identical(x$relsem, "<NA>")) {
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

    datatable(combined_data,
      rownames = FALSE, extensions = "Buttons",
      options = list(
        dom = "Bt",
        buttons = list(
          list(
            extend = "csv",
            text = "Export CSV",
            filename = paste0("Composite Realiability")
          ),
          list(
            extend = "excel",
            text = "Export Excel",
            filename = paste0("Composite Reliability")
          )
        )
      )
    ) %>%
      formatStyle(
        columns = numeric_cols,
        color = styleInterval(c(0.6, 0.7), c("red", "orange", "green"))
      ) %>%
      formatStyle(
        columns = "Model",
        backgroundColor = "#f8f9fa"
      ) %>%
      formatRound(columns = numeric_cols, digits = 3)
  })

  # ===== Table  FSCORES =====
  output$fscores_cfa <- renderDT(
    {
      req(input$run_cfa)
      lst <- cfa_fit_list()
      cur_id <- cfa_current_id()
      if (is.null(cur_id)) {
        return(NULL)
      }
      scoreCfa <- cfa_fit_list()[[cur_id]]$scoreCfa
      numeric_cols <- which(sapply(scoreCfa, is.numeric))
      datatable(scoreCfa,
        rownames = TRUE, extensions = "Buttons",
        options = list(
          dom = "Brtp",
          buttons = list(
            list(
              extend = "csv",
              text = "Export CSV",
              filename = paste0("Factor Scores")
            ),
            list(
              extend = "excel",
              text = "Export Excel",
              filename = paste0("Factor Scores")
            )
          )
        )
      ) %>%
        formatRound(columns = numeric_cols, digits = 3)
    },
    server = FALSE
  )

  # ====== Data Preview Visualizations ======
  output$cfa_summary_table <- renderUI({
    req(data_user())
    vars_to_use <- input$selected_vars
    if (is.null(vars_to_use) || length(vars_to_use) == 0) vars_to_use <- names(data_user())
    valid_vars <- intersect(vars_to_use, names(data_user()))
    req(length(valid_vars) > 0)
    
    df <- data_user()[, valid_vars, drop = FALSE]
    
    summary_list <- lapply(names(df), function(v) {
      x <- df[[v]]
      x_na <- na.omit(x)
      n <- length(x_na)
      
      if (is.numeric(x)) {
        mean_val <- round(mean(x_na), 2)
        sd_val <- round(sd(x_na), 2)
        min_val <- round(min(x_na), 2)
        max_val <- round(max(x_na), 2)
        type <- "Scale"
        
        data.frame(
          Variable = v,
          Type = type,
          N = n,
          `Mean/Mode` = as.character(mean_val),
          `SD/Count` = as.character(sd_val),
          Min = as.character(min_val),
          Max = as.character(max_val),
          check.names = FALSE
        )
      } else {
        tbl <- table(x_na)
        if(length(tbl) > 0) {
          mode_val <- names(tbl)[which.max(tbl)]
          count_val <- max(tbl)
        } else {
          mode_val <- "-"
          count_val <- "-"
        }
        type <- if (is.ordered(x)) "Ordinal" else "Nominal"
        
        data.frame(
          Variable = v,
          Type = type,
          N = n,
          `Mean/Mode` = as.character(mode_val),
          `SD/Count` = as.character(count_val),
          Min = "-",
          Max = "-",
          check.names = FALSE
        )
      }
    })
    
    summary_df <- do.call(rbind, summary_list)
    
    html_out <- summary_df %>%
      kable(format = "html", escape = FALSE, caption = "<b>Table 1</b><br><i>Descriptive Statistics</i>", align = "llccccc") %>%
      kable_styling(bootstrap_options = "none", full_width = FALSE, position = "left") %>%
      row_spec(0, bold = FALSE, extra_css = "border-top: 1.5px solid black; border-bottom: 1.5px solid black; font-family: 'Times New Roman', Times, serif; font-size: 11pt;") %>%
      row_spec(nrow(summary_df), extra_css = "border-bottom: 1.5px solid black;") %>%
      column_spec(1:ncol(summary_df), extra_css = "padding: 4px 12px; font-size: 11pt; line-height: 1; font-family: 'Times New Roman', Times, serif;")
    
    HTML(as.character(html_out))
  })
  output$cfa_data_summary_plot <- renderPlot({
    req(data_user())
    vars_to_use <- input$cfa_summary_vars
    if (is.null(vars_to_use) || length(vars_to_use) == 0) vars_to_use <- input$selected_vars
    if (is.null(vars_to_use) || length(vars_to_use) == 0) vars_to_use <- names(data_user())

    valid_vars <- intersect(vars_to_use, names(data_user()))
    if (length(valid_vars) == 0) {
      return(NULL)
    }

    df <- data_user()[, valid_vars, drop = FALSE]

    types <- sapply(df, function(x) class(x)[1])
    type_df <- as.data.frame(table(types))
    names(type_df) <- c("DataType", "Count")
    
    theme_choice <- input$cfa_summary_theme
    if (is.null(theme_choice)) theme_choice <- "default"
    
    bar_colors <- switch(theme_choice,
      "pastel" = scale_fill_brewer(palette = "Pastel1"),
      "blue"   = scale_fill_brewer(palette = "Blues"),
      "dark"   = scale_fill_brewer(palette = "Dark2"),
      scale_fill_brewer(palette = "Set2")
    )

    ggplot(type_df, aes(x = DataType, y = Count, fill = DataType)) +
      geom_col(width = 0.5, show.legend = FALSE, alpha = 0.9, color = "transparent") +
      geom_text(aes(label = Count), vjust = -0.5, fontface = "bold", color = "#495057") +
      bar_colors +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, color = "#343a40"),
        axis.title.x = element_text(face = "bold", color = "#495057", margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", color = "#495057", margin = margin(r = 10)),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # Prevent overlap
        panel.grid.major.x = element_blank(),
        plot.margin = margin(15, 15, 15, 15),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)
      ) +
      labs(title = "Data Type Composition", x = "Data Type", y = "Number of Variables")
  }, bg="transparent")

  output$cfa_data_heatmap <- renderPlot({
    req(data_user())
    vars_to_use <- input$selected_vars
    if (is.null(vars_to_use) || length(vars_to_use) == 0) vars_to_use <- names(data_user())

    valid_vars <- intersect(vars_to_use, names(data_user()))
    if (length(valid_vars) == 0) {
      return(NULL)
    }

    df <- data_user()[, valid_vars, drop = FALSE]

    num_df <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(num_df) < 2) {
      plot.new()
      title(main = "Not enough numeric variables for correlation heatmap", col.main = "#6c757d")
      return(NULL)
    }

    cor_matrix <- cor(num_df, use = "pairwise.complete.obs")
    # Set upper triangle to NA to display only lower triangle for cleaner look
    cor_matrix[upper.tri(cor_matrix)] <- NA

    cor_melted <- as.data.frame(as.table(cor_matrix))
    cor_melted <- cor_melted[!is.na(cor_melted$Freq), ]
    names(cor_melted) <- c("Var1", "Var2", "value")

    theme_choice <- input$cfa_summary_theme
    if (is.null(theme_choice)) theme_choice <- "default"
    
    heatmap_colors <- switch(theme_choice,
      "pastel" = scale_fill_gradient2(low = "#b3cde3", high = "#fbb4ae", mid = "white", midpoint = 0, limit = c(-1,1), name="Pearson\nCorrelation"),
      "blue"   = scale_fill_gradient2(low = "#deebf7", high = "#3182bd", mid = "white", midpoint = 0, limit = c(-1,1), name="Pearson\nCorrelation"),
      "dark"   = scale_fill_gradient2(low = "#1b9e77", high = "#d95f02", mid = "white", midpoint = 0, limit = c(-1,1), name="Pearson\nCorrelation"),
      scale_fill_gradient2(low = "#4575b4", high = "#d73027", mid = "white", midpoint = 0, limit = c(-1,1), name="Pearson\nCorrelation")
    )

    ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white", size = 0.5) +
      heatmap_colors +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "#495057"),
        axis.text.y = element_text(color = "#495057"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, color = "#343a40"),
        plot.background = element_rect(fill = "transparent", color = NA)
      ) +
      coord_fixed() +
      labs(title = "Correlation Heatmap (Numeric Variables)")
  }, bg="transparent")

  # Helper function untuk menentukan status fit ======
  get_fit_status <- function(chisq, df, pvalue, rmsea, cfi, srmr, tli, has_heywood = FALSE) {
    if (is.na(chisq) || is.na(rmsea) || is.na(cfi) || is.na(srmr)) {
      status_res <- list(status = "Unknown", color = "#e2e3e5")
    } else {
      c_df <- chisq / df
      tli_val <- ifelse(is.na(tli), cfi, tli) # Fallback to CFI if TLI missing

      if (c_df < 2 && rmsea <= 0.05 && cfi >= 0.95 && tli_val >= 0.95 && srmr <= 0.05) {
        status_res <- list(status = "Excellent", color = "lightgreen")
      } else if (c_df < 3 && rmsea < 0.08 && cfi >= 0.90 && tli_val >= 0.90 && srmr <= 0.08) {
        status_res <- list(status = "Good", color = "#d4edda")
      } else if (c_df < 5 && (rmsea < 0.10 || cfi >= 0.85 || srmr <= 0.10) ||
        sum(c(c_df < 3, rmsea < 0.08, cfi >= 0.90, tli_val >= 0.90, srmr <= 0.08)) >= 3) {
        status_res <- list(status = "Acceptable", color = "#fff3cd")
      } else {
        status_res <- list(status = "Poor", color = "#f8d7da")
      }
    }

    if (has_heywood) {
      status_res$status <- paste(status_res$status, "(\u26A0 Heywood)")
      status_res$color <- "#ffcccc" # Overwrite with light red if Heywood case
    }

    return(status_res)
  }


  # Loadings table
  output$loadings_table <- renderDT(
    {
      cur_id <- cfa_current_id()
      if (is.null(cur_id)) {
        return(NULL)
      }

      fit <- cfa_fit_list()[[cur_id]]$fit

      if (input$cfa_std_est) {
        sol <- tryCatch(lavaan::standardizedSolution(fit), error = function(e) NULL)
      } else {
        sol <- tryCatch(lavaan::parameterEstimates(fit), error = function(e) NULL)
      }

      if (is.null(sol)) {
        return(datatable(data.frame(Message = "No parameter estimates available")))
      }

      loadings <- sol[sol$op == "=~", ]

      if (nrow(loadings) == 0) {
        return(datatable(data.frame(Message = "No factor loadings found")))
      }

      if (input$cfa_std_est) {
        display_df <- loadings[, c("lhs", "rhs", "est.std", "pvalue")]
        colnames(display_df) <- c("Factor", "Item", "StdLoading", "pvalue")
        display_df$StdLoading <- round(display_df$StdLoading, 3)
      } else {
        display_df <- loadings[, c("lhs", "rhs", "est", "se", "pvalue")]
        colnames(display_df) <- c("Factor", "Item", "Estimate", "SE", "pvalue")
        display_df$Estimate <- round(display_df$Estimate, 3)
        display_df$SE <- round(display_df$SE, 3)
      }

      display_df$pvalue <- format.pval(display_df$pvalue, digits = 3)

      datatable(display_df,
        extensions = "Buttons",
        options = list(
          pageLength = 15, scrollX = TRUE, dom = "Brtp",
          buttons = list(
            list(
              extend = "csv",
              text = "Export CSV",
              filename = paste0("Factor Loading")
            ),
            list(
              extend = "excel",
              text = "Export Excel",
              filename = paste0("Factor Loading")
            )
          )
        )
      ) %>%
        formatStyle(
          columns = "StdLoading",
          fontWeight = "bold",
          color = styleInterval(c(0.3, 0.5, 0.7), c("red", "orange", "blue", "green"))
        )
    },
    server = FALSE
  )

  # Modification indices
  mi_df <- reactive({
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) {
      return(NULL)
    }

    fit <- cfa_fit_list()[[cur_id]]$fit
    mi <- tryCatch(lavaan::modindices(fit, sort. = TRUE, minimum.value = 1), error = function(e) NULL)

    if (is.null(mi) || nrow(mi) == 0) {
      return(NULL)
    }

    if (nrow(mi) > 20) mi <- mi[1:20, ]

    mi
  })

  output$mi_table <- renderDT(
    {
      mi <- mi_df()
      # mi <- mi %>% dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
      if (is.null(mi)) {
        return(datatable(data.frame(Message = "No modification indices available")))
      }

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

      datatable(display_mi,
        rownames = FALSE, escape = FALSE,
        options = list(
          pageLength = 10, scrollX = TRUE, dom = "rtp",
          # Atur ukuran font dan styling
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-size': '5px', 'font-weight': 'bold'});",
            "$(this.api().table().body()).css({'font-size': '5px', 'line-height': '1'});",
            "}"
          )
        )
      ) %>%
        formatRound(columns = which(sapply(display_mi, is.numeric)), digits = 3)
    },
    server = FALSE
  )

  # Handle MI button clicks
  observeEvent(input$mi_selected, {
    req(input$mi_selected)

    mi <- mi_df()
    if (is.null(mi)) {
      return()
    }

    idx <- as.numeric(input$mi_selected)
    if (idx > nrow(mi)) {
      return()
    }

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
      "Blue-Yellow" = list(man = "#A1E3F9", lat = c("#FFFFBA")),
      "Ocean" = list(man = "#B3E5FC", lat = c("#81D4FA", "#4FC3F7", "#29B6F6")),
      "Forest" = list(man = "#C8E6C9", lat = c("#A5D6A7", "#81C784", "#4CAF50")),
      "Rainbow" = list(man = "#B3E5FC", lat = c("#FF8A65", "#81D4FA", "#AED581", "#CE93D8", "#FFF176")),
      "Pastel" = list(man = "#F8ECE5", lat = c("#E2E2E2", "#E8EAF6", "#F3E5F5", "#E0F2F1")),
      "Greyscale" = list(man = "#EEEEEE", lat = c("#DDDDDD", "#CCCCCC", "#BBBBBB", "#AAAAAA")),
      "Earth" = list(man = "#D7CCC8", lat = c("#BCAAA4", "#A1887F", "#8D6E63", "#795548")),
      "Vibrant" = list(man = "#FFE0B2", lat = c("#FF5252", "#E040FB", "#7C4DFF", "#536DFE")),
      "Monochrome" = list(man = "#E0E0E0", lat = c("#9E9E9E", "#757575", "#616161", "#424242")),
      "Sunset" = list(man = "#FFCCBC", lat = c("#FF8A65", "#FF7043", "#F4511E", "#E64A19")),
      "Rose" = list(man = "#F8BBD0", lat = c("#F06292", "#EC407A", "#E91E63", "#D81B60")),
      "Mint" = list(man = "#B2DFDB", lat = c("#4DB6AC", "#26A69A", "#009688", "#00897B"))
    )
  }

  # Get factor names safely
  get_factor_names <- function(fit_obj) {
    tryCatch(
      {
        if (inherits(fit_obj, "lavaan")) {
          params <- lavaan::parameterEstimates(fit_obj)
          factors <- unique(params$lhs[params$op == "=~"])
          if (length(factors) > 0) {
            return(factors)
          }

          model_text <- fit_obj@Model@model.syntax
          if (is.character(model_text)) {
            lines <- strsplit(model_text, "\n")[[1]]
            factor_lines <- grep("=~", lines, value = TRUE)
            factors <- sapply(strsplit(factor_lines, "=~"), function(x) trimws(x[1]))
            return(factors)
          }
        }
        return("Unknown")
      },
      error = function(e) {
        return("Error getting factors")
      }
    )
  }

  # Helper to generate the fit text
  get_fit_text <- function(m, selected, dec_sep = ".") {
    all_indices <- c("chisq", "df", "pvalue", "cmindf", "rmsea", "cfi", "gfi", "srmr", "tli", "nfi")
    if ("SELECT ALL" %in% selected) selected <- all_indices
    selected <- intersect(all_indices, selected)
    
    # Check if robust indices were used
    rob_pfx <- if ("is_robust" %in% names(m) && !is.na(m["is_robust"]) && m["is_robust"] == 1) " (R)" else ""
    
    labels <- c(chisq = paste0("χ²", rob_pfx), df = paste0("df", rob_pfx), pvalue = "p", 
                cmindf = paste0("CMIN/DF", rob_pfx), rmsea = paste0("RMSEA", rob_pfx), 
                cfi = paste0("CFI", rob_pfx), gfi = "GFI", srmr = "SRMR", 
                tli = paste0("TLI", rob_pfx), nfi = paste0("NFI", rob_pfx))
    
    # Calculate CMIN/DF if requested
    if ("cmindf" %in% selected && !is.na(m["chisq"]) && !is.na(m["df"]) && m["df"] > 0) {
      m["cmindf"] <- m["chisq"] / m["df"]
    } else if ("cmindf" %in% selected) {
      m["cmindf"] <- NA
    }
    
    format_val <- function(name, val) {
      if (is.na(val)) {
        return("NA")
      }
      fmt_with_sep <- function(x) {
        if (dec_sep == ",") gsub("\\.", ",", x) else x
      }
      if (name == "pvalue") {
        return(fmt_with_sep(format.pval(val, digits = 3)))
      }
      if (name == "chisq") {
        return(fmt_with_sep(as.character(round(val, 2))))
      }
      if (name == "df") {
        return(val)
      }
      return(fmt_with_sep(sprintf("%.3f", val)))
    }
    paste(sapply(selected, function(n) paste0(labels[n], "=", format_val(n, m[n]))), collapse = "; ")
  }

  # Variances table
  output$variances_table <- renderDT(
    {
      cur_id <- cfa_current_id()
      if (is.null(cur_id)) {
        return(NULL)
      }

      fit <- cfa_fit_list()[[cur_id]]$fit

      if (input$cfa_std_est) {
        sol <- tryCatch(lavaan::standardizedSolution(fit), error = function(e) NULL)
      } else {
        sol <- tryCatch(lavaan::parameterEstimates(fit), error = function(e) NULL)
      }

      if (is.null(sol)) {
        return(datatable(data.frame(Message = "No parameter estimates available")))
      }

      variances <- sol[sol$op == "~~" & sol$lhs == sol$rhs, ]

      if (nrow(variances) == 0) {
        return(datatable(data.frame(Message = "No variances found")))
      }

      if (input$cfa_std_est) {
        display_df <- variances[, c("lhs", "est.std", "se", "pvalue")]
        colnames(display_df) <- c("Variable", "StdVariance", "SE", "pvalue")
        display_df$StdVariance <- round(display_df$StdVariance, 3)
        display_df$SE <- round(display_df$SE, 3)
      } else {
        display_df <- variances[, c("lhs", "est", "se", "pvalue")]
        colnames(display_df) <- c("Variable", "Variance", "SE", "pvalue")
        display_df$Variance <- round(display_df$Variance, 3)
        display_df$SE <- round(display_df$SE, 3)
      }

      display_df$pvalue <- format.pval(display_df$pvalue, digits = 3)

      datatable(display_df,
        extensions = "Buttons",
        options = list(
          pageLength = 15, scrollX = TRUE, dom = "Brtp",
          buttons = list(
            list(extend = "csv", text = "Export CSV", filename = "Variances"),
            list(extend = "excel", text = "Export Excel", filename = "Variances")
          )
        )
      )
    },
    server = FALSE
  )

  # Variance warning
  output$variance_warning <- renderUI({
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) {
      return(NULL)
    }

    fit <- cfa_fit_list()[[cur_id]]$fit
    sol <- tryCatch(lavaan::parameterEstimates(fit), error = function(e) NULL)

    if (is.null(sol)) {
      return(NULL)
    }

    variances <- sol[sol$op == "~~" & sol$lhs == sol$rhs, ]
    has_negative <- any(variances$est < 0, na.rm = TRUE)

    if (has_negative) {
      tags$div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        tags$b(" Warning: Heywood case detected! One or more variances are negative. This may indicate a problem with the model or data.")
      )
    } else {
      NULL
    }
  })

  # HTML Report Download Handler
  # Modal for HTML export
  # Make tempdir available for iframe
  # Per-session directory, so concurrent users never share a report file
  cfa_report_res <- session_report_dir(session, "cfa")
  
  cfa_report_path <- reactiveVal(NULL)

  # Renders the R Markdown report and returns the path of the generated HTML
  # file (or NULL on failure). Shared by the preview button and the download
  # handler, so a report can be downloaded without generating a preview first.
  generate_cfa_report <- function(progress_label = "Generating Report...") {
    if (is.null(cfa_current_id())) {
      return(NULL)
    }
    cur_id <- cfa_current_id()
    fit_info <- cfa_fit_list()[[cur_id]]

    report_path <- "cfa_report.Rmd"
    if (!file.exists(report_path)) {
      report_path <- file.path(system.file("app", package = "projectLSA"), "cfa_report.Rmd")
    }

    tempReport <- file.path(cfa_report_res$path, "cfa_report.Rmd")
    file.copy(report_path, tempReport, overwrite = TRUE)

    # Get plot colors safely
    if (input$plot_color_scheme == "Custom") {
      man_col <- input$mancolour
      lat_col <- input$latcolour
    } else {
      colors <- get_color_scheme(input$plot_color_scheme)
      man_col <- colors$man
      lat_col <- colors$lat
    }

    # Get all models for history
    lst <- cfa_fit_list()
    fit_initial <- lst[[1]]$fit

    model_history_df <- do.call(rbind, lapply(lst, function(x) {
      m <- x$measures
      hw <- ifelse(!is.null(x$has_heywood) && x$has_heywood, TRUE, FALSE)
      status_info <- get_fit_status(
        m["chisq"], m["df"], m["pvalue"],
        m["rmsea"], m["cfi"], m["srmr"], m["tli"], hw
      )
      data.frame(
        Model = x$id,
        Note = ifelse(is.null(x$note) || x$note == "", "-", x$note),
        chisq = m["chisq"],
        df = m["df"],
        pvalue = m["pvalue"],
        RMSEA = m["rmsea"],
        CFI = m["cfi"],
        TLI = m["tli"],
        SRMR = m["srmr"],
        Heywood = ifelse(hw, "Yes", "No"),
        Status = status_info$status,
        stringsAsFactors = FALSE
      )
    }))

    out_html <- file.path(cfa_report_res$path, "cfa_report_out.html")

    # Detect model type
    pe <- lavaan::parameterEstimates(fit_info$fit)
    model_type <- if (any(pe$op == "~")) "SEM" else "CFA"

    showModal(modalDialog(progress_label, footer = NULL))
    ok <- tryCatch({
      rmarkdown::render(tempReport,
        output_file = out_html,
        params = list(
          model_type = model_type,
          fit_final = fit_info$fit,
          fit_initial = fit_initial,
          model_history_df = model_history_df,
          console_out = console_context$text,
          ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text,
          dec = input$cfa_dec_sep,
          ave = fit_info$ave,
          relsem = fit_info$relsem,
          htmt = fit_info$htmt,
          plot_standardized = input$plot_standardized,
          plot_style = input$plot_style,
          plot_layout = input$plot_layout,
          plot_residuals = input$plot_residuals,
          plot_exoCov = input$plot_exoCov,
          plot_edge_label_size = input$plot_edge_label_size,
          plot_nodesize_lat = input$plot_nodesize_lat,
          plot_nodesize_man = input$plot_nodesize_man,
          plot_nodesize_man2 = input$plot_nodesize_man2,
          plot_rotation = input$plot_rotation,
          man_col = man_col,
          lat_col = lat_col,
          bifactor = input$bifactor,
          edgewidth = input$edgewidth,
          plotwidth = input$plotwidth,
          plotheight = input$plotheight
        ),
        envir = new.env(parent = globalenv())
      )
      cfa_report_path(out_html)
      TRUE
    }, error = function(e) {
      showNotification(paste("Error generating report:", e$message), type = "error")
      FALSE
    }, finally = {
      removeModal()
    })

    if (isTRUE(ok)) out_html else NULL
  }

  observeEvent(input$cfa_generate_preview, {
    req(cfa_current_id())
    generate_cfa_report("Generating Report Preview...")
  })

  output$cfa_report_preview_frame <- renderUI({
    req(cfa_report_path())
    # Cache-busting query string so a regenerated report is actually reloaded
    tags$iframe(
      src = paste0(cfa_report_res$prefix, "/cfa_report_out.html?v=", as.integer(Sys.time())),
      width = "100%", height = "800px", style = "border: none;"
    )
  })

  # Export Report modal (also carries the support/donation message)
  observeEvent(input$btn_export_modal, {
    showModal(modalDialog(
      title = "Export HTML Report",
      tags$div(
        style = "text-align: center;",
        tags$h4("Your report is ready to download!"),
        tags$p("If you find projectLSA helpful for your research, consider supporting its continuous development. Your support helps us keep the project alive!"),
        tags$div(
          style = "background-color: #f8f9fa; border: 2px dashed #17a2b8; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
          tags$strong("Support via Bank BNI:", style = "color: #17a2b8; font-size: 16px;"), tags$br(),
          tags$span("0331355050", style = "font-size: 24px; font-weight: bold; color: #d32f2f; letter-spacing: 2px;"), tags$br(),
          tags$em("Account Name: Hasan Djidu"), tags$br(), tags$br(),
          tags$strong("International Transfer (SWIFT):", style = "color: #17a2b8; font-size: 14px;"), tags$br(),
          tags$span("BNINIDJA", style = "font-size: 16px; font-weight: bold; color: #d32f2f;")
        ),
        downloadButton("download_report_html_modal", "Download Report Now", class = "btn btn-success btn-lg")
      ),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })

  cfa_report_filename <- function() {
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) {
      return(paste0("Report_", Sys.Date(), ".html"))
    }

    fit_obj <- cfa_fit_list()[[cur_id]]$fit
    pe <- lavaan::parameterEstimates(fit_obj)

    # Detect if SEM
    is_sem <- any(pe$op == "~")
    prefix <- if (is_sem) "SEM_Report_" else "CFA_Report_"

    latents <- unique(pe$lhs[pe$op == "=~"])
    latent_name <- if (length(latents) > 0) latents[length(latents)] else "Model"

    paste0(prefix, latent_name, "_", Sys.Date(), ".html")
  }

  cfa_report_content <- function(file) {
    req(cfa_current_id())
    # Render on demand when the user downloads without generating a preview
    path <- cfa_report_path()
    if (is.null(path) || !file.exists(path)) {
      path <- generate_cfa_report("Generating Report...")
    }
    req(path)
    file.copy(path, file, overwrite = TRUE)
  }

  output$download_report_html <- downloadHandler(
    filename = cfa_report_filename,
    content = cfa_report_content,
    contentType = "text/html"
  )

  output$download_report_html_modal <- downloadHandler(
    filename = cfa_report_filename,
    content = cfa_report_content,
    contentType = "text/html"
  )

  # Refactored plot rendering (shared between display and download)
  render_cfa_plot_impl <- function() {
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) {
      plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE, main = "No CFA model available")
      text(1, 1, "Please run a CFA model first\n\n1. Select 'Built-in: HolzingerSwineford1939'\n2. Keep the example model\n3. Click 'Run CFA'", cex = 1.2, col = "darkred")
      return(invisible(NULL))
    }

    obj <- cfa_fit_list()[[cur_id]]
    fit_obj <- obj$fit
    m <- obj$measures

    if (input$plot_color_scheme == "Custom") {
      colors <- list(man = input$mancolour, lat = input$latcolour)
    } else {
      colors <- get_color_scheme(input$plot_color_scheme)
    }

    b_margin <- 6
    A <- semPlot::semPaths(
      fit_obj,
      what = ifelse(input$plot_standardized, "std", "est"),
      structural = (input$plot_model_scope == "structural"),
      style = input$plot_style,
      layout = input$plot_layout,
      layoutSplit = input$plot_layout_split,
      curvePivot = input$plot_curve,
      subScale = input$subScale_Wi,
      subScale2 = input$subScale_He,
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
      label.color = "black",
      mar = c(b_margin, 15 - input$plotwidth, 20 - input$plotheight, 15 - input$plotwidth),
      bifactor = input$bifactor,
      esize = input$edgewidth,
      edge.color = "black",
      freeStyle = c(1, "black"),
      fixedStyle = c(2, "black"),
      fade = FALSE,
      DoNotPlot = TRUE
    )

    A <- semptools::mark_sig(
      semPaths_plot = A, object = fit_obj,
      alphas = c("*" = 0.05, "**" = 0.01, "***" = 0.001)
    )

    # Apply decimal separator
    if (input$cfa_dec_sep == ",") {
      A$graphAttributes$Edges$labels <- gsub("\\.", ",", A$graphAttributes$Edges$labels)
    }

    plot(A)

    # Add fit info inside a box
    factor_names <- get_factor_names(fit_obj)
    fit_text_raw <- get_fit_text(m, input$fit_indices_selected, dec_sep = input$cfa_dec_sep)
    wrapped_fit <- paste(strwrap(paste0("Fit indices: ", fit_text_raw), width = 110), collapse = "\n")

    estimator_text <- paste0("Estimator = ", input$cfa_estimator, " | Factors: ", paste(factor_names, collapse = ", "))
    sig_sign <- '*** : p ≤ 0.001", "** : p ≤ 0.01", "* : p ≤ 0.05'

    legend_text <- c(wrapped_fit, estimator_text, sig_sign)
    text_colors <- c("#000000", "blue", "#00002d")

    leg_info <- legend("bottom", legend = legend_text, plot = FALSE, inset = c(0, 0), cex = 0.9, y.intersp = 1.0)

    max_w <- max(strwidth(legend_text, cex = 0.9))
    legend("bottom",
      legend = rep("", length(legend_text)), bg = "#f8f9fa", box.col = "#e2e8f0",
      inset = c(0, 0), cex = 0.9, xpd = TRUE, y.intersp = 1.0, text.width = max_w
    )

    center_x <- leg_info$rect$left + leg_info$rect$w / 2
    y_coords <- leg_info$text$y
    for (i in seq_along(legend_text)) {
      text(x = center_x, y = y_coords[i], labels = legend_text[i], col = text_colors[i], cex = 0.9, xpd = TRUE, adj = c(0.5, 0.5))
    }
  }

  output$path_plot <- renderPlot({
    tryCatch(
      {
        render_cfa_plot_impl()
      },
      error = function(e) {
        plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE, main = "Plot Error")
        text(1, 1, paste("Error creating plot:\n", e$message), cex = 1.2, col = "red")
      }
    )
  })

  output$download_cfa_plot <- downloadHandler(
    filename = function() {
      paste0("CFA_Plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file, width = 12, height = 10, units = "in", res = 300)
      tryCatch({
        render_cfa_plot_impl()
      }, finally = {
        dev.off()
      })
    }
  )

  observeEvent(input$fit_indices_selected, {
    sel <- input$fit_indices_selected
    if ("SELECT ALL" %in% sel && length(sel) > 1) {
      updateSelectInput(session, "fit_indices_selected",
        selected = if (tail(sel, 1) == "SELECT ALL") {
          "SELECT ALL"
        } else {
          setdiff(sel, "SELECT ALL")
        }
      )
    }
  })

  # Fit information below plot
  output$plot_fit_info <- renderUI({
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) {
      return(NULL)
    }

    obj <- cfa_fit_list()[[cur_id]]
    m <- obj$measures
    fit_obj <- obj$fit

    factor_names <- get_factor_names(fit_obj)
    selected <- input$fit_indices_selected
    fit_text <- get_fit_text(m, selected, dec_sep = input$cfa_dec_sep)

    estimator_text <- paste0(
      "Estimator = ", input$cfa_estimator,
      " | Factors: ", paste(factor_names, collapse = ", ")
    )
    sig_sign <- paste0(
      '*** : p ≤ 0.001", "** : p ≤ 0.01", "* : p ≤ 0.05'
    )

    tagList(
      tags$div(
        style = "margin-top: 0px; padding: 5px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
        tags$p(
          style = "margin: 0; font-size: 11px; color: #2c3e50;",
          tags$b("Fit indices: "), fit_text
        ),
        tags$p(
          style = "margin: 0; font-size: 11px; color: darkblue;",
          estimator_text
        ),
        tags$p(
          style = "margin: 0; font-size: 10px; color: #6c757d;",
          sig_sign
        )
      )
    )
  })

  # Download handlers
  output$download_measures <- downloadHandler(
    filename = function() paste0("cfa_measures_", Sys.Date(), ".csv"),
    content = function(file) {
      lst <- cfa_fit_list()
      if (length(lst) == 0) {
        return(NULL)
      }

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
      if (is.null(cur_id)) {
        return(NULL)
      }

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
      if (is.null(cur_id)) {
        return(NULL)
      }

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
      if (is.null(cur_id)) {
        return(NULL)
      }

      scoreCfa <- cfa_fit_list()[[cur_id]]$scoreCfa

      write.csv(scoreCfa, file, row.names = FALSE)
    }
  )

  # ====== AUTOMATED MEASUREMENT INVARIANCE ======
  observeEvent(input$run_auto_invariance, {
    req(data_user(), input$cfa_model_text, input$selected_vars, input$mgcfa_group_var)

    if (nchar(trimws(input$cfa_model_text)) == 0) {
      showNotification("Please enter a CFA model", type = "error")
      return()
    }

    updateTabsetPanel(session, "main_tab_cfa", selected = "invariance_tab_cfa")

    df_subset <- data_user()[, input$selected_vars, drop = FALSE]
    df <- cbind(df_subset, data_user()[, input$mgcfa_group_var, drop = FALSE])

    showModal(modalDialog("Running Automated Invariance Testing (Configural -> Metric -> Scalar -> Strict)...", footer = NULL))

    # 1. Configural
    fit_config <- run_lavaan(model_text = input$cfa_model_text, data = df, estimator = input$cfa_estimator, missing = input$cfa_missing, mode = "mgcfa", group = input$mgcfa_group_var, group_equal = "configural")
    # 2. Metric
    fit_metric <- run_lavaan(model_text = input$cfa_model_text, data = df, estimator = input$cfa_estimator, missing = input$cfa_missing, mode = "mgcfa", group = input$mgcfa_group_var, group_equal = "loadings")
    # 3. Scalar
    fit_scalar <- run_lavaan(model_text = input$cfa_model_text, data = df, estimator = input$cfa_estimator, missing = input$cfa_missing, mode = "mgcfa", group = input$mgcfa_group_var, group_equal = "loadings, intercepts")
    # 4. Strict
    fit_strict <- run_lavaan(model_text = input$cfa_model_text, data = df, estimator = input$cfa_estimator, missing = input$cfa_missing, mode = "mgcfa", group = input$mgcfa_group_var, group_equal = "loadings, intercepts, residuals")

    removeModal()

    output$invariance_results <- renderUI({
      extract_fit <- function(f, name) {
        if (is.null(f)) {
          return(data.frame(Model = name, Chisq = NA, df = NA, pvalue = NA, CFI = NA, RMSEA = NA))
        }
        fm <- lavaan::fitMeasures(f)
        get_val <- function(std, scaled, robust) {
          if (!missing(robust) && robust %in% names(fm) && !is.na(fm[robust])) {
            return(fm[robust])
          }
          if (!missing(scaled) && scaled %in% names(fm) && !is.na(fm[scaled])) {
            return(fm[scaled])
          }
          if (std %in% names(fm)) {
            return(fm[std])
          }
          return(NA)
        }
        chisq <- get_val("chisq", "chisq.scaled")
        df <- get_val("df", "df.scaled")
        pvalue <- get_val("pvalue", "pvalue.scaled")
        cfi <- get_val("cfi", "cfi.scaled", "cfi.robust")
        rmsea <- get_val("rmsea", "rmsea.scaled", "rmsea.robust")
        data.frame(Model = name, Chisq = chisq, df = df, pvalue = pvalue, CFI = cfi, RMSEA = rmsea)
      }

      res <- rbind(
        extract_fit(fit_config, "Configural (Base)"),
        extract_fit(fit_metric, "Metric (Weak)"),
        extract_fit(fit_scalar, "Scalar (Strong)"),
        extract_fit(fit_strict, "Strict (Residual)")
      )

      res$Delta_Chisq <- c(NA, diff(res$Chisq))
      res$Delta_df <- c(NA, diff(res$df))
      res$p_Delta_Chisq <- pchisq(abs(res$Delta_Chisq), abs(res$Delta_df), lower.tail = FALSE)
      res$Delta_CFI <- c(NA, diff(res$CFI))
      res$Delta_RMSEA <- c(NA, diff(res$RMSEA))

      # Fix signs for Delta CFI and RMSEA for better readability
      res_table <- datatable(res, rownames = FALSE, options = list(dom = "t", scrollX = TRUE)) %>%
        formatRound(c("Chisq", "CFI", "RMSEA", "Delta_Chisq", "Delta_CFI", "Delta_RMSEA"), digits = 3) %>%
        formatRound(c("pvalue", "p_Delta_Chisq"), digits = 4)

      tagList(
        tags$h4("Measurement Invariance Testing Results"),
        tags$p("Comparison of nested models: Configural vs Metric vs Scalar vs Strict"),
        res_table,
        br(),
        tags$b("Interpretation Guidelines (Chen, 2007; Cheung & Rensvold, 2002):"),
        tags$ul(
          tags$li("Invariance is supported if ", tags$b("ΔCFI ≥ -0.010"), " and ", tags$b("ΔRMSEA ≤ 0.015"), " (or non-significant Δχ²)."),
          tags$li("Strict invariance is often too restrictive and rarely achieved in practice.")
        )
      )
    })
  })

  # ==== R Console Output & Model Export ====
  observeEvent(cfa_current_id(), {
    cur_id <- cfa_current_id()
    if (is.null(cur_id)) {
      console_context$text <- "No model available."
    } else {
      all_models <- cfa_fit_list()
      full_out <- ""
      
      for (i in seq_along(all_models)) {
        mod <- all_models[[i]]
        out <- paste0("\n========================================\n", 
                      "           ", mod$id, "\n",
                      "========================================\n")
        if (mod$note != "") out <- paste0(out, "Note: ", mod$note, "\n\n")
        
        out <- paste0(out, paste(capture.output(print(summary(mod$fit, fit.measures = TRUE, standardized = TRUE))), collapse = "\n"))
        
        if (requireNamespace("semTools", quietly = TRUE)) {
          try({
            rel <- semTools::reliability(mod$fit)
            out <- paste0(out, "\n\n=== RELIABILITY & AVE ===\n", paste(capture.output(print(rel)), collapse = "\n"))
          }, silent = TRUE)
        }
        full_out <- paste0(full_out, out, "\n")
      }
      
      console_context$text <- full_out
    }
  })

  output$export_cfa_rds <- downloadHandler(
    filename = function() { paste0("CFA_Workspace_", Sys.Date(), ".rds") },
    content = function(file) {
      cur_id <- cfa_current_id()
      req(cur_id)
      workspace <- list(
        type = "projectLSA_workspace",
        module = "CFA",
        raw_data = raw_data_user(),
        cfa_fit_list = cfa_fit_list(),
        cfa_current_id = cfa_current_id(),
        cfa_aggregations = cfa_aggregations(),
        input_state = list(
          cfa_estimator = input$cfa_estimator,
          cfa_missing = input$cfa_missing,
          cfa_std_est = input$cfa_std_est,
          htmt_opt = input$htmt_opt,
          cfa_analysis_mode = input$cfa_analysis_mode,
          mgcfa_invariance = input$mgcfa_invariance,
          mgcfa_group = input$mgcfa_group,
          cfa_model_text = input$cfa_model_text,
          cfa_summary_theme = input$cfa_summary_theme,
          plot_model_scope = input$plot_model_scope,
          plot_style = input$plot_style,
          plot_layout = input$plot_layout,
          plot_color_scheme = input$plot_color_scheme,
          plot_standardized = input$plot_standardized,
          fit_indices_selected = input$fit_indices_selected,
          cfa_agg_syntax = input$cfa_agg_syntax,
          cfa_agg_method = input$cfa_agg_method
        )
      )
      saveRDS(workspace, file)
    }
  )

  # ==== Score New Data ====
  output$download_cfa_template <- downloadHandler(
    filename = function() {
      "CFA_template.xlsx"
    },
    content = function(file) {
      cur_id <- cfa_current_id()
      req(cur_id)
      fit <- cfa_fit_list()[[cur_id]]$fit
      # extract item names from lavaan model
      items <- lavaan::lavNames(fit, type = "ov")
      df <- data.frame(matrix(ncol = length(items), nrow = 0))
      colnames(df) <- items
      writexl::write_xlsx(df, file)
    }
  )

  # Handle New Data Upload and Calculation
  cfa_newdata_raw <- reactiveVal(NULL)

  observeEvent(input$cfa_newdata, {
    req(input$cfa_newdata)
    ext <- tools::file_ext(input$cfa_newdata$name)
    df <- switch(ext,
      "csv" = read.csv(input$cfa_newdata$datapath),
      "xlsx" = readxl::read_excel(input$cfa_newdata$datapath),
      "xls" = readxl::read_excel(input$cfa_newdata$datapath),
      stop("Invalid file format")
    )
    cfa_newdata_raw(df)
    updatePickerInput(session, "cfa_newdata_agg_vars", choices = names(df))
  })

  observeEvent(input$cfa_newdata_btn_aggregate, {
    df <- cfa_newdata_raw()
    syntax <- input$cfa_newdata_agg_syntax
    req(df, syntax)
    
    lines <- strsplit(syntax, "\n")[[1]]
    count <- 0
    method <- input$cfa_newdata_agg_method
    
    for (line in lines) {
      if (grepl("=", line)) {
        parts <- strsplit(line, "=")[[1]]
        if (length(parts) == 2) {
          name <- make.names(trimws(parts[1]))
          vars_raw <- strsplit(parts[2], "[, ]+")[[1]]
          vars <- trimws(vars_raw[vars_raw != ""])
          
          # Only use vars that exist in df
          valid_vars <- intersect(vars, names(df))
          if (length(valid_vars) > 0 && name != "") {
            if (method == "mean") {
              df[[name]] <- rowMeans(df[, valid_vars, drop = FALSE], na.rm = TRUE)
            } else if (method == "sum") {
              df[[name]] <- rowSums(df[, valid_vars, drop = FALSE], na.rm = TRUE)
            }
            count <- count + 1
          }
        }
      }
    }
    
    if (count > 0) {
      cfa_newdata_raw(df)
      shinyWidgets::updatePickerInput(session, "cfa_newdata_agg_vars_insert", choices = names(df))
      showNotification(paste(count, "variables calculated and added to new data."), type = "message")
      shinyWidgets::updatePickerInput(session, "cfa_newdata_agg_vars_insert", selected = character(0))
    }
  })

  # Handle insert button for new data
  observeEvent(input$cfa_newdata_btn_insert_agg, {
    vars <- input$cfa_newdata_agg_vars_insert
    if (length(vars) > 0) {
      current <- input$cfa_newdata_agg_syntax
      prefix <- ifelse(!is.null(current) && nchar(current) > 0 && !endsWith(current, "\n"), "\n", "")
      new_text <- paste0(current, prefix, "NewVar = ", paste(vars, collapse = ", "))
      updateTextAreaInput(session, "cfa_newdata_agg_syntax", value = new_text)
    }
  })

  observeEvent(input$cfa_newdata_btn_reset_agg, {
    req(input$cfa_newdata)
    ext <- tools::file_ext(input$cfa_newdata$name)
    df <- switch(ext,
      "csv" = read.csv(input$cfa_newdata$datapath),
      "xlsx" = readxl::read_excel(input$cfa_newdata$datapath),
      "xls" = readxl::read_excel(input$cfa_newdata$datapath),
      stop("Invalid file format")
    )
    cfa_newdata_raw(df)
    updatePickerInput(session, "cfa_newdata_agg_vars", choices = names(df))
    showNotification("New data has been reset to its original uploaded state.", type = "message")
  })

  cfa_current_newdata_view <- reactiveVal(NULL)

  observe({
    cfa_current_newdata_view(cfa_newdata_raw())
  })

  observeEvent(input$cfa_score_newdata_btn, {
    df <- cfa_newdata_raw()
    req(df)
    cur_id <- cfa_current_id()
    req(cur_id)
    fit <- cfa_fit_list()[[cur_id]]$fit
    
    tryCatch({
      fs <- as.data.frame(lavaan::lavPredict(fit, newdata = df))
      for (col in names(fs)) {
        min_val <- min(fs[[col]], na.rm = TRUE)
        max_val <- max(fs[[col]], na.rm = TRUE)
        if (max_val > min_val) {
          fs[[paste0(col, "_0_100")]] <- (fs[[col]] - min_val) / (max_val - min_val) * 100
        } else {
          fs[[paste0(col, "_0_100")]] <- 100
        }
      }
      res <- cbind(df, fs)
      cfa_current_newdata_view(res)
      showNotification("Factor scores calculated and appended to the data.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error computing scores:", e$message), type = "error")
    })
  })

  output$cfa_newscores_table <- DT::renderDataTable({
    req(cfa_current_newdata_view())
    df <- cfa_current_newdata_view()
    
    # Round only numeric columns
    num_cols <- sapply(df, is.numeric)
    if (any(num_cols)) {
      df[num_cols] <- lapply(df[num_cols], round, 3)
    }
    
    datatable(df, options = list(scrollX = TRUE))
  })

  output$download_cfa_newscores <- downloadHandler(
    filename = function() {
      "CFA_New_Scores.csv"
    },
    content = function(file) {
      req(cfa_current_newdata_view())
      write.csv(cfa_current_newdata_view(), file, row.names = FALSE)
    }
  )

  # ==== AI Assistant ====
  # Update global AI context whenever results change
  observe({
    res_text <- ""
    cur_id <- cfa_current_id()
    if (!is.null(cur_id) && !is.null(cfa_fit_list()[[cur_id]]$fit)) {
      all_models <- cfa_fit_list()
      
      for (i in seq_along(all_models)) {
        mod <- all_models[[i]]
        fit <- mod$fit
        m <- mod$measures
        
        fit_text <- paste0("\n========================================\n",
                           "           ", mod$id, "\n",
                           "========================================\n")
        if (mod$note != "") {
           fit_text <- paste0(fit_text, "Modification Note: ", mod$note, "\n\n")
        }
        
        fit_text <- paste0(fit_text, "EXACT FIT INDICES (MUST USE THESE VALUES FOR INTERPRETATION!):\n")
        if (!is.null(m)) {
           fit_text <- paste0(fit_text, 
              "Chi-Square: ", round(m["chisq"],2), " (df=", m["df"], ", p=", round(m["pvalue"],3), ")\n",
              "RMSEA: ", round(m["rmsea"],3), "\n",
              "CFI: ", round(m["cfi"],3), "\n",
              "TLI: ", round(m["tli"],3), "\n",
              "SRMR: ", round(m["srmr"],3), "\n",
              "GFI: ", round(m["gfi"],3), "\n"
           )
        }
        summary_text <- paste(capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE)), collapse = "\n")
        
        rel_text <- ""
        if (requireNamespace("semTools", quietly = TRUE)) {
          try({
            rel <- semTools::reliability(fit)
            rel_text <- paste0("\n\nRELIABILITY & AVE:\n", paste(capture.output(print(rel)), collapse = "\n"))
          }, silent = TRUE)
        }
        
        res_text <- paste0(res_text, fit_text, "\nRAW MODEL SUMMARY:\n", summary_text, rel_text, "\n")
      }
    }
    ai_context$results_text <- res_text
    ai_context$module <- "Confirmatory Factor Analysis (CFA)"
  })

  # ====== Methodological Guide Modal ======
  observeEvent(input$cfa_method_guide, {
    showModal(modalDialog(
      title = tags$b(icon("book"), " Methodological Guidelines"),
      size = "l",
      tags$div(
        tags$h4(tags$b("Estimator Selection")),
        tags$p("Choosing the right estimator is critical for valid inferences in CFA/SEM:"),
        tags$ul(
          tags$li(tags$b("ML (Maximum Likelihood):"), " Default for continuous data with multivariate normality."),
          tags$li(tags$b("MLR (Robust ML):"), " Recommended for continuous data that violate normality assumptions (computes robust standard errors and scaled test statistics)."),
          tags$li(tags$b("WLSMV (Weighted Least Squares Mean and Variance adjusted):"), " Best practice for ordinal or categorical data (e.g., Likert scales with < 5 points)."),
          tags$li(tags$b("Other WLS/ULS variants:"), " Used for specific covariance structure requirements (see Kline, 2016).")
        ),
        tags$hr(),
        tags$h4(tags$b("Missing Data Treatment")),
        tags$p("Improper handling of missing data can bias parameter estimates:"),
        tags$ul(
          tags$li(tags$b("Listwise Deletion:"), " Default. Removes any row with a missing value. Valid only if data is Missing Completely at Random (MCAR). Can severely reduce statistical power."),
          tags$li(tags$b("FIML (Full Information Maximum Likelihood):"), " Often the gold standard for Missing at Random (MAR) data. Uses all available data points without discarding cases (Enders, 2010)."),
          tags$li(tags$b("Pairwise Deletion:"), " Uses available pairs for correlation. Generally discouraged due to potential non-positive definite covariance matrices.")
        ),
        tags$hr(),
        tags$h5(tags$b("References:")),
        tags$ul(
          style = "font-size: 12px;",
          tags$li("Kline, R. B. (2016). Principles and practice of structural equation modeling (4th ed.). Guilford publications."),
          tags$li("Enders, C. K. (2010). Applied missing data analysis. Guilford press."),
          tags$li("MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model modifications in covariance structure analysis: The problem of capitalization on chance. Psychological Bulletin, 111(3), 490.")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}
