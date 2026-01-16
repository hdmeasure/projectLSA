# ==== server_lpa.R ====
server_lpa <- function(input, output, session) {
  library(tidyLPA)
  library(mclust)
  library(ggplot2)
  library(stats)
  library(haven)
  set.seed(100)

  # --- Upload data ----
  data_user <- reactive({
    if (input$data_source == "pisaUSA15") {
      df <- tidyLPA::pisaUSA15[1:500,]%>% mutate(id_auto = paste0("id_", sprintf("%04d", 1:n())))
    } else if (input$data_source == "curry_mac") {
      df <- tidyLPA::curry_mac %>% mutate(id_auto = paste0("id_", sprintf("%04d", 1:n())))
      
    } else if (input$data_source == "id_edu") {
      df <- tidyLPA::id_edu %>% mutate(id_auto = paste0("id_", sprintf("%04d", 1:n())))
    } else {
      req(input$datafile)
      ext <- tolower(tools::file_ext(input$datafile$name))
      showModal(modalDialog(title = NULL, "Reading Your File, Please wait...", footer = NULL, easyClose = FALSE))
      df <- switch(
        ext,
        "csv"  = read.csv(input$datafile$datapath, stringsAsFactors = FALSE),
        "xls"  = readxl::read_excel(input$datafile$datapath),
        "xlsx" = readxl::read_excel(input$datafile$datapath),
        "sav"  = haven::read_sav(input$datafile$datapath),
        "rds"  = readRDS(input$datafile$datapath),
        stop("Unsupported file type. Please upload CSV, Excel, SPSS (.sav), or RDS file.")
      )
      removeModal()
      df <- df %>% mutate(across(everything(), ~ifelse(.x == "", NA, .x)),
                          id_auto = paste0("id_", sprintf("%04d", 1:n()))
                          )
    }
    return(df)
  })
  observeEvent(input$data_source, {
    if (input$data_source != "upload") {
      showNotification(paste("Using built-in dataset:", input$data_source), type = "message")
    }
  })
  output$data_description <- renderUI({
    desc_html <- if (input$data_source == "pisaUSA15") {
      desc_pisa
    } else if (input$data_source == "curry_mac") {
      desc_curry_text
    } else if (input$data_source == "id_edu") {
      desc_id_edu
    } else { ""
    }
    
    div(
      style = "font-size: 13px; line-height: 1.4; color: #333;",
      HTML(desc_html)
    )
  })
  # ---  ID ---
  output$id_select_ui <- renderUI({
    req(data_user())
    selectInput(
      "id_vars",
      label = "Select ID Columns (Optional):",
      choices = names(data_user()),
      selected = names(data_user())[str_detect(names(data_user()), "id")],
      multiple = TRUE
      )
  })
  
  # --- Pilih variabel ---
  output$var_select_ui <- renderUI({
    req(data_user())
    selectInput(
      "selected_vars",
      label = "Select Variables for LPA:",
      choices = names(data_user()),
      selected = names(data_user())[1:min(3, ncol(data_user()))],
      multiple = TRUE 
      )
  })
  
  observeEvent(c(input$data_source, input$datafile), {
    updateSelectInput(session,"selected_vars",selected = "") 
  }, ignoreInit = TRUE)
  
  # --- Data preview ----
  output$data_preview <- renderDT({
    req(data_user(), input$selected_vars)
    df <- data_user() %>% 
      dplyr::select(input$selected_vars)
    numeric_cols <- which(sapply(df, function(x) is.numeric(x)))
    datatable(df,
              extensions = 'Buttons',
              options = list(dom='Brtp',scrollX = TRUE, #pageLength = nrow(df),
                             buttons = list(
                               list(extend = 'csv',
                                    text = 'Export CSV',
                                    filename = 'Data LPA'
                               ),
                               list(extend = 'excel',
                                    text = 'Export Excel',
                                    filename = 'Data LPA'
                               ))),
      rownames = T) %>% 
      formatRound(columns = numeric_cols, digits = 2)
  }, server = FALSE)
  
  output$data_summary_lpa <- DT::renderDT({
    req(data_user(), input$selected_vars)
    
    df <- data_user() %>% 
      dplyr::select(all_of(input$selected_vars))
    
    table_summary <- data.frame(
      N        = sapply(df, function(x) sum(!is.na(x))),
      Missing  = sapply(df, function(x) sum(is.na(x))),
      Mean     = sapply(df, function(x) mean(x, na.rm = TRUE)),
      SD       = sapply(df, function(x) sd(x, na.rm = TRUE)),
      Variance = sapply(df, function(x) var(x, na.rm = TRUE)),
      Min      = sapply(df, function(x) min(x, na.rm = TRUE)),
      Max      = sapply(df, function(x) max(x, na.rm = TRUE))
    )
    
    # Transpose supaya Stat = baris, Variabel = kolom
    table_summary <- t(table_summary)
    # Round Mean, SD, Variance ke 2 desimal
    table_summary[c("Mean", "SD", "Variance","Min", "Max"), ] <- round(table_summary[c("Mean", "SD", "Variance", "Min", "Max"), ], 2)
    
    DT::datatable(
      table_summary,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        dom = "Bt",
        buttons = list(
          list(
            extend = "csv",
            text = "Export CSV",
            filename = "Data_LPA_Summary"
          ),
          list(
            extend = "excel",
            text = "Export Excel",
            filename = "Data_LPA_Summary"
          )
        )
      ),
      rownames = TRUE
    )
  }, server = FALSE)
 
  
  # --- Jalankan LPA ----
  observeEvent(input$run_lpa, {
    req(data_user(), input$selected_vars)
    updateTabsetPanel(session, "main_tab_lpa", selected = "fit_tab")
  })
  fitcompare <- eventReactive(input$run_lpa, {
    req(data_user(), input$selected_vars, input$model_to_run)
    showModal(modalDialog(title = NULL, "Please wait, running LPA...", footer = NULL, easyClose = FALSE))
    df <- data_user() %>% dplyr::select(input$selected_vars)
    df <- df %>% mutate(across(everything(), as.numeric))
    min_k <- input$min_profiles
    max_k <- input$max_profiles
    
    model_spec <- tibble::tibble(
      model = c(1, 2, 3, 6),
      variances = c("equal", "varying", "equal", "varying"),
      covariances = c("zero", "zero", "equal", "varying")
    )

    model_to_run <- input$model_to_run
    
    spec_selected <- model_spec %>%
      dplyr::filter(model %in% model_to_run) %>%
      dplyr::arrange(match(model, model_to_run))
    
    variances   <- spec_selected$variances
    covariances <- spec_selected$covariances
    
    withProgress(message = "Running...", {
      incProgress(1/(max_k-min_k+1), detail = paste("Compare LPA results"))
      fitcompare <- df %>%
        tidyLPA::single_imputation() %>%   # ← ini kuncinya
        tidyLPA::estimate_profiles(
          n_profiles = min_k:max_k,
          variances  = variances,
          covariances = covariances
          #model = model_to_run
        ) %>%
        tidyLPA::get_fit()
      fitcompare <- as.data.frame(fitcompare) %>% dplyr::rename(n_profiles=Classes)
      removeModal()
      showNotification("LPA completed successfully!", type = "message")
    })
    fitcompare
  })
  
  models <- eventReactive(c(input$best_k, input$model_type), {
    req(data_user(), fitcompare(),input$selected_vars, input$best_k)
    df <- data_user()[, input$selected_vars]
    df <- df %>% mutate(across(everything(), as.numeric))
    min_k <- input$min_profiles
    max_k <- input$max_profiles
    model_type <- input$model_type
    variances <- ifelse(input$model_type %in% c(1, 3), "equal", "varying")
    covariances <- ifelse(input$model_type %in% c(1,2), "zero",
                          ifelse(input$model_type %in% c(3), "equal", "varying"))
    model_list <- list()
    for (k in input$best_k) {
      withProgress(message = "Running...", {
      incProgress(1/(max_k-min_k+1), detail = paste("Estimate LPA Model for ", k, " Profiles"))
      model_list[[as.character(k)]] <- df %>%
        tidyLPA::single_imputation() %>%   # ← ini kuncinya
        tidyLPA::estimate_profiles(
          n_profiles = k,
          variances = variances,
          covariances = covariances
        )
      showNotification("Estimation completed successfully!", type = "message")
      })
    }
    model_list
   
  })
  
  # --- Fit table ----
  output$fit_table <- renderDT({
    req(fitcompare())
    fitcompare <- fitcompare() %>%
      dplyr::mutate(across(where(is.numeric), ~round(.x, 2))) %>%
      dplyr::select(-c(AWE, CAIC, CLC, KIC, SABIC, ICL))
    prob_min_vals <- sort(unique(fitcompare$prob_min), decreasing = TRUE)
    prob_max_vals <- sort(unique(fitcompare$prob_max),decreasing = TRUE)
    aic_vals <- sort(unique(fitcompare$AIC))
    bic_vals <- sort(unique(fitcompare$BIC))
    ent_vals <- sort(unique(fitcompare$Entropy), decreasing = TRUE)
    
    datatable(fitcompare, extensions = 'Buttons',
              options = list(dom='B', scrollX = TRUE, pageLength = 40,
                             buttons = list(
                               list(extend='csv', text='Export CSV', filename='FIT Comparison LPA'),
                               list(extend='excel', text='Export Excel', filename='FIT Comparison LPA')
                             )),
              rownames = FALSE) %>%
      formatStyle('AIC',
                  backgroundColor = styleEqual(
                    c(aic_vals[1], aic_vals[2]),
                    c('lightgreen', 'khaki')
                  ),
                  fontWeight = styleEqual(aic_vals[1], 'bold')
      ) %>%
      formatStyle('BIC',
                  backgroundColor = styleEqual(
                    c(bic_vals[1], bic_vals[2]),
                    c('lightgreen', 'khaki')
                  ),
                  fontWeight = styleEqual(bic_vals[1], 'bold')
      ) %>%
      formatStyle('Entropy',
                  backgroundColor = styleInterval(c(0.7, 0.8), c('lightcoral', 'khaki', 'lightgreen')),
                  fontWeight      = styleInterval(c(0.7, 0.8), c('bold', 'bold',''))
      ) %>%
      formatStyle('prob_min',
                  backgroundColor = styleEqual(
                    c(prob_min_vals[1], prob_min_vals[2]),
                    c('lightgreen', 'khaki')
                  ),
                  fontWeight = styleEqual(ent_vals[1], 'bold')
      ) %>%
      formatStyle('prob_max',
                  backgroundColor = styleEqual(
                    c(prob_max_vals[1], prob_max_vals[2]),
                    c('lightgreen', 'khaki')
                  ),
                  fontWeight = styleEqual(ent_vals[1], 'bold')
      ) %>%
      formatStyle('n_min',
                  backgroundColor = styleInterval(
                    c(0.05, 0.07),
                    c('lightcoral', 'khaki', 'lightgreen'))
                  ) %>%
      formatStyle('BLRT_p',
                  backgroundColor = styleInterval(0.05, c('lightgreen', 'lightcoral')),
                  fontWeight = styleInterval(0.05, c('bold', ''))
      )
  }, server = FALSE)
  fit_bic_reactive <- reactive({
    req(fitcompare())
    fit_bic <- fitcompare() %>%
      dplyr::select(Model, n_profiles, BIC) %>%
      dplyr::mutate(n_profiles = factor(n_profiles), Model = factor(Model))
    
    ggplot(fit_bic, aes(n_profiles, BIC, group = Model, shape = Model, color = Model)) +
      geom_line(size = 1.2, linetype = 4) + geom_point(size = 3) +
      geom_text(aes(label = round(BIC, 2)), vjust = -0.6, size = 4, show.legend = FALSE) +
      labs(subtitle  = "BIC Comparison Across Models", x = "Number of Profiles", y = "BIC") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", axis.line = element_line(color = "black"))
  })
  fit_aic_reactive <- reactive({
    req(fitcompare())
    fit_aic <- fitcompare() %>%
      dplyr::select(Model, n_profiles, AIC) %>%
      dplyr::mutate(n_profiles = factor(n_profiles), Model = factor(Model))
    
    ggplot(fit_aic, aes(n_profiles, AIC, group = Model, shape = Model, color = Model)) +
      geom_line(size = 1.2, linetype = 5) + geom_point(size = 3) +
      geom_text(aes(label = round(AIC, 2)), vjust = -0.6, size = 4, show.legend = FALSE) +
      labs(subtitle = "AIC Comparison Across Models", x = "Number of Profiles", y = "AIC") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", axis.line = element_line(color = "black"))
  })
  fit_entropy_reactive <- reactive({
    req(fitcompare())
    fit_entropy <- fitcompare() %>%
      dplyr::select(Model, n_profiles, Entropy) %>%
      dplyr::mutate(n_profiles = factor(n_profiles), Model = factor(Model))
    
    ggplot(fit_entropy, aes(n_profiles, Entropy, group = Model, shape = Model,color = Model)) +
      geom_line(size = 1.2, linetype = 2) + geom_point(size = 3) +
      geom_text(aes(label = round(Entropy, 2)), vjust = -0.6, size = 4, show.legend = FALSE) +
      labs(subtitle = "Entropy Comparison Across Models", x = "Number of Profiles", y = "Entropy") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", axis.line = element_line(color = "black"))
  })
  fit_class_size_reactive <- reactive({
    req(fitcompare())
    fit_class_size <- fitcompare() %>%
      dplyr::select(Model, n_profiles, n_min) %>%
      dplyr::mutate(n_profiles = factor(n_profiles), Model = factor(Model))
    
    ggplot(fit_class_size, aes(n_profiles, n_min, group = Model, shape = Model, color = Model)) +
      geom_line(size = 1.2, linetype = 1) + geom_point(size = 4) +
      geom_text(aes(label = round(n_min, 2)), vjust = -0.6, size = 4, show.legend = FALSE) +
      labs(subtitle = "Min Class Size Comparison Across Models", x = "Number of Profiles", y = "Minimum Class Size") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", axis.line = element_line(color = "black"))
  })
  
  # ==== Render Plot ====
  output$fit_bic <- renderPlot({
    fit_bic_reactive()
  })
  output$fit_aic <- renderPlot({
    fit_aic_reactive()
  })
  output$fit_entropy <- renderPlot({
    fit_entropy_reactive()
  })
  output$fit_class_size <- renderPlot({
    fit_class_size_reactive()
  })

  # ==== Download Buttons ====
  # output$download_plot_AicBic_LPA <- make_download_plot(
  #   plot_reactive = fit_plot_reactive,
  #   filename_prefix = "Fit AIC BIC Plot_LPA"
  # )
  # 
  # output$download_plot_entropy_LPA <- make_download_plot(
  #   plot_reactive = entropy_plot_reactive,
  #   filename_prefix = "EntropyPlot_LPA"
  # )
  # --- Input nama profil ---
  output$profile_name_inputs <- renderUI({
    req(input$best_k)
    lapply(1:input$best_k, function(i) {
      textInput(paste0("profile_name_", i), paste("Profile", i, "Name:"), value = paste("Profile", i))
    })
  })
  best_model_plot_reactive <- reactive({
    req(models(), input$best_k, input$selected_vars)
    
    k <- as.character(input$best_k)
    model_k <- models()[[k]]
    params  <- tidyLPA::get_estimates(model_k)
    
    # === Profile names (AMAN) ===
    profile_names <- sapply(1:input$best_k, function(i) {
      nm <- input[[paste0("profile_name_", i)]]
      if (is.null(nm) || nm == "") paste0("Profile ", i) else as.character(nm)
    })
    
    # === Ambil Means + SE ===
    plot_df <- params %>%
      dplyr::filter(
        Category == "Means",
        Parameter %in% input$selected_vars
      ) %>%
      dplyr::mutate(
        Profile = factor(
          Class,
          levels = seq_along(profile_names),
          labels = profile_names
        ),
        Variable = factor(Parameter, levels = input$selected_vars),
        Mean = Estimate,
        SE   = se
      )
    
    # === Plot ===
    ggplot(plot_df,
           aes(x = Variable, y = Mean,
               group = Profile,
               color = Profile,
               linetype = Profile,
               shape = Profile)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      
      # === Error bar (Mean ± SE) ===
      geom_errorbar(
        aes(ymin = Mean - SE, ymax = Mean + SE),
        width = 0.15,
        linewidth = 0.8
      ) +
      
      geom_text(
        aes(label = round(Mean, 2)),
        vjust = -0.9,
        size = 4,
        show.legend = FALSE
      ) +
      
      labs(
        title = paste0("Profile Plot Based on Model Estimates (K = ", k, ")"),
        x = "Indicator",
        y = "Estimated Mean (± SE)"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank()
      )
  })
  
  # RenderP lot
  output$best_model_plot <- renderPlot({
    best_model_plot_reactive()
  })
  
  # ==== Download Buttons ====
  output$download_plot_best_LPA <- make_download_plot(
    plot_reactive = best_model_plot_reactive,
    filename_prefix = "BestPlot_LPA"
  )
  #=======
  summary_data <- reactive({
    req(models(), input$best_k)
    
    model_k <- models()[[as.character(input$best_k)]]
    df_cls  <- get_data(model_k)
    params  <- tidyLPA::get_estimates(model_k)
  
    profile_names <- sapply(1:input$best_k, function(i) input[[paste0("profile_name_", i)]])
    df_cls$Profile <- factor(df_cls$Class, labels = profile_names)
    class_size <- df_cls %>%
          dplyr::count(Profile) %>%
          dplyr::mutate(Percent = round(100 * n / sum(n), 2))
    
    # === Mean & Variance (model-based) ===
    mean_var_tbl <- params %>%
      dplyr::filter(Category %in% c("Means", "Variances")) %>%
      dplyr::mutate(Profile = factor(Class, labels = profile_names)) %>%
      dplyr::select(Profile, Variable = Parameter, Category, Estimate) %>%
      tidyr::pivot_wider(
        names_from  = Category,
        values_from = Estimate
      ) %>%
      dplyr::mutate(
        MeanVar = sprintf("%.2f (%.2f)", Means, Variances)
      ) %>%
      dplyr::select(Profile, Variable, MeanVar)
    
    # === Bentuk tabel ringkas ===
    profile_summary <- mean_var_tbl %>%
      tidyr::pivot_wider(
        names_from  = Variable,
        values_from = MeanVar
      )
    
    # === Gabungkan dengan class size ===
    summary_table <- class_size %>%
      dplyr::left_join(profile_summary, by = "Profile")
    
    summary_table
  })
  # --- Render tabel ke UI ---
  output$summary_table <- renderDT({
    req(summary_data())
    df <- summary_data()
    DT::datatable(df,extensions = 'Buttons',
                  options = list(scrollX = TRUE, dom = 'B',
                                 buttons = list(
                                   list(
                                     extend = 'csv',
                                     text = 'Export CSV',
                                     filename = paste0('Summary LPA')
                                   ),
                                   list(
                                     extend = 'excel',
                                     text = 'Export Excel',
                                     filename = paste0('Summary LPA')
                                   ))),
                  rownames = FALSE)
  })

  
  df_out <- reactive({
    req(models(), input$best_k)
    df <- data_user()
    id_cols <- if (is.null(input$id_vars) || length(input$id_vars) == 0) "id_auto" else input$id_vars
    model_k <- models()[[as.character(input$best_k)]]
    cluster <- get_data(model_k)$Class
    profile_names <- sapply(1:input$best_k, function(i) input[[paste0("profile_name_", i)]])
    df$Profile <- factor(cluster, labels = profile_names)
    #df_out <- df[, c(id_cols, input$selected_vars, "Profile")]
    numeric_cols <- which(sapply(df_out, is.numeric))
    df
  })
  # --- Data profil per ID ---
  output$profile_table <- renderDT({
    req(models(), input$best_k)
    df <- df_out()
    id_cols <- if (is.null(input$id_vars) || length(input$id_vars) == 0) "id_auto" else input$id_vars
    df <- df[, c(id_cols, input$selected_vars, "Profile")]
    numeric_cols <- which(sapply(df, is.numeric))

    datatable(df,
              extensions = 'Buttons',
              options = list(dom='Brtp',scrollX = TRUE, pageLength = 10,
                             buttons = list(
                               list(extend = 'csv',
                                    text = 'Export CSV',
                                    filename = 'Data Profle LPA'
                               ),
                               list(extend = 'excel',
                                    text = 'Export Excel',
                                    filename = 'Data Profile LPA'
                               ))),
              rownames = T) %>%
      formatRound(columns = numeric_cols, digits = 2)
  }, server = FALSE)
  output$var_x_ui <- renderUI({
    req(df_out())
    selectInput(
      "var_x",
      label = "X Absis",
      choices = names(df_out()),
      selected = "Profile",
      multiple = FALSE 
    )
  })
  output$var_y_ui <- renderUI({
    req(df_out())
    selectInput(
      "var_y",
      label = "Y Absis",
      choices = setdiff(names(df_out()), "Profile"),
      selected = "",
      multiple = TRUE
    )
  })
  output$cross_latent <- renderPlot({
    req(models(), input$best_k, input$var_x, input$var_y, input$plot_type)
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
        Profile  = .data[[input$var_x]],
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
        fill = Profile,
        color = Profile,
        group = Profile
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
          aes(linetype = Profile),
          size = 1.2
        ) +
        geom_point(
          aes(shape = Profile),
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
        subtitle = paste0("Profile Plot for ", input$best_k, " Profiles"),
        x = "Variable",
        y = "Mean Value"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),
        legend.position = "bottom",
        axis.line = element_line(color = "black")
      )
  })
  output$anova_lpa_ui <- renderUI({
    req(models(), input$best_k, input$var_x, input$var_y)
    
    if (isTRUE(input$check_anova)) {
      tagList(
        tags$h5(
          paste0(
            "Latent Profile Differences on ",
            paste(input$var_y, collapse = ", ")
          )
        ),
        tags$h6("Analysis of Variance"),
        tableOutput("anova_lpa_table"),
        uiOutput("posthoc_lpa_ui")
      )
    }
  })
  output$anova_lpa_table <- renderTable({
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
  output$posthoc_lpa_ui <- renderUI({
    req(df_out(), input$var_x, input$var_y)
    
    if (length(input$var_y) == 1) {
      tagList(
        tags$h6("Post-hoc Test (Tukey HSD)"),
        tableOutput("posthoc_lpa_table")
      )
    }
  })
  output$posthoc_lpa_table <- renderTable({
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
  output$crosstab_ui <- renderUI({
    req(input$crosstab_result)
    if (!input$crosstab_result) return(NULL)
    
    req(df_out())
    vars <- setdiff(names(df_out()), "Profile")
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
        table(Profile = df$Profile, Category = df[[v]])
      )
      
      tab <- tab |>
        dplyr::group_by(Category) |>
        dplyr::mutate(
          Percent = round(Freq / sum(Freq) * 100, 2)
        ) |>
        dplyr::ungroup()
      
      data.frame(
        Profile  = tab$Profile,
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
        by = list(Profile = df$Profile),
        FUN = function(x)
          paste0(round(mean(x, na.rm = TRUE), 2),
                 " (", round(sd(x, na.rm = TRUE), 2), ")")
      ) |>
        setNames(c("Profile", v))
    })
    
    Reduce(function(x, y) merge(x, y, by = "Profile"), res)
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
      tab <- as.data.frame(table(Profile = df$Profile, Category = df[[v]]))
      
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
           aes(x = Category, y = Percent, fill = Profile)) +
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
        title = "Categorical Cross-Tabulation by Profile",
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
      dplyr::select(Profile, dplyr::all_of(vars_num)) |>
      tidyr::pivot_longer(
        cols = -Profile,
        names_to = "Variable",
        values_to = "Value"
      ) |>
      dplyr::group_by(Profile, Variable) |>
      dplyr::summarise(
        Mean = mean(Value, na.rm = TRUE),
        SD   = sd(Value, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(plot_data, aes(x = Profile, y = Mean, fill = Profile)) +
      geom_col(width = 0.6) +
      geom_errorbar(
        aes(ymin = Mean - SD, ymax = Mean + SD),
        width = 0.2
      ) +
      facet_wrap(~ Variable, scales = "free_y") +
      labs(
        title = "Continuous Variables by Profile",
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
  
  
  
  
  
  
  
  
  
  
  
}
