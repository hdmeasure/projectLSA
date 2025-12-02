# ==== server_lpa.R ====

server_lpa <- function(input, output, session) {
  library(tidyLPA)
  # --- Upload data ----
  data_user <- reactive({
    if (input$data_source == "pisaUSA15") {
      df <- tidyLPA::pisaUSA15 %>% mutate(id_auto = paste0("id_", sprintf("%04d", 1:n())))
    } else if (input$data_source == "curry_mac") {
      df <- tidyLPA::curry_mac %>% mutate(id_auto = paste0("id_", sprintf("%04d", 1:n())))
      
    } else if (input$data_source == "id_edu") {
      df <- tidyLPA::id_edu %>% mutate(id_auto = paste0("id_", sprintf("%04d", 1:n())))
      
    } else {
      req(input$datafile)
      ext <- tools::file_ext(input$datafile$name)
      df <- if (ext == "csv") read.csv(input$datafile$datapath) else read_excel(input$datafile$datapath)
      df <- df %>% mutate(across(everything(), ~ifelse(.x == "", NA, .x)))
      if (!"id" %in% tolower(names(df))) {
        df <- df %>% mutate(id_auto = sprintf("id_%04d", 1:n()))
        
      }
    }
    
    return(na.omit(df))
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
    } else {
      ""
    }
    
    div(
      style = "font-size: 13px; line-height: 1.4; color: #333;",
      HTML(desc_html)
    )
  })
  
  # ---  ID ---
  output$id_select_ui <- renderUI({
    req(data_user())
    selectizeInput(
      "id_vars",
      label = "Select ID Columns (Optional):",
      choices = names(data_user()),
      selected = names(data_user())[str_detect(names(data_user()), "id")],
      multiple = TRUE,
      options = list(placeholder = 'Choose one or more ID columns')
    )
  })
  
  # --- Pilih variabel ---
  output$var_select_ui <- renderUI({
    req(data_user())
    selectizeInput(
      "selected_vars",
      label = "Select Variables for LPA:",
      choices = names(data_user()),
      selected = names(data_user()%>% dplyr::select(-c(id_auto)))[1:min(5, ncol(data_user()))],
      multiple = TRUE,
      options = list(placeholder = 'Choose variables to include in analysis')
    )
  })
  # --- Data preview ----
  output$data_preview <- renderDT({
    req(data_user())
    df <- data_user() %>% 
      dplyr::select(input$selected_vars)
    numeric_cols <- which(sapply(df, function(x) is.numeric(x)))
    #numeric_cols <- which(vapply(df, function(x) is.numeric(x) && length(x) > 0, logical(1)))

#    datatable(df,
 #             extensions = 'Buttons',
  #            options = list(dom='Brtp',scrollX = TRUE, pageLength = 25,  
   #                          buttons = list(
    #                           list(extend = 'csv',
     #                               text = 'Export CSV',
      #                              filename = 'Data LPA'
       #                        ),
        #                       list(extend = 'excel',
         #                           text = 'Export Excel',
          #                          filename = 'Data LPA'
           #                    ))),
   #   rownames = T) %>% 
    #  formatRound(columns = numeric_cols, digits = 2)
  #numeric_cols <- which(vapply(df, is.numeric, logical(1)))

dt <- datatable(
  df,
  extensions = 'Buttons',
  options = list(
    dom = 'Brtp',
    scrollX = TRUE,
    pageLength = 25,
    buttons = list(
      list(extend = 'csv', text = 'Export CSV', filename = 'Data LPA'),
      list(extend = 'excel', text = 'Export Excel', filename = 'Data LPA')
    )
  ),
  rownames = TRUE
)

# Hanya formatRound jika benar-benar ada kolom numeric
if (length(numeric_cols) > 0) {
  dt <- dt %>% formatRound(columns = numeric_cols, digits = 2)
}

dt
  })

  # --- Jalankan LPA ----
  observeEvent(input$run_lpa, {
    req(data_user(), input$selected_vars)
    updateTabsetPanel(session, "main_tab_lpa", selected = "fit_tab")
  })
  fitcompare <- eventReactive(input$run_lpa, {
    req(data_user(), input$selected_vars)
    df <- data_user() %>% dplyr::select(input$selected_vars)
    df <- df %>% mutate(across(everything(), as.numeric))
    min_k <- input$min_profiles
    max_k <- input$max_profiles
    model_type <- input$model_type
    withProgress(message = "Running LPA Models...", value = 0, {
        fitcompare <- tidyLPA::estimate_profiles(df, n_profiles = min_k:max_k, models = 1) %>% 
          get_fit()
        fitcompare <- as.data.frame(fitcompare) %>% dplyr::rename(n_profiles=Classes)
        
      showNotification("LPA analysis completed successfully!", type = "message")
    })
    fitcompare
  })
  models <- eventReactive(c(input$run_lpa,input$best_k), {
    req(data_user(), input$selected_vars, input$best_k)
    df <- data_user()[, input$selected_vars]
    df <- df %>% mutate(across(everything(), as.numeric))
    min_k <- input$min_profiles
    max_k <- input$max_profiles
    model_type <- input$model_type
      model_list <- list()
      for (k in input$best_k) {
        model_list[[as.character(k)]] <- estimate_profiles(df, n_profiles = k, models = model_type)
      }
      model_list
  })
  
  # --- Fit table ----
  output$fit_table <- renderDT({
    req(models())
    #fit_stats <- purrr::map_df(models(), get_fit, .id = "n_profiles")
    fitcompare <- fitcompare()%>% 
      dplyr::mutate(across(LogLik:BLRT_p, ~round(.x, 3))) %>%
      dplyr::select(-c(AWE, CAIC, CLC, KIC, SABIC, ICL))
    min_AIC   <- min(fitcompare$AIC, na.rm = TRUE)
    max_Entropy <- max(fitcompare$Entropy, na.rm = TRUE)
    min_BIC   <- min(fitcompare$BIC, na.rm = TRUE)
    datatable(fitcompare,extensions = 'Buttons',
              options = list(dom='B',scrollX = TRUE, pageLength = 20,  
                             buttons = list(
                               list(extend = 'csv',
                                    text = 'Export CSV',
                                    filename = 'FIT Comparison LPA'
                               ),
                               list(extend = 'excel',
                                    text = 'Export Excel',
                                    filename = 'FIT Comparison LPA'
                               ))),
              rownames = FALSE) %>% 
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
        columns = 'Entropy',
        color = styleEqual(max_Entropy, 'black'),
        backgroundColor = styleEqual(max_Entropy, 'lightgreen'),
        fontWeight = styleEqual(max_Entropy, 'bold')
      ) %>% 
      formatStyle(
        columns = 'n_min',
        backgroundColor = styleInterval(0.07, c('lightcoral', 'lightgreen'))
      ) %>% 
      formatStyle(
        columns = 'BLRT_p',
        backgroundColor = styleInterval(0.05, c('lightgreen', 'lightcoral')),
        fontWeight = styleInterval(0.05, c('bold', '')),
        
      )
  })
  
  # --- Fit plot (AIC/BIC) ---
  fit_plot_reactive <- reactive({
    # req(models())
    req(fitcompare())
    
    # fit_stats <- map_df(models(), get_fit, .id = "n_profiles") %>%
    #   dplyr::select(n_profiles, AIC, BIC) %>% dplyr::mutate(n_profiles = as.numeric(n_profiles))
    fitcompare <- fitcompare() %>% 
      dplyr::select(n_profiles, AIC, BIC) %>% dplyr::mutate(n_profiles = as.numeric(n_profiles))
    fit_long <- fitcompare %>%
      pivot_longer(-n_profiles, names_to = "Index", values_to = "Value")
    
    ggplot(fit_long, aes(x = n_profiles, y = Value, color = Index, group = Index)) +
      geom_line(size = 1.2) + geom_point(size = 3) +
      geom_text(aes(label = round(Value, 2)), vjust = -0.6, size = 3) +
      labs(title = "Model Comparison (AIC & BIC)", x = "Number of Profiles", y = "Fit Index") +
      theme_minimal(base_size = 14) + 
      theme(legend.position = "bottom", axis.line = element_line(color = "black"))
  })
  
  # --- Entropy plot ---
  entropy_plot_reactive <- reactive({
    # req(models())
    req(fitcompare())
    
    stats <- fitcompare() %>% 
      dplyr::select(n_profiles, Entropy, n_min) %>% dplyr::mutate(n_profiles = as.numeric(n_profiles))
    # stats <- map_df(models(), get_fit, .id = "n_profiles")
    ggplot(stats, aes(x = as.numeric(n_profiles))) +
      geom_line(aes(y = Entropy, color = "Entropy"), size = 1.2) +
      geom_point(aes(y = Entropy, color = "Entropy"), size = 3) +
      geom_text(aes(y = Entropy, label = round(Entropy, 3)), vjust = -0.8, size = 3.5) +
      geom_line(aes(y = n_min, color = "Smallest Class Size"), size = 1.2) +
      geom_point(aes(y = n_min, color = "Smallest Class Size"), size = 3) +
      geom_text(aes(y = n_min, label = round(n_min, 3)), vjust = -0.8, size = 3.5) +
      labs(title = "Entropy and Smallest Class Size per Model", x = "Number of Profiles", y = "Value") +
      theme_minimal(base_size = 14) + 
      theme(legend.position = "bottom", axis.line = element_line(color = "black"))
    
  })
  
  # ==== Render Plot ====
  output$fit_plot <- renderPlot({
    fit_plot_reactive()
  })
  
  output$entropy_plot <- renderPlot({
    entropy_plot_reactive()
  })
  
  # ==== Download Buttons ====
  output$download_plot_AicBic_LPA <- make_download_plot(
    plot_reactive = fit_plot_reactive,
    filename_prefix = "Fit AIC BIC Plot_LPA"
  )
  
  output$download_plot_entropy_LPA <- make_download_plot(
    plot_reactive = entropy_plot_reactive,
    filename_prefix = "EntropyPlot_LPA"
  )
  
  # --- Input nama profil ---
  output$profile_name_inputs <- renderUI({
    req(input$best_k)
    lapply(1:input$best_k, function(i) {
      textInput(paste0("profile_name_", i), paste("Profile", i, "Name:"), value = paste("Profile", i))
    })
  })
  
  # --- Best model plot + summary ---
  best_model_plot_reactive <- reactive({
    req(models(), input$best_k, input$selected_vars)
    k <- as.character(input$best_k)
    model_k <- models()[[k]]
    df <- get_data(model_k)
    df$Cluster <- df$Class
    profile_names <- sapply(1:input$best_k, function(i) input[[paste0("profile_name_", i)]])
    df$Profile <- factor(df$Cluster, labels = profile_names)
    
    Summary <- df %>%
      group_by(Profile) %>%
      summarise(across(all_of(input$selected_vars), mean, na.rm = TRUE))
    
    Summary_long <- Summary %>%
      pivot_longer(cols = all_of(input$selected_vars), names_to = "Variable", values_to = "Mean")
    Summary_long$Variable <- factor(Summary_long$Variable, levels = input$selected_vars)
    
    ggplot(Summary_long, aes(x = Variable, y = Mean, group = Profile,
                             color = Profile, linetype = Profile, shape = Profile)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_text(aes(label = round(Mean, 2)), vjust = -0.7, size = 4.5) +
      labs(title = paste0("Best Plot LPA for ", k, " Profiles"), x = "Variable", y = "Mean Score", color = "Profile") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", axis.line = element_line(color = "black"))
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
  
  # --- Tabel ringkasan best model ---
  summary_data <- reactive({
    req(models(), input$best_k)
    
    model_k <- models()[[as.character(input$best_k)]]
    df <- get_data(model_k)
    df$Cluster <- df$Class
    profile_names <- sapply(1:input$best_k, function(i) input[[paste0("profile_name_", i)]])
    df$Profile <- factor(df$Cluster, labels = profile_names)
    
    # --- Ukuran kelas ---
    class_size <- df %>%
      count(Profile) %>%
      mutate(Percent = round(100 * n / sum(n), 2))
    
    # --- Hitung Mean dan SD lalu gabungkan jadi satu kolom ---
    summary_stats <- df %>%
      group_by(Profile) %>%
      summarise(across(
        all_of(input$selected_vars),
        list(Mean = ~mean(.x, na.rm = TRUE),
             SD   = ~sd(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      )) %>%
      rowwise() %>%
      dplyr::mutate(across(
        ends_with("_Mean"),
        ~sprintf("%.2f (%.2f)",
                 get(cur_column()),
                 get(str_replace(cur_column(), "_Mean", "_SD")))
      )) %>%
      dplyr::select(-ends_with("_SD")) %>%
      ungroup()
    
    # --- Ubah nama kolom menjadi "Mean_<variabel> (SD)" ---
    new_names <- names(summary_stats)
    new_names <- ifelse(
      grepl("_Mean$", new_names),
      paste0("Mean(SD)_", str_remove(new_names, "_Mean")),
      new_names
    )
    names(summary_stats) <- new_names
    
    # --- Gabungkan class size dan statistik ---
    summary_table <- left_join(class_size, summary_stats, by = "Profile")
    
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
  
  
  # --- Data profil per ID ---
  output$profile_table <- renderDT({
    req(models(), input$best_k)
    df <- data_user()
    id_cols <- if (is.null(input$id_vars) || length(input$id_vars) == 0) "id_auto" else input$id_vars
    model_k <- models()[[as.character(input$best_k)]]
    cluster <- get_data(model_k)$Class
    profile_names <- sapply(1:input$best_k, function(i) input[[paste0("profile_name_", i)]])
    df$Profile <- factor(cluster, labels = profile_names)
    df_out <- df[, c(id_cols, input$selected_vars, "Profile")]
    numeric_cols <- which(sapply(df_out, is.numeric))
    datatable(df_out, 
              extensions = 'Buttons',
              options = list(dom='Brtp',scrollX = TRUE, pageLength = 25,  
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
      formatRound(columns = numeric_cols, digits = 3)
  })
   
}
