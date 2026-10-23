server_efa <- function(input, output, session, ai_context, console_context) {
  # text typed (or drafted by the assistant) in the Report form
  report_notes <- server_report_form(input, output, session, "efa", ai_context, method_ctx = function() { req(efa_result()); list(fa = efa_result()$fa) })

  library(psych)
  library(lavaan)
  
  # ==== LOAD DATA ====
  raw_data_user <- reactive({
    req(input$data_source)
    if (input$data_source == "bfi") return(psych::bfi %>% dplyr::select(A1:O5)%>% tibble::rownames_to_column("id_auto"))
    if (input$data_source == "HolzingerSwineford1939") return(lavaan::HolzingerSwineford1939%>% dplyr::select(x1:x9)%>% tibble::rownames_to_column("id_auto"))
    if (input$data_source == "upload") {
      req(input$datafile)
      ext <- tolower(tools::file_ext(input$datafile$name))
      showModal(modalDialog(title = NULL, "Reading Your File, Please wait...", footer = NULL, easyClose = FALSE))
      if (ext == "rds") {
        res <- readRDS(input$datafile$datapath)
        if (is.list(res) && !is.data.frame(res) && identical(res$type, "projectLSA_workspace")) {
          if (identical(res$module, "EFA")) {
            # Restore Workspace State
            efa_aggregations(res$efa_aggregations)
            efa_result(res$efa_result)
            df <- res$raw_data
            showNotification("EFA Workspace restored successfully!", type = "message")
          } else {
            stop("Uploaded workspace belongs to a different module: ", res$module)
          }
        } else if (is.list(res) && "fa" %in% names(res) && inherits(res$fa, "fa")) {
          efa_result(res)
          vars <- rownames(res$fa$loadings)
          df <- as.data.frame(matrix(NA, nrow=1, ncol=length(vars)))
          colnames(df) <- vars
          showModal(modalDialog("EFA Model uploaded successfully! Please navigate to the Summary or Plot tab.", easyClose = TRUE))
        } else if (inherits(res, "fa")) {
          efa_result(list(fa = res, cor_mat = NULL, fit_stats = NULL, n_obs = NULL))
          vars <- rownames(res$loadings)
          df <- as.data.frame(matrix(NA, nrow=1, ncol=length(vars)))
          colnames(df) <- vars
          showModal(modalDialog("EFA Model uploaded successfully! Please navigate to the Summary or Plot tab.", easyClose = TRUE))
        } else {
          df <- res
        }
      } else {
        df <- switch(
          ext,
          "csv"  = data.table::fread(input$datafile$datapath, data.table = FALSE),
          "xls"  = readxl::read_excel(input$datafile$datapath),
          "xlsx" = readxl::read_excel(input$datafile$datapath),
          "sav"  = haven::read_sav(input$datafile$datapath),
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
  
  # Reactive value to store user-defined aggregations
  efa_aggregations <- reactiveVal(list())
  
  # New data_user reactive that applies aggregations on top of raw_data_user
  data_user <- reactive({
    df <- raw_data_user()
    req(df)
    
    aggs <- efa_aggregations()
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
  observeEvent(input$efa_btn_aggregate, {
    req(input$efa_agg_vars, input$efa_agg_name)
    
    # Check if name is valid and doesn't exist already
    new_name <- make.names(input$efa_agg_name)
    
    new_agg <- list(
      vars = input$efa_agg_vars,
      name = new_name,
      method = input$efa_agg_method
    )
    
    # Append to existing aggregations
    current_aggs <- efa_aggregations()
    current_aggs[[new_name]] <- new_agg
    efa_aggregations(current_aggs)
    
    showNotification(paste("Aggregated variable", new_name, "created using", input$efa_agg_method, "method."), type = "message")
    
    # Clear inputs
    updateTextInput(session, "efa_agg_name", value = "")
    updateSelectizeInput(session, "efa_agg_vars", selected = "")
  })
  
  # Handle reset aggregation button
  observeEvent(input$efa_btn_reset_agg, {
    efa_aggregations(list())
    showNotification("All aggregations have been reset.", type = "warning")
  })
  
  # Populate aggregation UI choices
  observe({
    req(raw_data_user())
    # We populate with raw data columns + existing aggregated columns
    df_names <- names(data_user())
    updateSelectizeInput(session, "efa_agg_vars", choices = df_names)
  })
  

  # ==== Pilih ID ====
  output$id_select_ui_fa <- renderUI({
    req(data_user())
    shinyWidgets::pickerInput(
      "id_lca",
      label = "Select ID Columns (Optional):",
      choices = names(data_user()),
      selected = names(data_user())[str_detect(names(data_user()), "id")],
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, placeholder = 'Choose one or more ID columns')
    )
  })
  
  # ==== Pilih Variabel (selain ID) ====
  output$var_select_ui <- renderUI({
    req(data_user())
    all_vars <- names(data_user())
    id_cols <- input$id_lca
    
    # Hilangkan kolom ID dari pilihan variabel
    available_vars <- setdiff(all_vars, id_cols)
    shinyWidgets::pickerInput(
      "selected_vars",
      label = "Select Variables:",
      choices = available_vars,
      selected = available_vars,  # default pilih semua yang tersisa
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 3")
    )
  })

  output$data_preview_fa <- renderDT({
    req(data_user(),input$selected_vars)
    df <- data_user()[,input$selected_vars]
    df <- df %>%
      dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))
    
    DT::datatable(
      head(df, 50),extensions = 'Buttons',
      options = list(dom='Brtp',scrollX = TRUE, pageLength = 25,  
                     buttons = list(
                       list(extend = 'csv',
                            text = 'Export CSV',
                            filename = 'Data EFA'
                       ),
                       list(extend = 'excel',
                            text = 'Export Excel',
                            filename = 'Data EFA'
                       ))),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # ====== Data Preview Visualizations ======
  output$efa_summary_table <- renderUI({
    vars_to_use <- input$selected_vars
    if (is.null(vars_to_use) || length(vars_to_use) == 0) vars_to_use <- names(data_user())
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

  output$efa_data_summary_plot <- renderPlot({
    vars_to_use <- input$selected_vars
    if (is.null(vars_to_use) || length(vars_to_use) == 0) vars_to_use <- names(data_user())
    if (is.null(vars_to_use) || length(vars_to_use) == 0) vars_to_use <- names(data_user())

    valid_vars <- intersect(vars_to_use, names(data_user()))
    if (length(valid_vars) == 0) {
      return(NULL)
    }

    df <- data_user()[, valid_vars, drop = FALSE]

    types <- sapply(df, function(x) class(x)[1])
    type_df <- as.data.frame(table(types))
    names(type_df) <- c("DataType", "Count")
    
    theme_choice <- input$efa_summary_theme
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

  output$efa_data_heatmap <- renderPlot({
    vars_to_use <- input$selected_vars
    if (is.null(vars_to_use) || length(vars_to_use) == 0) vars_to_use <- names(data_user())
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
    cor_matrix[upper.tri(cor_matrix)] <- NA

    cor_melted <- as.data.frame(as.table(cor_matrix))
    cor_melted <- cor_melted[!is.na(cor_melted$Freq), ]
    names(cor_melted) <- c("Var1", "Var2", "value")

    theme_choice <- input$efa_summary_theme
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
  
  # === Ketika user memilih CFA ===
  observeEvent(input$fa_type, {
    if (input$fa_type == "cfa") {
      message("Running CFA server...")
      cfa_server(input, output, session, data_user = data_user)
    }
  })
  
  
  
  
  # ==== Dynamic input nama faktor ====
  output$factor_names_ui <- renderUI({
    req(input$n_factors)
    n <- input$n_factors
    div(
      style = "
      display: flex;
      justify-content: center;
      flex-wrap: wrap;
      gap: 8px;
      margin-top: 5px;
    ",
      
      lapply(1:n, function(i) {
        textInput(
          inputId = paste0("factor_name_", i),
          label = NULL,
          value = paste0("Factor ", i),
          width = "100px"
        )
      }),
      br(),
    )
  })
  
  # ==== EFA ====
  efa_result <- reactiveVal(NULL)
  
  observeEvent(c(input$run_efa, input$n_factors), {
    req(data_user(), input$selected_vars)
    req(input$run_efa > 0 || !is.null(efa_result())) # Only run if button clicked or already loaded
    
    withProgress(message = "Running Exploratory Factor Analysis (EFA)...", value = 0, {
      incProgress(0.1, detail = "Preparing data...")
      
      # a variable list left over from another data set must not stop the app
      vars_ok <- intersect(input$selected_vars, names(data_user()))
      req(length(vars_ok) > 0)
      df <- as.data.frame(data_user())[ , vars_ok, drop = FALSE]
      
      # ensure numeric columns
      nonnum <- which(!sapply(df, is.numeric))
      if (length(nonnum) > 0) {
        df[nonnum] <- lapply(df[nonnum], function(x) as.numeric(as.character(x)))
      }
      
      # handle missing
      if (input$missing_method_efa == "mean") {
        for (j in seq_along(df)) df[is.na(df[, j]), j] <- mean(df[, j], na.rm = TRUE)
        missing_arg <- "no"
      } else if (input$missing_method_efa == "pairwise") {
        missing_arg <- "pairwise"
      } else {
        df <- na.omit(df)
        missing_arg <- "listwise"
      }
      
      incProgress(0.3, detail = "Computing KMO and Bartlett tests...")
      # compute KMO and Bartlett
      kmo_res <- tryCatch(psych::KMO(df), error = function(e) NULL)
      bartlett_res <- tryCatch(
        psych::cortest.bartlett(cor(df, use = "pairwise.complete.obs"), n = nrow(df)),
        error = function(e) NULL
      )
      
      incProgress(0.6, detail = "Performing Parallel Analysis...")
      par <- tryCatch(
        psych::fa.parallel(df, n.iter = 20, main = "Parallel Analysis (fa.parallel)"),
        error = function(e) NULL
      )
      
      incProgress(0.9, detail = paste("Extracting", input$n_factors, "factors..."))
      fa_res <- tryCatch(
        psych::fa(df, nfactors = input$n_factors, rotate = input$rotation_method),
        error = function(e) NULL
      )
      
      incProgress(1, detail = "Finalizing results...")
      
      efa_out <- list(
        kmo = kmo_res,
        bartlett = bartlett_res,
        parallel = par,
        fa = fa_res,
        loadings = if (!is.null(fa_res)) fa_res$loadings else NULL,
        df_used = df,
        scores = as.data.frame(psych::factor.scores(df, fa_res, Phi = NULL,rho=NULL,missing=FALSE,impute="mean")$scores)
      )
      
      efa_result(efa_out)
    })
  }, ignoreInit = TRUE)

  
  # ==== Pindah ke tab hasil EFA setelah dijalankan ====
  observeEvent(input$run_efa, {
    updateTabsetPanel(session, "main_tab_fa", selected = "KMO & Bartlett Tests")
  })
  
  
  # ==== Tampil hasil tes EFA dengan gaya HTML ====
  output$efa_tests <- renderUI({
    req(efa_result())
    out <- efa_result()
    n_fact <- if (!is.null(out$parallel$nfact)) out$parallel$nfact else NA
    n_comp <- if (!is.null(out$parallel$ncomp)) out$parallel$ncomp else NA
    bart <- out$bartlett
    kmo <- out$kmo
    verdict <- function(txt, tone) tags$span(class = "pl-verdict", `data-tone` = tone, txt)

    rows <- list(
      tags$tr(tags$td(class = "pl-lbl", "Parallel analysis"),
              tags$td(class = "pl-l", sprintf("%s factors, %s components", n_fact, n_comp)), tags$td(),
              tags$td(class = "pl-l", "Number of factors whose eigenvalues exceed those of random data."))
    )
    if (!is.null(kmo)) {
      msa <- kmo$MSA
      lab <- if (msa >= .90) c("Marvelous", "ok", "The data are highly suitable for factor analysis.")
        else if (msa >= .80) c("Meritorious", "ok", "The data are suitable for factor analysis.")
        else if (msa >= .70) c("Middling", "ok", "Acceptable for factor analysis.")
        else if (msa >= .60) c("Mediocre", "warn", "Factor analysis may still be appropriate.")
        else if (msa >= .50) c("Miserable", "warn", "Consider improving data quality or sample size.")
        else c("Unacceptable", "bad", "Factor analysis is not recommended.")
      rows[[length(rows) + 1]] <- tags$tr(tags$td(class = "pl-lbl", "Kaiser-Meyer-Olkin (KMO)"),
        tags$td(class = "pl-l", sprintf("overall MSA = %.3f", msa)), tags$td(verdict(lab[1], lab[2])), tags$td(class = "pl-l", lab[3]))
    }
    if (!is.null(bart)) {
      pval <- bart$p.value; sig <- !is.na(pval) && pval < 0.05
      rows[[length(rows) + 1]] <- tags$tr(tags$td(class = "pl-lbl", "Bartlett's test of sphericity"),
        tags$td(class = "pl-l", HTML(sprintf("&chi;&sup2;(%s) = %s, <i>p</i> %s", bart$df, format(round(bart$chisq, 2), big.mark = ","),
                                       if (pval < .001) "&lt; .001" else paste0("= ", sub("^0", "", sprintf("%.3f", pval)))))),
        tags$td(verdict(if (sig) "Significant" else "Not significant", if (sig) "ok" else "bad")),
        tags$td(class = "pl-l", if (sig) "The correlation matrix is not an identity matrix, so factor analysis is appropriate."
                                 else "The correlation matrix is close to an identity matrix; factor analysis may not suit these data."))
    }
    tags$section(class = "pl-flat",
      tags$h4(class = "pl-flat-title", "Is the data suitable for factor analysis?"),
      tags$div(class = "pl-table-wrap",
        tags$table(class = "pl-table pl-table-text",
          tags$thead(tags$tr(tags$th("Test"), tags$th("Result"), tags$th("Verdict"), tags$th("Reading"))),
          tags$tbody(rows))))
  })

  # Sampling adequacy per item: a compact table with in-cell bars (easier to read and to copy than a lollipop plot)
  output$kmo_item_tbl <- renderUI({
    req(efa_result())
    kmo <- efa_result()$kmo
    req(!is.null(kmo))
    d <- data.frame(Item = c("Overall", names(kmo$MSAi)), MSA = c(kmo$MSA, as.numeric(kmo$MSAi)), stringsAsFactors = FALSE)
    cat_of <- function(x) if (x >= .9) c("Marvelous", "ok") else if (x >= .8) c("Meritorious", "ok") else if (x >= .7) c("Middling", "ok")
      else if (x >= .6) c("Mediocre", "warn") else if (x >= .5) c("Miserable", "warn") else c("Unacceptable", "bad")
    rows <- lapply(seq_len(nrow(d)), function(i) {
      cl <- cat_of(d$MSA[i])
      tags$tr(class = if (i == 1) "pl-row-total" else NULL,
              tags$td(class = "pl-lbl", d$Item[i]), tags$td(sprintf("%.3f", d$MSA[i])),
              tags$td(class = "pl-barcell", tags$span(class = "pl-bar", `data-tone` = cl[2], tags$i(style = sprintf("width:%d%%", round(max(0, min(1, d$MSA[i])) * 100))))),
              tags$td(class = "pl-l", cl[1]))
    })
    tags$div(class = "pl-table-wrap",
      tags$table(class = "pl-table",
        tags$thead(tags$tr(tags$th("Item"), tags$th("MSA"), tags$th(""), tags$th("Category"))),
        tags$tbody(rows)),
      tags$p(class = "pl-stat-note", "Items below .50 are candidates for removal; .50 to .69 is weak, .70 and above is acceptable."))
  })
  
  
  # ==== Plot KMO per Item (Lollipop Chart) ====
  output$kmo_item <- renderPlot({
    req(efa_result())
    out <- efa_result()
    kmo_res <- out$kmo
    req(!is.null(kmo_res))
    # Ambil nilai MSA per item (measure of sampling adequacy)
    kmo_values <- kmo_res$MSAi
    MSA_all <- data.frame(Item = 'Overall', KMO= kmo_res$MSA)
    kmo_data <- data.frame(
      Item = names(kmo_values),
      KMO = as.numeric(kmo_values)
    ) %>% rbind(MSA_all)
    
    # Tambahkan kategori kualitas KMO
    kmo_data <- kmo_data %>%
      dplyr::mutate(Kategori = dplyr::case_when(
        KMO < 0.5 ~ "Unacceptable",
        KMO < 0.6 ~ "Miserable",
        KMO < 0.7 ~ "Mediocre",
        KMO < 0.8 ~ "Meritorious",
        TRUE ~ "Marvelous"
      ))
    
    # Skema warna kategori
    kategori_colors <- c(
      "Unacceptable" = "#d73027",
      "Miserable"    = "#fc8d59",
      "Mediocre"     = "#fee08b",
      "Meritorious"  = "#4575b4",
      "Marvelous"    = "blue"
    )
    kmo_data$Item <- factor(kmo_data$Item, levels = kmo_data$Item)
    
    # Plot lollipop
    ggplot(kmo_data, aes(x = KMO, y = Item)) +
      geom_segment(aes(x = 0, xend = KMO, yend = Item, color = Kategori), linewidth = 1.5) +
      geom_point(aes(color = Kategori), size = 4) +
      # === Tambahkan label nilai di ujung ===
      geom_text(aes(label = round(KMO, 2)), 
                hjust = -0.3,  # geser sedikit ke kanan
                size = 3, 
                color = "black") +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1) +
      scale_color_manual(values = kategori_colors) +
      labs(
        title = "Kaiser-Meyer-Olkin (KMO) per Item",
        subtitle = "Measure of Sampling Adequacy (MSA)",
        x = "MSA Value",
        y = "Item",
        color = "Category"
      ) +
      xlim(0, 1) +
      theme_pl(13) +
      theme(axis.text.y = element_text(face = "bold"), panel.grid.major.x = element_line(color = "#e7e5f4", linewidth = 0.4))
  })
  
  # Scree plot behaviour:
  output$scree_plot <- renderPlot({
    req(efa_result())
    out <- efa_result()
    df <- out$df_used
    invisible(capture.output(
      psych::fa.parallel(df, fm = "ml", n.iter = 20, main = "Parallel Analysis (fa.parallel)")
    ))    
  })

  # ==== Output Ringkasan Hasil FA (Centered) ====
  output$efa_summary <- renderUI({
    req(efa_result())
    out <- efa_result()
    
    if (is.null(out$fa)) {
      return(tags$p("No factor analysis result available.", style = "color: #999; font-style: italic; text-align:center;"))
    }
    
    fa <- out$fa
    n_factors <- fa$factors
    method <- fa$method
    rotation <- fa$rotation
    total_var <- sum(fa$Vaccounted["Proportion Var", ]) * 100
    
    # --- Ambil nama faktor dari input teks ---
    factor_names <- sapply(1:n_factors, function(i) {
      input[[paste0("factor_name_", i)]] %||% paste0("Factor ", i)
    })
    
    # --- Matriks loading ---
    L <- as.matrix(fa$loadings)
    if (is.null(colnames(L))) colnames(L) <- paste0("Factor", seq_len(ncol(L)))
    if (length(factor_names) == ncol(L)) colnames(L) <- factor_names
    rown <- rownames(L)
    
    # Tentukan loading terbesar per item
    absmat <- abs(L)
    max.idx <- apply(absmat, 1, function(x) if (all(is.na(x))) NA_integer_ else which.max(x))
    
    # --- loadings table: the largest loading of each item is highlighted ---
    hdr <- tags$tr(tags$th("Item"), lapply(colnames(L), tags$th))
    body <- lapply(seq_len(nrow(L)), function(i) {
      tags$tr(tags$td(class = "pl-lbl", rown[i]), lapply(seq_len(ncol(L)), function(j) {
        val <- L[i, j]
        txt <- if (is.na(val)) "" else sprintf("%.3f", val)
        is_max <- !is.na(max.idx[i]) && j == max.idx[i] && abs(val) > 0
        tags$td(class = if (is_max) "is-max" else NULL, txt)
      }))
    })
    tbl <- tags$table(class = "pl-table", tags$thead(hdr), tags$tbody(body))

    # --- variance explained ---
    variance_df <- as.data.frame(round(fa$Vaccounted * 100, 2))
    variance_df <- tibble::rownames_to_column(variance_df, var = "Metric")
    if (length(factor_names) == ncol(variance_df) - 1) colnames(variance_df)[-1] <- factor_names
    vt <- tags$table(class = "pl-table",
      tags$thead(tags$tr(lapply(colnames(variance_df), tags$th))),
      tags$tbody(lapply(seq_len(nrow(variance_df)), function(i)
        tags$tr(tags$td(class = "pl-lbl", variance_df[i, 1]), lapply(variance_df[i, -1], function(x) tags$td(sprintf("%.2f", x)))))))

    chip <- function(k, v) tags$span(class = "pl-chip-kv", tags$small(k), tags$b(v))
    tagList(
      tags$div(class = "pl-chips",
               chip("Extraction", method), chip("Rotation", rotation), chip("Factors", n_factors), chip("Total variance", sprintf("%.1f%%", total_var))),
      tags$h5(class = "pl-sub", "Factor loadings"),
      tags$p(class = "pl-stat-note", "The largest loading of each item is shaded."),
      tags$div(class = "pl-table-wrap", tbl),
      tags$h5(class = "pl-sub", "Variance explained (%)"),
      tags$div(class = "pl-table-wrap", vt)
    )
  })
  
  # =====Factor Scores EFA =====
  output$efa_scores_table <- DT::renderDataTable({
    req(efa_result())
    out <- efa_result()
    
    if (is.null(out$fa)) {
      return(tags$p("No factor analysis result available.", style = "color: #999; font-style: italic; text-align:center;"))
    }
    n_factors <- out$fa$factors
    
    # --- Ambil nama faktor dari input teks ---
    factor_names <- sapply(1:n_factors, function(i) {
      input[[paste0("factor_name_", i)]] %||% paste0("Factor ", i)
    })
    
    scores <- efa_result()$scores
    if (length(factor_names) == ncol(scores))
      colnames(scores) <- factor_names
    datatable(
      round(as.data.frame(scores), 3),extensions = 'Buttons',
      options = list(dom='Brtp',scrollX = TRUE, pageLength = 25,  
                     buttons = list(
                       list(extend = 'csv',
                            text = 'Export CSV',
                            filename = 'Factor Scores EFA'
                       ),
                       list(extend = 'excel',
                            text = 'Export Excel',
                            filename = 'Factor Scores EFA'
                       ))),
      caption = "Estimated Factor Scores for Each Observation"
    )
  })
  # ==== 3. DOWNLOAD BUTTONS ====
  output$download_loading <- downloadHandler(
    filename = function() {
      paste0("efa_loadings_", Sys.Date(), ".csv")
    },
    content = function(file) {
      res <- efa_result()
      load_tab <- as.data.frame(round(res$fa$loadings[, ], 3))
      load_tab$Item <- rownames(res$fa$loadings)
      load_tab <- load_tab[, c(ncol(load_tab), 1:(ncol(load_tab)-1))]
      write.csv(load_tab, file, row.names = FALSE)
    }
  )
  
  output$download_scores <- downloadHandler(
    filename = function() {
      paste0("efa_factor_scores_", Sys.Date(), ".csv")
    },
    content = function(file) {
      scores <- efa_result()$scores
      write.csv(round(as.data.frame(scores), 3), file, row.names = FALSE)
    }
  )

  # ==== 4. R Console Output & Model Export ====
  observeEvent(efa_result(), {
    req(efa_result())
    out_res <- efa_result()
    if (!is.null(out_res$fa)) {
      out_text <- paste(capture.output(print(out_res$fa)), collapse = "\n")
      console_context$text <- out_text
    } else {
      console_context$text <- "Error in EFA model computation."
    }
  })
  
  output$export_efa_rds <- downloadHandler(
    filename = function() {
      paste0("EFA_Workspace_", Sys.Date(), ".rds")
    },
    content = function(file) {
      req(efa_result())
      workspace <- list(
        type = "projectLSA_workspace",
        module = "EFA",
        raw_data = raw_data_user(),
        efa_result = efa_result(),
        efa_aggregations = efa_aggregations()
      )
      saveRDS(workspace, file)
    }
  )

  # ==== 5. Score New Data ====
  output$download_efa_template <- downloadHandler(
    filename = function() { "EFA_template.xlsx" },
    content = function(file) {
      req(efa_result())
      items <- rownames(efa_result()$fa$loadings)
      df <- data.frame(matrix(ncol = length(items), nrow = 0))
      colnames(df) <- items
      writexl::write_xlsx(df, file)
    }
  )

  efa_newscores_reactive <- eventReactive(input$efa_score_newdata_btn, {
    req(efa_result(), input$efa_newdata)
    ext <- tools::file_ext(input$efa_newdata$name)
    df <- switch(
      ext,
      "csv" = read.csv(input$efa_newdata$datapath),
      "xlsx" = readxl::read_excel(input$efa_newdata$datapath),
      "xls" = readxl::read_excel(input$efa_newdata$datapath),
      stop("Invalid file format")
    )
    
    # Apply user-defined calculated variables to the new data
    aggs <- efa_aggregations()
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
    
    # Calculate scores
    fa_model <- efa_result()$fa
    # Make sure we only use the columns matching the items in the model
    items <- rownames(fa_model$loadings)
    df_used <- df[, items, drop = FALSE]
    
    scores <- psych::factor.scores(df_used, fa_model, missing = FALSE, impute = "mean")$scores
    
    n_factors <- fa_model$factors
    factor_names <- sapply(1:n_factors, function(i) {
      input[[paste0("factor_name_", i)]] %||% paste0("Factor ", i)
    })
    
    if (length(factor_names) == ncol(scores)) {
      colnames(scores) <- factor_names
    }
    
    as.data.frame(scores)
  })

  output$efa_newscores_table <- DT::renderDataTable({
    req(efa_newscores_reactive())
    datatable(round(efa_newscores_reactive(), 3), options = list(scrollX = TRUE))
  })

  output$download_efa_newscores <- downloadHandler(
    filename = function() { paste0("EFA_newscores_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(round(efa_newscores_reactive(), 3), file, row.names = FALSE)
    }
  )
  # ==== AI Assistant ====
  # Update global AI context whenever results change
  observe({
    res_text <- ""
    if (!is.null(efa_result())) {
      fit <- efa_result()
      res_text <- paste(capture.output(print(fit)), collapse = "\n")
    }
    ai_context$results_text <- res_text
    ai_context$module <- "Exploratory Factor Analysis (EFA)"
  })

  # Per-session directory, so concurrent users never share a report file
  efa_report_res <- session_report_dir(session, "efa")
  efa_report_path <- reactiveVal(NULL)
  
  # Renders the EFA report and returns the path of the generated HTML file
  # (NULL on failure). Shared by the preview button and the download handler.
  generate_efa_report <- function(progress_label = "Generating Report...") {
    if (is.null(efa_result())) {
      return(NULL)
    }

    # The template that sits next to the app wins; an installed copy is only a fallback
    report_path <- "efa_report.Rmd"
    if (!file.exists(report_path)) report_path <- file.path(system.file("app", package = "projectLSA"), "efa_report.Rmd")
    
    tempReport <- file.path(efa_report_res$path, "efa_report.Rmd")
    file.copy(report_path, tempReport, overwrite = TRUE)
    
    out_html <- file.path(efa_report_res$path, "efa_report_out.html")
    
    showModal(modalDialog(progress_label, footer = NULL))
    ok <- tryCatch({
      rmarkdown::render(tempReport, output_file = out_html,
        params = list(
          efa_res = efa_result(),
          console_out = console_context$text,
          notes = report_notes(),
          ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text
        ),
        envir = new.env(parent = globalenv())
      )
      efa_report_path(out_html)
      TRUE
    }, error = function(e) {
      showNotification(paste("Error rendering report:", e$message), type = "error")
      FALSE
    }, finally = {
      removeModal()
    })

    if (isTRUE(ok)) out_html else NULL
  }

  observeEvent(input$efa_generate_preview, {
    req(efa_result())
    generate_efa_report("Generating Report Preview...")
  })

  output$efa_report_preview_frame <- renderUI({
    req(efa_report_path())
    tags$iframe(
      src = paste0(efa_report_res$prefix, "/efa_report_out.html?v=", as.integer(Sys.time())),
      width = "100%", height = "800px", style = "border: none;"
    )
  })

  output$download_report_efa <- downloadHandler(
    filename = function() {
      paste0("EFA_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(efa_result())
      path <- efa_report_path()
      if (is.null(path) || !file.exists(path)) {
        path <- generate_efa_report("Generating Report...")
      }
      req(path)
      file.copy(path, file, overwrite = TRUE)
    },
    contentType = "text/html"
  )

  output$download_word_efa <- downloadHandler(
    filename = function() paste0("EFA_Report_", Sys.Date(), ".docx"),
    content = function(file) {
      req(efa_result())
      withProgress(message = "Building the Word report", value = 0.4, {
        render_word_report("efa_report_word.Rmd", list(
          efa_res = efa_result(), console_out = console_context$text, notes = report_notes(),
          ai_summary = if (is.null(ai_context$ai_report_text)) "" else ai_context$ai_report_text,
          dec = (input$dec_sep %||% ".")
        ), file, file.path(efa_report_res$path, "word"))
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
}
