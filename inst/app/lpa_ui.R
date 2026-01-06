# Fungsi untuk menu Siswa
lpa_ui <- function(project) {
  tabsetPanel(
    id = "main_tab_lpa",
    
    # --- TAB 1: Prepare Data & Model ----
    tabPanel(
      title = tagList(icon("upload"), "Prepare Data & Model"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # Tombol kembali ke halaman utama
            actionButton("go_home",
                         label = tagList(icon("home"), "Main Menu"), 
                         class = "btn btn-danger btn-block",
                         style = "width: 100% !important;"),
            br(),
          selectInput(
            "data_source", 
            "Select Data Source:",
            choices = c("Upload Data" = "upload",
                        "Built-in dataset: pisaUSA15" = "pisaUSA15",
                        "Built-in dataset: curry_mac" = "curry_mac",
                        "Built-in dataset: id_edu" = "id_edu"
                        ),
            selected = "upload"
          ),
          conditionalPanel(
            condition = "input.data_source == 'upload'",
            fileInput("datafile", "Upload Data (csv/xlsx)", accept = c(".csv", ".xlsx"))
          ),
          #fileInput("datafile", "Upload Data (csv/xlsx)", accept = c(".csv", ".xlsx")),
          uiOutput("id_select_ui"),
          uiOutput("var_select_ui"),
          numericInput("min_profiles", "Min Number of Profiles:", 2, min = 1),
          numericInput("max_profiles", "Max. Number of Profiles:", 10, min = 2),
          
          actionButton("run_lpa", 
                       label = tagList(icon("play"), "Run LPA"), 
                       class = "btn btn-success btn-block",
                       style = "width: 100% !important;")
          
          
        ),
        mainPanel(
          width = 9,
          
          h5(icon("table"), "Data Preview"),
          DTOutput("data_preview"),
          br(),
          uiOutput("data_description"),
          
          br(),
        )
      )
    ),
    
    # --- TAB 2: Fit Model Comparison ----
    tabPanel(
      title = tagList(icon("chart-line"), "Fit Model Comparison"),
      value = "fit_tab",  # <--- tambahkan ini juga
      
      fluidRow(
        div(
          style = "text-align:center;",  # pusatkan kontainer
          div(
            style = "display:inline-block; font-size:11px; line-height:0.9; padding:0; margin:0;",
            
            column(12, h5(icon("info-circle"), "Model Fit Statistics"), 
                   DTOutput("fit_table")#,
                   # tags$div(
                   #   style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
                   #   tags$b("Note:"),
                   #   tags$ul(
                   #     tags$li(tags$span(style = "color: blue;", "BIC:"),"Bayesian information criterion"),
                   #     tags$li(tags$span(style = "color: blue;", "AIC:"),"Aikake information criterion"),
                   #     tags$li(tags$span(style = "color: blue;", "Entropy:"),"A measure of classification uncertainty (1 = complete certainty; 0 = complete uncertainty)"),
                   #     tags$li(tags$span(style = "color: blue;", "prob_min:"),"Minimum of the diagonal of the average latent class probabilities for most likely class membership, by assigned class"),
                   #     tags$li(tags$span(style = "color: blue;", "prob_max:"),"Maximum of the diagonal of the average latent class probabilities for most likely class membership, by assigned class"),
                   #     tags$li(tags$span(style = "color: blue;", "n_min:"),"Proportion of the sample assigned to the smallest class"),
                   #     tags$li(tags$span(style = "color: blue;", "n_max:"),"Proportion of the sample assigned to the largest class"),
                   #     tags$li(tags$span(style = "color: blue;", "BLRT:"),"Bootstrapped likelihood test"),
                   #   )
                   # )
                   )
        )),
        br(),
        # column(6, h5(icon("chart-bar"), "AIC & BIC Comparison"), 
        #        downloadButton("download_plot_AicBic_LPA", "Download Plot AIC/BIC (.png)",),
        #        plotOutput("fit_plot")),
        # 
        # 
        # column(6, h5(icon("sliders-h"), "Entropy & Smallest Class Size"), 
        #        downloadButton("download_plot_entropy_LPA", "Download Plot Entropy & Class Size (.png)"),
        #        plotOutput("entropy_plot")
        #        ),
      column(6, h5(icon("chart-bar"), "BIC Comparison"), 
             plotOutput("fit_bic"), height = "200px"),
      column(6, h5(icon("chart-bar"), "AIC Comparison"), 
             plotOutput("fit_aic"),height = "200px"),
      column(6, h5(icon("chart-bar"), "Entropy Comparison"), 
             plotOutput("fit_entropy"),height = "200px"),
      column(6, h5(icon("chart-bar"), "Class Size Comparison"), 
             plotOutput("fit_class_size"),height = "200px")
      )
    ),
    
    # --- TAB 3: Best Model ----
    tabPanel(
      title = tagList(icon("star"), "Best Model"),
      sidebarLayout(
        sidebarPanel(
          width = 3, 
          selectInput(
            "model_type", 
            "Select Model Type:",
            choices = list(
              "Model 1: Equal variances, covariances fixed to 0" = 1,
              "Model 2: Varying variances, covariances fixed to 0" = 2,
              "Model 3: Equal variances & covariances" = 3,
              "Model 6: Varying variances & covariances" = 6
            ),
            selected = 1
          ),
          numericInput("best_k", label = "Select the Best Number of Profiles:",value = 3, min = 1),
          uiOutput("profile_name_inputs")
        ),
        mainPanel(
          width = 9, 
          br(),
    
          h5(icon("project-diagram"), "Profile Plot of the Best Model"),
          downloadButton("download_plot_best_LPA", "Download Plot Best Model (.png)"),
          plotOutput("best_model_plot", height = "450px"),
          br(),
          
          div(
            style = "text-align:center;",  # pusatkan kontainer
            div(
              style = "display:inline-block; font-size:11px; line-height:0.9; padding:0; margin:0;",
              h5(icon("info-circle"), "Class Size and Summary Table"), 
              DTOutput("summary_table") 
              )
          )
        )
      )
    ),
    
    # --- TAB 4: Summary & Report ----
    tabPanel(
      title = tagList(icon("file-alt"), "Summary & Report"),
      h5("Profile Classification Data"),
      DTOutput("profile_table"),
    ),
    tabPanel(
      title = tagList(icon("info-circle"), "About"),
      fluidRow(
        column(
          width = 8, offset = 2,
          br(),
          div(
            style = "text-align:center;",
            tags$hr(),
            tags$h5("projectLSA Was Developed By:"),
            tags$p(
              tags$a(
                href = "https://scholar.google.com/citations?user=PSAwkTYAAAAJ&hl=id",
                target = "_blank",
                "Dr. Hasan Djidu, M.Pd."),
              tags$br(),
              "Universitas Sembilanbelas November Kolaka"
            ),
            tags$h5("Supervised By:"),
            tags$p(
              tags$a(
                href = "https://scholar.google.com/citations?user=7CzPTYIAAAAJ&hl=id",
                target = "_blank",
                "Prof. Dr. Heri Retnawati, M.Pd."), 
              tags$br(),
              "Universitas Negeri Yogyakarta"
            ),
            
            tags$p(tags$a(
              href = "https://scholar.google.com/citations?hl=id&user=VGKeBm0AAAAJ",
              target = "_blank",
              "Prof. Dr. Samsul Hadi"), 
              tags$br(),
              "Universitas Negeri Yogyakarta"
            ),
            tags$p(tags$a(
              href = "https://scholar.google.com/citations?hl=id&user=k4MA8XgAAAAJ",
              target = "_blank",
              "Dr. Drs. Ir. Haryanto, M.Pd., M.T."), 
              tags$br(),
              "Universitas Negeri Yogyakarta"
            ),
            tags$b("Contact:"),
            tags$a("hasandjidu@gmail.com"),
            tags$hr()
          )
        ),
        column(
          width = 8, offset = 2,
          h4("References (R Packages)"),
          uiOutput("package_references_lpa"),
          br(),
          div(
            style = "text-align:center;",
            tags$p(
              style = "font-size:13px; color:#777;",
              format(Sys.Date(), "%Y"), 
              "projectLSA. All rights reserved."
            ) ))
      )
    )
  )
}