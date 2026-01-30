# Fungsi untuk menu LCA
lca_ui <- function(project) {
  tabsetPanel(
    id = "main_tab_lca",
    
    # --- TAB 1: Prepare Data & Model ----
    tabPanel(
      title = tagList(icon("upload"), "Prepare Data & Model"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          actionButton("go_home", 
                       label = tagList(icon("home"), "Main Menu"), 
                       class = "btn btn-danger btn-block",
                       style = "width: 100% !important;"),
          br(),
          selectInput(
            "data_source_lca", 
            "Select Data Source:",
            choices = c("Upload Data" = "upload",
                        "Cheating (Dayton, 1998)" = "cheat",
                        "Lazarsfeld & Henry (1968)" = "lazar",
                        "General Social Survey (McCutcheon, 1987)" = "gss82"
            ),
            selected = "upload"
          ),
          conditionalPanel(
            condition = "input.data_source_lca == 'upload'",
            fileInput("datafile_lca", "Upload Data (csv/xlsx)", accept = c(".csv", ".xlsx"))
          ),
          uiOutput("id_select_ui_lca"),
          uiOutput("var_select_ui_lca"),
          numericInput("min_class_lca", "Min. Number of Class:", 1, min = 1),
          numericInput("max_class_lca", "Max. Number of Class:", 6, min = 3),
          uiOutput("cov_lca_ui"),
          
          br(),
          
          actionButton("run_lca", 
                       label = tagList(icon("play"), "Run LCA"), 
                       class = "btn btn-success btn-block",
                       style = "width: 100% !important;")
        ),
        mainPanel(
          width = 9,
          h5(icon("table"), "Data Preview"),
          DTOutput("data_preview_lca"),
          DTOutput("data_summary_lca"),
          br(),
          uiOutput("data_description"),
          
          br(),
          
          
        )
      )
    ),
    
    # --- TAB 2: Fit Model Comparison ----
    tabPanel(
      title = tagList(icon("chart-line"), "Fit Model Comparison"),
      value = "fit_tab_lca",  
      
      fluidRow(
        column(12,
               div(
                 style = "text-align:center;",  
                 div(
                   style = "display:inline-block; font-size:11px; line-height:0.9; padding:0; margin:0;",
                   h5(icon("info-circle"), "Model Fit Statistics"), 
                   DTOutput("fit_table_lca")
                 )
               ),
               br(),
               tags$div(
                 style = "margin-top: 0px; font-size: 12px; color: #6c757d;",
                 class = "badge-info",
                
                 tags$b("Note:"),
                 tags$ul(
                   tags$li(tags$span(style = "color: blue;", "AIC:"), 
                           "Akaike Information Criterion; smaller values indicate better model fit."),
                   tags$li(tags$span(style = "color: blue;", "BIC:"), 
                           "Bayesian Information Criterion; smaller values indicate better and more parsimonious model fit."),
                   tags$li(tags$span(style = "color: blue;", "Entropy:"), 
                           "A measure of classification quality (values closer to 1 indicate clearer class separation)."),
                   tags$li(tags$span(style = "color: blue;", "Gsq:"), 
                           "Likelihood ratio statistic (G²) for assessing absolute model fit."),
                   tags$li(tags$span(style = "color: blue;", "p_Gsq:"), 
                           HTML("Bootstrap p-value for G². <b>H₀:</b> the model with K classes adequately fits the data. p ≥ 0.05 indicates acceptable absolute fit.")
                   ),
                   tags$li(tags$span(style = "color: blue;", "Deviance:"), 
                           "Deviance statistic comparing two nested models with K and K+1 classes."),
                   tags$li(tags$span(style = "color: blue;", "p_Deviance:"), 
                           HTML("Bootstrap p-value from the deviance test reported for the <b>K-class model</b>. 
     <b>H₀:</b> the (K−1)-class model is not significantly worse than the K-class model. 
     p < 0.05 indicates that adding one class (from K−1 to K) significantly improves model fit.")
                   ),
                   tags$li(tags$span(style = "color: blue;", "Av_Prob:"), 
                           "Average posterior classification probability by class, indicating classification accuracy.")
                 )
               )
        ),
        column(6, h5(icon("chart-bar"), "AIC & BIC Comparison"), 
               downloadButton("download_plot_AicBic_LCA", "Download Plot AIC/BIC (.png)"),
               plotOutput("fit_plot_lca")
               ),
        column(6, h5(icon("chart-bar"), "Entropy & Average Posterior Probability Comparison"), 
               downloadButton("download_plot_entropy", "Download Plot Entropy & Av.Probs (.png)"),
               plotOutput("entropy_plot_lca")
        ), br(), br(),
      )
    ),
    
    # --- TAB 3: Best Model ----
    tabPanel(
      title = tagList(icon("star"), "Best Model"),
      sidebarLayout(
        sidebarPanel(
          width = 2, 
          numericInput("best_class_lca",label =  "Select the Best Number of Class:",value = 3, min = 1),
          uiOutput("class_name_inputs_lca")
        ),
        mainPanel(
          width = 10, 
          h5(icon("project-diagram"), "Class Plot of the Best Model"),
          downloadButton("download_plot_best_LCA", "Download Plot Best Model (.png)"),
          tags$b("Item-Categories Probabilities Plot"),
          ggiraph::girafeOutput("best_model_plot_lca", width = '100%', height = 'auto'),
          h5(icon("table"), "Class Size and Item-Category Probabilities"),
          tableOutput("summary_table_lca"),
          h5(icon("table"), "Latent Class Response Patterns"),
          DT::DTOutput("data_summary_lca_with_class"),
          br(),
          br(),
          
        )
      )
    ),
    
    # --- TAB 4: Summary & Report ----
    tabPanel(
      title = tagList(icon("file-alt"), "Summary & Report"),
      h4("Class Classification Data"),
      DTOutput("profile_table_lca"),
      h4("Compare Class"),
      column(12,
        column(3, uiOutput("var_x_ui")),
        column(4, uiOutput("var_y_ui")),
        column(2,
               selectInput(
                 "plot_type", "Plot Type:",
                 choices = c("Line" = "line", "Bar" = "bar"),
                 selected = "bar"
               )
        ),
        column(3,
               checkboxInput(
                 "check_anova",
                 label = "Compare Classes",
                 value = FALSE
               )
        )
      ),
      br(),

      column(12,
        column(6, plotOutput("cross_latent", height = "420px")),
        column(6, uiOutput("anova_lca_ui"))
      ),
      column(12, uiOutput("reg_lca_ui")),
      column(12,
             h4("Explore Other Variables by Class"),
             checkboxInput(
               "crosstab_result",
               label = "Explore variable",
               value = FALSE
             ),
             column(12, uiOutput("crosstab_ui")),
             br(),
             br(),
             br()
      ) 
    ),
    # --- TAB 5: About ----
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
          uiOutput("package_references_lca"),
          br(),
          div(
            style = "text-align:center;",
            tags$p(
              style = "font-size:13px; color:#777;",
             format(Sys.Date(), "%Y"), 
              "projectLSA. All rights reserved."
            )
          )
        )
      )
    )
  )
}
