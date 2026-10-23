# Fungsi untuk menu LCA
lca_ui <- function(project) {
  tabs <- pl_tabs(
    "main_tab_lca",
    
    # --- TAB 1: Prepare Data & Model ----
    tabPanel(
      title = tagList(icon("upload"), "Prepare Data & Model"),
      pl_split(
        pl_side(
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
            fileInput("datafile_lca", "Upload Data/Workspace (csv/xlsx/sav/rds)", accept = c(".csv", ".xlsx", ".sav", ".rds"))
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
                       style = "width: 100% !important; margin-bottom: 10px;"),
          downloadButton("export_lca_rds", "Save Project (.rds)",
                         class = "btn btn-primary",
                         style = "width: 100% !important;")
        ),
        pl_main(
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
                 style = "text-align:left;",  
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
        column(12, tags$div(
          class = "pl-two pl-two-flat",
          tags$section(class = "pl-flat", plotOutput("fit_plot_lca", height = "250px")),
          tags$section(class = "pl-flat", plotOutput("entropy_plot_lca", height = "250px"))
        ))
      )
    ),
    
    # --- TAB 3: Best Model ----
    tabPanel(
      title = tagList(icon("star"), "Best Model"),
      pl_split(
        pl_side(
          width = 2, 
          numericInput("best_class_lca",label =  "Select the Best Number of Class:",value = 3, min = 1),
          uiOutput("class_name_inputs_lca"),
          tags$hr(),
          selectInput("lca_plot_type", "Plot type",
                      choices = c("Automatic" = "auto", "Profile lines (two categories per item)" = "profile", "Stacked bars" = "stack"), selected = "auto"),
          selectInput("lca_palette", "Colours",
                      choices = c("Brand" = "brand", "Colour-blind safe" = "okabe", "Soft pastel" = "pastel", "Warm" = "warm", "Cool" = "cool", "Greys" = "grey", "Choose my own" = "custom"), selected = "brand"),
          uiOutput("lca_custom_cols"),
          checkboxInput("lca_show_values", "Show probabilities on the lines", FALSE)
        ),
        pl_main(
          width = 10,
          tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", icon("project-diagram"), " Class plot of the best model"),
                       ggiraph::girafeOutput("best_model_plot_lca", width = '100%', height = 'auto')),
          tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", icon("table"), " Class size and item-category probabilities"),
                       tableOutput("summary_table_lca")),
          tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", icon("table"), " Latent class response patterns"),
                       DT::DTOutput("data_summary_lca_with_class"))
        )
      )
    ),

    # --- Follow-up analyses: classification quality, comparisons, cross-tabs, predictors, export ----
    followup_tabs("lca", "class"),

    # --- Report Preview ----
    tabPanel(
      title = tagList(icon("file-lines"), "Report"),
      pl_report_tab("lca", "lca_generate_preview", "download_report_lca", "lca_report_preview_frame")
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
  list(controls = NULL, nav = tabs$nav, stage = tabs$stage)
}
