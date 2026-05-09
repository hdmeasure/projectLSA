# fa_ui.R (revisi)
cfa_ui <- function(project) {
  fluidRow(
    column(width = 3,
           
           wellPanel(
             actionButton("go_home", 
                        label = tagList(icon("home"), "Main Menu"), 
                        class = "btn btn-danger btn-block",
                        style = "width: 100% !important;"),
             br(),
             selectInput("data_source", "Select Data Source:",
                         choices = c("Upload Data" = "upload",
                                     "Built-in: bfi" = "bfi",
                                     "Built-in: HolzingerSwineford1939" = "HolzingerSwineford1939"),
                         selected = "upload"),
             conditionalPanel(condition = "input.data_source == 'upload'",
                              fileInput("datafile", "Upload Data (csv/xlsx)", accept = c(".csv", ".xlsx"))),
             uiOutput("id_select_ui"),
             uiOutput("var_select_ui"),
             tags$label("Model (Lavaan syntax):"),
             uiOutput("cfa_model_ui"),
            
             selectInput("cfa_estimator", "Estimator:",
                         choices = c("ML" = "ML", "MLR" = "MLR", "WLSMV" = "WLSMV", 
                                     "DWLS" = "DWLS", "GLS" = "GLS", "ULS" = "ULS"),
                         selected = "ML"),
             selectInput("cfa_missing", "Missing data:",
                         choices = c("Listwise" = "listwise", "Pairwise" = "pairwise", "FIML" = "fiml"),
                         selected = "listwise"),
             radioButtons("cfa_dec_sep", "Decimal Separator:", 
                          choices = c("English (.)" = ".", "Indonesian (,)" = ","),
                          selected = ".", inline = TRUE),
             checkboxInput("cfa_std_est", "Standardized estimates", TRUE),
             checkboxInput("htmt_opt", "Heterotrait–Monotrait Ratio (HTMT)", FALSE),
             
             actionButton("run_cfa", label = tagList(icon("play"), "Run CFA"),
                          class = "btn btn-success btn-block",
                          style = "width: 100% !important;"),
             br(),
             actionButton("btn_export_modal", "Export Report (HTML)", class = "btn btn-warning btn-block btn-glow", style = "width: 100%; font-weight: bold;", onclick="$(this).removeClass('btn-glow');")
           )
    ), 
    column(width = 9,
           tabsetPanel(
             id = "main_tab_cfa",
             
             tabPanel(
               title = tagList(icon("upload"), "Data Preview"),
               DTOutput("data_preview")),
             # ====Model Summary =====
             tabPanel(
               title = tagList(icon("chart-line"), "Model Summary"),
                      value = "fit_tab_cfa", 
                      br(),
                      fluidRow(
                        uiOutput("fit_comparison")

                      )),
             tabPanel(
               title = tagList(icon("table"), "Loadings & Params"),
               br(), DTOutput("loadings_table")
               ),
             tabPanel(
               title = tagList(icon("chart-bar"), "Variances"),
               br(), 
               uiOutput("variance_warning"),
               DTOutput("variances_table")
               ),
             tabPanel(
               title = tagList(icon("calculator"), "Factor Scores"),
               br(),
               DTOutput("fscores_cfa")
               ),
            # ====Plot =====
             tabPanel(
               title = tagList(icon("project-diagram"), "Path Plot"),
                      br(),
                      fluidRow(
                        column(3,
                               tags$h5("Plot Settings", style = "color: #2c3e50;"),
                               
                               wellPanel(
                                 style = "padding: 10px; margin-bottom: 10px;",
                                 tags$strong("1. General & Layout"),
                                 fluidRow(
                                   column(6, selectInput("plot_style", "Style:", choices = c("lisrel", "ram", "mx", "OpenMx"), selected = "lisrel")),
                                   column(6, selectInput("plot_layout", "Layout:", choices = c("tree", "tree2", "tree3", "spring", "circle", "circle2"), selected = "tree2"))
                                 ),
                                 fluidRow(
                                   column(6, numericInput("plotwidth", "Width:", value = 5, min = 1, max = 15, step = 0.3)),
                                   column(6, numericInput("plotheight", "Height:", value = 5, min = 1, max = 20, step = 0.3))
                                 ),
                                 fluidRow(
                                   column(6, numericInput("plot_rotation", "Rotation:", value = 4, min = 1, max = 4, step = 1)),
                                   column(6, textInput("bifactor", "Bfactor:", value = NULL, placeholder = "Gen. Factor"))
                                 )
                               ),
                               
                               wellPanel(
                                 style = "padding: 10px; margin-bottom: 10px;",
                                 tags$strong("2. Node & Edge Sizes"),
                                 fluidRow(
                                   column(4, numericInput("plot_nodesize_lat", "Latent:", value = 8, min = 3, max = 15, step = 0.3)),
                                   column(4, numericInput("plot_nodesize_man", "Obs(W):", value = 8, min = 2, max = 15, step = 0.3)),
                                   column(4, numericInput("plot_nodesize_man2", "Obs(H):", value = 4, min = 1, max = 10, step = 0.3))
                                 ),
                                 fluidRow(
                                   column(6, numericInput("plot_edge_label_size", "Label Size:", value = 0.75, min = 0.5, max = 2, step = 0.1)),
                                   column(6, numericInput("edgewidth", "Edge Width:", value = 0.3, min = 0.3, max = 5, step = 0.1))
                                 )
                               ),
                               
                               wellPanel(
                                 style = "padding: 10px; margin-bottom: 10px;",
                                 tags$strong("3. Colors & Display Options"),
                                 selectInput("plot_color_scheme", "Color scheme:",
                                             choices = c("Blue-Yellow", "Ocean", "Forest", "Rainbow", "Pastel", "Greyscale", "Earth", "Vibrant", "Monochrome", "Sunset", "Rose", "Mint", "Custom"),
                                             selected = "Blue-Yellow"),
                                 conditionalPanel(
                                   condition = "input.plot_color_scheme == 'Custom'",
                                   fluidRow(
                                     column(6, colourpicker::colourInput("mancolour", "Manifest:", value = "#A1E3F9", showColour = "background", palette = "square")),
                                     column(6, colourpicker::colourInput("latcolour", "Latent:", value = "#FFFFBA", showColour = "background", palette = "square"))
                                   )
                                 ),
                                 checkboxInput("plot_standardized", "Standardized estimates", TRUE),
                                 checkboxInput("plot_residuals", "Show residuals", FALSE),
                                 checkboxInput("plot_exoCov", "Show exogenous covariances", FALSE)
                               )
                        ),
                        column(9, 
                               plotOutput("path_plot", height = "700px"),
                               br(),
                               div(style = "text-align: center; margin-top: 10px; background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #e2e8f0;",
                                   tags$strong("4. Fit Indices & Export", style = "display: block; margin-bottom: 10px;"),
                                   checkboxGroupInput(
                                     "fit_indices_selected",
                                     label = NULL,
                                     choices = c("Chi-Square (χ²)" = "chisq", "df" = "df", "p-value" = "pvalue", "RMSEA" = "rmsea", "CFI" = "cfi", "GFI" = "gfi", "SRMR" = "srmr", "TLI" = "tli", "NFI" = "nfi"),
                                     selected = c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"),
                                     inline = TRUE
                                   ),
                                   br(),
                                   downloadButton("download_cfa_plot", "Download Plot (PNG)", class = "btn btn-primary")
                               )
                        )
                      )),
             # --- About ----
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
                   uiOutput("package_references_cfa"),
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
    )
  )  
}