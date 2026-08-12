# cfa_ui.R - projectLSA CFA/SEM Module UI
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
                                     "Built-in: bfi (CFA/EFA)" = "bfi",
                                     "Built-in: HolzingerSwineford1939 (MGCFA/CFA)" = "HolzingerSwineford1939",
                                     "Built-in: PoliticalDemocracy (SEM)" = "PoliticalDemocracy",
                                     "Built-in: Demo.growth (LGM)" = "Demo.growth"),
                         selected = "upload"),
             conditionalPanel(condition = "input.data_source == 'upload'",
                              fileInput("datafile", "Upload Data/Workspace (csv/xlsx/sav/rds)", accept = c(".csv", ".xlsx", ".sav", ".rds"))),
             uiOutput("id_select_ui"),
             uiOutput("var_select_ui"),
             tags$label("Model (Lavaan syntax):"),
             uiOutput("cfa_model_ui"),
             
             tags$div(
               style = "margin-top: 15px; margin-bottom: 5px;",
               actionLink("cfa_method_guide", " Methodological Guide", icon = icon("info-circle"), 
                          style = "font-weight: bold; color: #17a2b8; font-size: 13px;")
             ),
             
             selectInput("cfa_estimator", "Estimator:",
                         choices = c("ML" = "ML", "GLS" = "GLS", "WLS" = "WLS", 
                                     "DWLS" = "DWLS", "ULS" = "ULS", "DLS" = "DLS", 
                                     "PML" = "PML", "MLM" = "MLM", "MLMVS" = "MLMVS", 
                                     "MLMV" = "MLMV", "MLF" = "MLF", "MLR" = "MLR", 
                                     "WLSM" = "WLSM", "WLSMVS" = "WLSMVS", "WLSMV" = "WLSMV", 
                                     "ULSM" = "ULSM", "ULSMVS" = "ULSMVS", "ULSMV" = "ULSMV"),
                         selected = "ML"),
             selectInput("cfa_missing", "Missing data:",
                         choices = c("Listwise" = "listwise", "Pairwise" = "pairwise", "FIML" = "fiml"),
                         selected = "listwise"),
             checkboxInput("cfa_std_est", "Standardized estimates", TRUE),
             checkboxInput("htmt_opt", "Heterotrait–Monotrait Ratio (HTMT)", FALSE),
             
             actionButton("run_cfa", label = tagList(icon("play"), "Run CFA/SEM"),
                          class = "btn btn-success",
                          style = "width: 100% !important; margin-bottom: 10px;"),
             downloadButton("export_cfa_rds", "Export Model (.rds)",
                            class = "btn btn-primary",
                            style = "width: 100% !important; margin-bottom: 10px;"),
             actionButton("btn_export_modal", "Export Report (HTML)",
                          class = "btn btn-warning btn-block btn-glow",
                          style = "width: 100%; font-weight: bold;",
                          onclick = "$(this).removeClass('btn-glow');")
           )
    ), 
    column(width = 9,
           tabsetPanel(
             id = "main_tab_cfa",
             
             tabPanel(
               title = tagList(icon("upload"), "Data Preview"),
               br(),
               
               DTOutput("data_preview"),
               tags$hr(),
               
               bsCollapse(id = "cfa_data_preview_collapse", multiple = TRUE,
                 bsCollapsePanel("Data Summary", style = "default",
                    fluidRow(
                      column(12, selectInput("cfa_summary_theme", "Chart Theme:", choices = c("Default" = "default", "Pastel" = "pastel", "Blue" = "blue", "Dark" = "dark")))
                    ),
                   tabsetPanel(
                     tabPanel("Summary Table", 
                              br(),
                              uiOutput("cfa_summary_table")
                     ),
                     tabPanel("Visualizations", 
                              br(),
                              fluidRow(
                                column(6, plotOutput("cfa_data_summary_plot", height = "350px")),
                                column(6, plotOutput("cfa_data_heatmap", height = "350px"))
                              )
                     )
                   )
                 ),
                 bsCollapsePanel("Calculate Variable", style = "default",
                   fluidRow(
                     column(5,
                       tags$label("Calculation Syntax:"),
                       tags$p(style = "font-size: 11px; color: #666; margin-bottom: 2px;", "Format: NewVar = var1, var2, var3"),
                       textAreaInput("cfa_agg_syntax", label = NULL, rows = 6, placeholder = "A = x1, x2, x3\nB = y1, y2")
                     ),
                     column(4,
                       tags$label("Insert Variables:"),
                       shinyWidgets::pickerInput("cfa_agg_vars_insert", label = NULL, choices = NULL, multiple = TRUE, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                       actionButton("cfa_btn_insert_agg", "Insert to Syntax", icon = icon("arrow-left"), class = "btn-info btn-sm", style = "width: 100%;")
                     ),
                     column(3,
                       selectInput("cfa_agg_method", "Method (Global):", choices = c("Mean" = "mean", "Sum" = "sum")),
                       actionButton("cfa_btn_aggregate", "Calculate All", class = "btn-primary", style = "width: 100%; margin-bottom: 5px;"),
                       actionButton("cfa_btn_reset_agg", "Reset Data", class = "btn-danger btn-sm", style = "width: 100%;")
                     )
                   )
                 )
               )),
             # ====Model Summary =====
             tabPanel(
               title = tagList(icon("chart-line"), "Model Summary"),
                      value = "fit_tab_cfa", 
                      br(),
                      fluidRow(
                        uiOutput("fit_comparison")

                      )),
             tabPanel(
               title = tagList(icon("table"), "Parameter Estimates"),
               br(), 
               tags$h4("Factor Loadings & Regression Paths"),
               DTOutput("loadings_table"),
               br(), tags$hr(), br(),
               tags$h4("Variances & Covariances"),
               uiOutput("variance_warning"),
               DTOutput("variances_table")
               ),
             tabPanel(
               title = tagList(icon("calculator"), "Factor Scores"),
               br(),
               tabsetPanel(
                 tabPanel("Current Data Scores",
                          br(),
                          DTOutput("fscores_cfa")
                 ),
                 tabPanel("Score New Data",
                          br(),
                          wellPanel(
                            style = "background: #f4f6f9; border: 1px solid #e2e8f0; padding-bottom: 5px;",
                            fluidRow(
                              column(12,
                                tags$h4(icon("file-import"), " Score New Data"),
                                tags$p("Upload new data to calculate factor scores using the fitted CFA model.")
                              ),
                              column(4,
                                downloadButton("download_cfa_template", "1. Download Template (Excel)", class = "btn-info", style = "width: 100%; margin-top: 10px;")
                              ),
                              column(4,
                                fileInput("cfa_newdata", "2. Upload New Data (Excel/CSV)", accept = c(".csv", ".xlsx", ".xls"), width = "100%")
                              ),
                              column(4,
                                actionButton("cfa_score_newdata_btn", "3. Calculate Scores", icon = icon("calculator"), class = "btn-success", style = "width: 100%; margin-top: 25px;")
                              )
                            )
                          ),
                          br(),
                          bsCollapse(id = "cfa_newdata_collapse",
                            bsCollapsePanel("Calculate Variable (Optional)", style = "info",
                              fluidRow(
                                column(5,
                                  tags$label("Calculation Syntax:"),
                                  tags$p(style = "font-size: 11px; color: #666; margin-bottom: 2px;", "Format: NewVar = var1, var2, var3"),
                                  textAreaInput("cfa_newdata_agg_syntax", label = NULL, rows = 6, placeholder = "A = x1, x2, x3\nB = y1, y2")
                                ),
                                column(4,
                                  tags$label("Insert Variables:"),
                                  shinyWidgets::pickerInput("cfa_newdata_agg_vars_insert", label = NULL, choices = NULL, multiple = TRUE, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                  actionButton("cfa_newdata_btn_insert_agg", "Insert to Syntax", icon = icon("arrow-left"), class = "btn-info btn-sm", style = "width: 100%;")
                                ),
                                column(3,
                                  selectInput("cfa_newdata_agg_method", "Method (Global):", choices = c("Mean" = "mean", "Sum" = "sum")),
                                  actionButton("cfa_newdata_btn_aggregate", "Calculate All", class = "btn-primary", style = "width: 100%; margin-bottom: 5px;"),
                                  actionButton("cfa_newdata_btn_reset_agg", "Reset Data", class = "btn-danger btn-sm", style = "width: 100%;")
                                )
                              )
                            )
                          ),
                          br(),
                          div(style = "text-align: right; margin-bottom: 5px;",
                              downloadButton("download_cfa_newscores", "Download New Scores (.csv)", class = "btn-primary btn-sm")),
                          DTOutput("cfa_newscores_table")
                 )
               )
             ),
             tabPanel(
               title = tagList(icon("cogs"), "Advanced Analysis"),
               value = "invariance_tab_cfa",
               br(),
               fluidRow(
                 column(4, 
                   wellPanel(
                     tags$h4(icon("sliders"), " Analysis Mode"),
                     selectInput("cfa_analysis_mode", "Analysis Mode:",
                                 choices = c("Standard CFA/SEM" = "standard",
                                             "Multi-group CFA" = "mgcfa",
                                             "Latent Growth Model" = "lgm")),
                     conditionalPanel("input.cfa_analysis_mode == 'mgcfa'",
                       uiOutput("mgcfa_group_ui"),
                       selectInput("mgcfa_invariance", "Invariance Level:",
                                   choices = c("Configural" = "configural",
                                               "Metric (Weak)" = "loadings",
                                               "Scalar (Strong)" = "loadings, intercepts",
                                               "Strict" = "loadings, intercepts, residuals")),
                       tags$hr(style="margin-top:10px; margin-bottom:10px;"),
                       tags$p(style="font-size:11px; color:#666;", "Or run all levels to compare:"),
                       actionButton("run_auto_invariance", "Auto Invariance Test", class = "btn-info btn-block btn-sm", style="width:100%;")
                     ),
                     conditionalPanel("input.cfa_analysis_mode == 'lgm'",
                       tags$p(style="font-size:11px; color:#666;", 
                              "Note: LGM uses lavaan::growth(). Ensure your syntax defines intercept (i) and slope (s) factors.")
                     )
                   )
                 ),
                 column(8,
                   tags$h4("Advanced Analysis Results"),
                   uiOutput("invariance_results")
                 )
               )
             ),
            # ====Plot =====
             tabPanel(
               title = tagList(icon("project-diagram"), "Path Plot"),
                      br(),
                      fluidRow(
                        column(3,
                        bsCollapse(
                          id = "plot_settings",
                          
                          bsCollapsePanel( style = 'info',
                                           "1. General & Layout",
                                     fluidRow(
                                       column(12, selectInput("plot_model_scope", "Display Scope:", 
                                                              choices = c("Full Model (Measurement + Structural)" = "full", 
                                                                          "Structural Model Only" = "structural"), 
                                                              selected = "full"))
                                     ),
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
                                     column(6, textInput("bifactor", "Bifactor:", value = NULL, placeholder = "Gen. Factor"))
                                   )
                                 ),
                                 
                          bsCollapsePanel( style = 'info',
                                           "2. Node & Edge Sizes",
                                   fluidRow(
                                     column(6, numericInput("plot_nodesize_lat", "Latent Size:", value = 8, min = 1, max = 15, step = 0.2)),
                                     column(6, numericInput("plot_nodesize_man", "Obs(W) Size:", value = 8, min = 1, max = 15, step = 0.2))
                                   ),
                                   fluidRow(
                                     column(6, numericInput("plot_nodesize_man2", "Obs(H) Size:", value = 4, min = 0.1, max = 10, step = 0.1)),
                                     column(6, numericInput("plot_edge_label_size", "Label Size:", value = 0.75, min = 0.2, max = 2, step = 0.05))
                                   ),
                                   fluidRow(
                                     column(12, numericInput("edgewidth", "Edge Width:", value = 0.3, min = 0.1, max = 5, step = 0.05))
                                   )
                                 ),
                          bsCollapsePanel( style = 'info',
                                           "3. Measurement Model",
                                   checkboxInput("plot_curve", "Curve", TRUE),
                                   checkboxInput("plot_layout_split", "Split Layout", FALSE),
                                   
                                   fluidRow(
                                     column(6, numericInput("subScale_Wi", "Sub_Wid:", value = 0.2, min = 0.2, max = 3, step = 0.1)),
                                     column(6, numericInput("subScale_He", "Sub_Hei:", value = 0.2, min = 0.2, max = 3, step = 0.1))
                                   )
                                 ),
                          bsCollapsePanel( style = 'info',
                                           "4. Colors & Display Options",
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
                                 ),
                          bsCollapsePanel( style = 'info',
                                           "5. Fit Indices",
                                           div(style = "text-align: center;",
                                             column(12, shinyWidgets::pickerInput("fit_indices_selected", "Select Fit Indices to Show:", 
                                       choices = c("SELECT ALL", "chisq", "df", "pvalue", "cmindf", "rmsea", "cfi", "tli", "srmr", "gfi", "nfi"), 
                                       selected = c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"), 
                                       multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE), width="100%")),
                                             column(12, checkboxInput("cfa_use_robust_fit", "Use Robust/Scaled Indices (if available)", TRUE))
                                           )
                          )
                        )
                        ),
                        column(9, 
                               downloadButton("download_cfa_plot", "Download Plot (PNG)", class = "btn btn-primary btn-glow"),
                               br(),
                               plotOutput("path_plot", height = "700px")
                        )
                      )),

             # --- Settings ---
             tabPanel(
               title = icon("sliders-h"),
               value = "settings_tab_cfa",
               br(),
               tags$h3(icon("sliders-h"), " General Settings"),
               tags$hr(),
               fluidRow(
                 column(6,
                   wellPanel(
                     tags$h4("Data & Output Formatting"),
                     radioButtons("cfa_dec_sep", "Decimal Separator:", 
                                  choices = c("Dot (.)" = ".", "Comma (,)" = ","),
                                  selected = ".", inline = TRUE)
                   )
                 )
               )
             ),
             # --- Report Preview ---
               tabPanel(
                 title = tagList(icon("file-alt"), " Report Preview"),
                 br(),
                 div(style = "display: flex; gap: 10px; margin-bottom: 15px;",
                     actionButton("cfa_generate_preview", tagList(icon("sync"), " Generate Report Preview"), class = "btn btn-success"),
                     downloadButton("download_report_html", "Download HTML Report", class = "btn btn-primary")
                 ),
                 div(
                   style = "border: 1px solid #ddd; border-radius: 4px; padding: 5px; background: #f9f9f9;",
                   uiOutput("cfa_report_preview_frame")
                 )
               ),

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
                    tags$br(),
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