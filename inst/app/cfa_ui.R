# cfa_ui.R - projectLSA CFA/SEM Module UI
cfa_ui <- function(project) {

  # ---- control panel (all input IDs unchanged) -----------------------------
  sidebar <- tagList(

    pl_card(
      "&#128194;", "Data", hint = "step 1",
      selectInput("data_source", "Data source",
                  choices = c("Upload Data" = "upload",
                              "Built-in: bfi (CFA/EFA)" = "bfi",
                              "Built-in: HolzingerSwineford1939 (MGCFA/CFA)" = "HolzingerSwineford1939",
                              "Built-in: PoliticalDemocracy (SEM)" = "PoliticalDemocracy",
                              "Built-in: Demo.growth (LGM)" = "Demo.growth"),
                  selected = "upload"),
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("datafile", "Upload data or workspace (csv, xlsx, sav, rds)",
                  accept = c(".csv", ".xlsx", ".sav", ".rds"))
      ),
      uiOutput("id_select_ui"),
      uiOutput("var_select_ui")
    ),

    pl_card(
      "&#129513;", "Model", hint = "step 2",
      tags$label("Model (lavaan syntax)"),
      uiOutput("cfa_model_ui"),
      actionLink("cfa_method_guide", " Methodological guide", icon = icon("circle-info"))
    ),

    pl_card(
      "&#9881;&#65039;", "Estimation", hint = "step 3",
      selectInput("cfa_estimator", "Estimator",
                  choices = c("ML" = "ML", "GLS" = "GLS", "WLS" = "WLS",
                              "DWLS" = "DWLS", "ULS" = "ULS", "DLS" = "DLS",
                              "PML" = "PML", "MLM" = "MLM", "MLMVS" = "MLMVS",
                              "MLMV" = "MLMV", "MLF" = "MLF", "MLR" = "MLR",
                              "WLSM" = "WLSM", "WLSMVS" = "WLSMVS", "WLSMV" = "WLSMV",
                              "ULSM" = "ULSM", "ULSMVS" = "ULSMVS", "ULSMV" = "ULSMV"),
                  selected = "ML"),
      # Ordinal-data option, offered only for estimators that lavaan can combine with `ordered`
      conditionalPanel(
        condition = "['WLS','DWLS','ULS','PML','WLSM','WLSMV','WLSMVS','ULSM','ULSMV','ULSMVS'].indexOf(input.cfa_estimator) > -1",
        checkboxInput("cfa_ordered", "Treat indicators as ordinal (ordered categorical)", value = TRUE),
        conditionalPanel(
          condition = "input.cfa_ordered",
          uiOutput("cfa_ordered_vars_ui"),
          tags$p(class = "pl-hint",
                 "Polychoric correlations and thresholds are used. Leave the list empty to treat all selected indicators as ordinal. FIML is not available for ordinal data; pairwise deletion is used.")
        ),
        conditionalPanel(
          condition = "!input.cfa_ordered",
          tags$p(class = "pl-note-warn",
                 icon("triangle-exclamation"), " Indicators are treated as continuous. Tick the option above for Likert/ordinal items.")
        )
      ),
      selectInput("cfa_missing", "Missing data",
                  choices = c("Listwise" = "listwise", "Pairwise" = "pairwise", "FIML" = "fiml"),
                  selected = "listwise"),
      checkboxInput("cfa_std_est", "Standardized estimates", TRUE),
      checkboxInput("htmt_opt", "Heterotrait-Monotrait ratio (HTMT)", FALSE)
    ),

    tags$div(
      class = "pl-dock",
      actionButton("run_cfa", label = tagList(icon("play"), "Run CFA/SEM"),
                   class = "btn btn-success pl-run"),
      downloadButton("export_cfa_rds", "Save project (.rds)", class = "btn btn-default"),
      actionButton("btn_export_modal", label = tagList(icon("file-export"), "Export report (HTML)"),
                   class = "btn btn-warning")
    )
  )

  # ---- stage: one hidden tabset, driven by the navigation rail -----------
  tabs <- pl_tabs(
    "main_tab_cfa",
    noctl = c("path_tab_cfa", "report_tab_cfa", "about_tab_cfa", "settings_tab_cfa"),
    subs = list(
      data_tab_cfa       = "Preview your data, summarise it, and build composite variables.",
      fit_tab_cfa        = "Fit indices, model status, and the history of every model you ran.",
      params_tab_cfa     = "Loadings, regression paths, variances, and covariances.",
      scores_tab_cfa     = "Scores for the current data, or score new respondents from this model.",
      invariance_tab_cfa = "Multi-group invariance testing and latent growth models.",
      path_tab_cfa       = "Customise and download the model diagram.",
      report_tab_cfa     = "Preview the HTML report and download it.",
      settings_tab_cfa   = "Output formatting.",
      about_tab_cfa      = "Authors, citation, and the R packages behind this module."
    ),

             
             tabPanel(
               title = tagList(icon("upload"), "Data Preview"),
               value = "data_tab_cfa",
               tags$div(class = "pl-tip",
                        tags$span(class = "pl-emoji", HTML("&#128075;")),
                        tags$span("Hi! Pick a data source on the left, write your model, then press ",
                                  tags$b("Run CFA/SEM"), ". Your results will bloom in the other tabs.")),
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
               value = "params_tab_cfa",
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
               value = "scores_tab_cfa",
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
             # ==== Path diagram =====
             tabPanel(
               title = tagList(icon("project-diagram"), "Path Plot"),
               value = "path_tab_cfa",
               pl_split(
                 pl_side(
                   # 1 - what the diagram and its legend show
                   pl_fold(
                     "Show", icon_name = "eye",
                     selectInput("plot_model_scope", "Display scope",
                                 choices = c("Full model (measurement + structural)" = "full",
                                             "Structural model only" = "structural"),
                                 selected = "full"),
                     checkboxInput("plot_standardized", "Standardized estimates", TRUE),
                     checkboxInput("plot_residuals", "Show residuals", FALSE),
                     checkboxInput("plot_exoCov", "Show exogenous covariances", FALSE),
                     shinyWidgets::pickerInput(
                       "fit_indices_selected", "Fit indices in the legend",
                       choices = c("SELECT ALL", "chisq", "df", "pvalue", "cmindf", "rmsea", "cfi", "tli", "srmr", "gfi", "nfi"),
                       selected = c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"),
                       multiple = TRUE, width = "100%",
                       options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 3")
                     ),
                     checkboxInput("cfa_use_robust_fit", "Use robust/scaled indices (if available)", TRUE)
                   ),
                   # 2 - arrangement of the nodes
                   pl_fold(
                     "Layout", icon_name = "sitemap",
                     pl_grid(
                       selectInput("plot_style", "Style", choices = c("lisrel", "ram", "mx", "OpenMx"), selected = "lisrel"),
                       selectInput("plot_layout", "Layout", choices = c("tree", "tree2", "tree3", "spring", "circle", "circle2"), selected = "tree2"),
                       numericInput("plot_rotation", "Rotation", value = 4, min = 1, max = 4, step = 1),
                       textInput("bifactor", "General factor", value = NULL, placeholder = "e.g. G")
                     ),
                     checkboxInput("plot_curve", "Curved paths", TRUE),
                     checkboxInput("plot_layout_split", "Split layout", FALSE),
                     conditionalPanel(
                       condition = "input.plot_layout_split",
                       pl_grid(
                         numericInput("subScale_Wi", "Sub-scale width", value = 0.2, min = 0.2, max = 3, step = 0.1),
                         numericInput("subScale_He", "Sub-scale height", value = 0.2, min = 0.2, max = 3, step = 0.1)
                       )
                     )
                   ),
                   # 3 - sizes
                   pl_fold(
                     "Size", icon_name = "up-right-and-down-left-from-center", open = FALSE,
                     tags$p(class = "pl-hint", "Canvas size is in inches and also applies to the PNG download."),
                     pl_grid(
                       numericInput("plotwidth", "Canvas width", value = 5, min = 1, max = 15, step = 0.3),
                       numericInput("plotheight", "Canvas height", value = 5, min = 1, max = 20, step = 0.3),
                       numericInput("plot_nodesize_lat", "Latent node", value = 8, min = 1, max = 15, step = 0.2),
                       numericInput("plot_nodesize_man", "Observed width", value = 8, min = 1, max = 15, step = 0.2),
                       numericInput("plot_nodesize_man2", "Observed height", value = 4, min = 0.1, max = 10, step = 0.1),
                       numericInput("plot_edge_label_size", "Label size", value = 0.75, min = 0.2, max = 2, step = 0.05),
                       numericInput("edgewidth", "Line width", value = 0.3, min = 0.1, max = 5, step = 0.05)
                     )
                   ),
                   # 4 - colours
                   pl_fold(
                     "Colors", icon_name = "palette", open = FALSE,
                     selectInput("plot_color_scheme", "Color scheme",
                                 choices = c("Blue-Yellow", "Ocean", "Forest", "Rainbow", "Pastel", "Greyscale", "Earth",
                                             "Vibrant", "Monochrome", "Sunset", "Rose", "Mint", "Custom"),
                                 selected = "Blue-Yellow"),
                     conditionalPanel(
                       condition = "input.plot_color_scheme == 'Custom'",
                       pl_grid(
                         colourpicker::colourInput("mancolour", "Observed", value = "#A1E3F9", showColour = "background", palette = "square"),
                         colourpicker::colourInput("latcolour", "Latent", value = "#FFFFBA", showColour = "background", palette = "square")
                       )
                     )
                   )
                 ),
                 pl_main(
                   pl_toolbar(
                     "Path diagram", "Changes apply instantly. Adjust the groups on the left."
                   ),
                   plotOutput("path_plot", height = "700px")
                 )
               )),

             # --- Settings ---
             tabPanel(
               title = tagList(icon("sliders-h"), "Settings"),
               value = "settings_tab_cfa",
               br(),
               tags$h3(icon("sliders-h"), " General Settings"),
               tags$hr(),
               tags$p(class = "pl-hint", "The decimal separator is set at the top right of the screen and applies to every module.")
             ),
             # --- Report Preview ---
               tabPanel(
                 title = tagList(icon("file-lines"), "Report"),
      value = "report_tab_cfa",
      pl_report_tab("cfa", "cfa_generate_preview", "download_report_html", "cfa_report_preview_frame")
    ),

             # --- About ----
            tabPanel(
              title = tagList(icon("info-circle"), "About"),
               value = "about_tab_cfa",
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

  list(controls = sidebar, nav = tabs$nav, stage = tabs$stage)
}
