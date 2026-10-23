# fa_ui.R (revisi)
efa_ui <- function(project) {
  tabs <- pl_tabs(
    "main_tab_fa",
    
    # === TAB 1: Prepare Data & Model ====
    tabPanel(
      title = tagList(icon("upload"), "Prepare Data & Model"),
      pl_split(
        pl_side(
          width = 3,
          # Tombol kembali
          actionButton("go_home", 
                       label = tagList(icon("home"), "Main Menu"), 
                       class = "btn btn-danger btn-block",
                       style = "width: 100% !important;"),
          br(),
          
          # === Input sumber data ===
          selectInput("data_source", "Select Data Source:",
                      choices = c("Upload Data" = "upload",
                                  "Built-in: bfi" = "bfi",
                                  "Built-in: HolzingerSwineford1939" = "HolzingerSwineford1939"
                                  
                                 )),
          
          conditionalPanel(
            condition = "input.data_source == 'upload'",
            fileInput("datafile", "Upload Data/Workspace (csv/xlsx/sav/rds)", accept = c(".csv", ".xlsx", ".sav", ".rds"))),
          uiOutput("id_select_ui_fa"),
          uiOutput("var_select_ui"),
          br(),
          selectInput("rotation_method", "Rotation Method:", 
                      choices = c("varimax", "oblimin", "promax", "none")),
          selectInput("missing_method_efa", "Handle Missing Data:",
                      choices = c("Listwise Deletion" = "listwise",
                                  "Pairwise Deletion" = "pairwise",
                                  "Mean Imputation" = "mean")),
          br(),
          actionButton("run_efa", label = tagList(icon("play"), "Run EFA"),
                       class = "btn btn-success",
                       style = "width: 100% !important; margin-bottom: 10px;"),
          downloadButton("export_efa_rds", "Export Model (.rds)", 
                         class = "btn btn-primary", 
                         style = "width: 100% !important;")
        ),
        
        pl_main(
          width = 9,
          DTOutput("data_preview_fa"),
          tags$hr(),
          bsCollapse(id = "efa_data_preview_collapse", multiple = TRUE,
            bsCollapsePanel("Data Summary", style = "default",
              fluidRow(
                column(12, selectInput("efa_summary_theme", "Chart Theme:", choices = c("Default" = "default", "Pastel" = "pastel", "Blue" = "blue", "Dark" = "dark")))
              ),
              tabsetPanel(
                tabPanel("Summary Table", 
                         br(),
                         uiOutput("efa_summary_table")
                ),
                tabPanel("Visualizations", 
                         br(),
                         fluidRow(
                           column(6, plotOutput("efa_data_summary_plot", height = "350px")),
                           column(6, plotOutput("efa_data_heatmap", height = "350px"))
                         )
                )
              )
            ),
            bsCollapsePanel("Calculate Variable", style = "default",
              fluidRow(
                column(4, shinyWidgets::pickerInput("efa_agg_vars", "Variables to Calculate:", choices = NULL, multiple = TRUE, width = "100%", options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                column(3, textInput("efa_agg_name", "New Variable Name:", placeholder = "e.g., Factor1_Score")),
                column(3, selectInput("efa_agg_method", "Calculation Method:", choices = c("Mean" = "mean", "Sum" = "sum"))),
                column(2, 
                       actionButton("efa_btn_aggregate", "Calculate", class = "btn-primary", style = "margin-top: 25px; width: 100%;"),
                       actionButton("efa_btn_reset_agg", "Reset", class = "btn-danger btn-sm", style = "margin-top: 5px; width: 100%;")
                )
              )
            )
          )
        )
      )
    ),
    # ===== Pre-Analisis  ======
    tabPanel(
      "KMO & Bartlett Tests",
      uiOutput("efa_tests"),
      tags$div(
        class = "pl-two pl-two-flat",
        tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Sampling adequacy per item"), uiOutput("kmo_item_tbl")),
        tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Scree plot"), plotOutput("scree_plot", height = "320px"))
      )
    ),
    tabPanel(
      "EFA Results",
      tags$div(
        class = "pl-efa-head",
        tags$div(class = "pl-efa-n", tags$label(`for` = "n_factors", "Number of factors"),
                 numericInput("n_factors", label = NULL, value = 3, min = 1, max = 10, width = "84px")),
        tags$div(class = "pl-efa-names", tags$label("Factor names"), uiOutput("factor_names_ui")),
        tags$div(class = "pl-efa-actions",
                 downloadButton("download_loading", "Loadings (.csv)", class = "btn btn-default btn-sm"),
                 downloadButton("download_scores", "Factor scores (.csv)", class = "btn btn-default btn-sm"))
      ),
      tags$div(
        class = "pl-two pl-two-flat pl-two-wide-left",
        tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Summary and loadings"), uiOutput("efa_summary")),
        tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Factor scores"), DTOutput("efa_scores_table"))
      )
    ),

    tabPanel(
      "Score New Data",
      pl_split(
        pl_side(
          width = 3,
          h4("Score New Data"),
          p("Upload new data to calculate factor scores using the fitted EFA model."),
          downloadButton("download_efa_template", "Download Data Template (Excel)", class = "btn-info btn-block"),
          br(),br(),
          fileInput("efa_newdata", "Upload New Data (Excel/CSV)", accept = c(".csv", ".xlsx", ".xls")),
          actionButton("efa_score_newdata_btn", "Calculate Scores", icon = icon("calculator"), class = "btn-success btn-block")
        ),
        pl_main(
          width = 9,
          div(style = "text-align: right; margin-bottom: 5px;",
              downloadButton("download_efa_newscores", "Download New Scores (.csv)", class = "btn-primary btn-sm")),
          DTOutput("efa_newscores_table")
        )
      )
    ),

    tabPanel(
      title = tagList(icon("file-lines"), "Report"),
      pl_report_tab("efa", "efa_generate_preview", "download_report_efa", "efa_report_preview_frame")
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
            tags$br(),
            tags$a("hasandjidu@gmail.com"),
            tags$hr()
          )
        ),
        column(
          width = 8, offset = 2,
          h4("References (R Packages)"),
          uiOutput("package_references_efa"),
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
