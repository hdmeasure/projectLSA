# Fungsi untuk menu Siswa
lta_ui <- function(project) {
    tabs <- pl_tabs(
    "main_tab_lta",
      
      # --- TAB 1: Prepare Data & Model ----
      tabPanel(
        title = tagList(icon("upload"), "Prepare Data & Model"),
        value = "prepare_tab_lta", 
        pl_split(
          pl_side(
            width = 3,
            # Tombol kembali ke halaman utama
            actionButton("go_home", 
                         label = tagList(icon("home"), "Main Menu"), 
                         class = "btn btn-danger btn-block",
                         style = "width: 100% !important;"),
            br(),
            #uiOutput("data_source_lta_ui"),
            selectInput("data_source_lta", 
              "Select Data Source:",choices = c(
                "UPLOAD DATA" = "upload",
                "Built in Data: Dichotomous" = "diko",
                "Built in Data: Polytomous" = "poli"),
              selected = "upload"
            ),
            conditionalPanel(
              condition = "input.data_source_lta == 'upload'",
              fileInput("datafile_lta", "Upload Data/Workspace (csv/xlsx/sav/rds)", accept = c(".csv", ".xlsx", ".sav", ".rds")),
              selectInput("datatype", 
                          "Select Data Type:",
                          choices = c("Dicotomous" = "diko",
                                      "Polytomous" = "poli"),
                          selected = 'poli')
            ),
            selectInput("dimension",
                        "Dimension:",
                        choices = c("Unidimensional" = "uni",
                                    "Multidimensional" = "multi"),
                        selected = "uni"),
            uiOutput("id_select_ui"),
            uiOutput("var_select_ui"),
            conditionalPanel(
              condition = "input.dimension == 'multi'",
              tags$label("Define Dimension Name & Item:"),
              uiOutput("lta_model_ui")
            ),
            selectInput("fit_stats", 
                        "Itemfit Stat:",
                        choices = c("S_X2 (complete cases only)" = "S_X2",
                                    "X2" = "X2",
                                    "X2*" = "X2*"),
                        selected = "S_X2"),
            
            actionButton("run_lta",
                         label = tagList(icon("play"), "Run LTA"), 
                         class = "btn btn-success",
                         style = "width: 100% !important; margin-bottom: 10px;"),
            downloadButton("export_lta_rds", "Export Model (.rds)", 
                           class = "btn btn-default", 
                           style = "width: 100% !important;")
          ),
          pl_main(
            width = 9,
            h5(icon("table"), "Data Preview"),
            DTOutput("data_preview_lta")
          )
        )
      ),
      # ==== The views of the chosen model. They are grouped in the top row: Model, Items, Persons, Group tests ====
      tabPanel(title = tagList(icon("scale-balanced"), "Compare models"), value = "lta_cmp",
               tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Model comparison"), uiOutput("fit_comparison"))),
      tabPanel(title = tagList(icon("table-list"), "Item parameters"), value = "lta_items",
               tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Item parameters and item fit"), uiOutput("item_summary"))),
      tabPanel(title = tagList(icon("sliders"), "Loadings"), value = "lta_load",
               tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Standardized loadings and communalities"), uiOutput("lta_loadings"))),
      tabPanel(title = tagList(icon("chart-line"), "Item curves"), value = "lta_curves",
               tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Item characteristic curves"), uiOutput("lta_curves_ui"))),
      tabPanel(title = tagList(icon("wave-square"), "Information"), value = "lta_info",
               tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Information and standard error"), uiOutput("lta_info_ui"))),
      tabPanel(title = tagList(icon("link"), "Local dependence"), value = "lta_ld",
               tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Local dependence (Q3)"), uiOutput("lta_q3"))),
      tabPanel(
        title = tagList(icon("brain"), "Factor scores"), value = "fs_tab",
        column(12, uiOutput('fscoreLTA') )
      ),
      tabPanel(title = tagList(icon("people-arrows-left-right"), "Theta invariance"), value = "lta_thinv", uiOutput("thinv_ui")),
      tabPanel(
        title = tagList(icon("shield-halved"), "Reliability"),
        value = "info_rel_tab",
        uiOutput("info_rel_ui")
      ),
      tabPanel(
        title = tagList(icon("people-arrows"), "DIF Analysis"),
        value = "dif_tab",
        uiOutput("dif_ui")
      ),
      tabPanel(
        title = tagList(icon("equals"), "Item invariance"),
        value = "inv_tab",
        uiOutput("inv_ui")
      ),

      tabPanel(
        title = tagList(icon("file-import"), "Score New Data"),
        pl_split(
          pl_side(
            width = 3,
            h4("Score New Data"),
            p("Upload new data to calculate factor scores using the fitted LTA/IRT model."),
            downloadButton("download_lta_template", "Download Data Template (Excel)", class = "btn-default btn-block"),
            br(),br(),
            fileInput("lta_newdata", "Upload New Data (Excel/CSV)", accept = c(".csv", ".xlsx", ".xls")),
            actionButton("lta_score_newdata_btn", "Calculate Scores", icon = icon("calculator"), class = "btn-success btn-block")
          ),
          pl_main(
            width = 9,
            div(style = "text-align: right; margin-bottom: 5px;",
                downloadButton("download_lta_newscores", "Download New Scores (.csv)", class = "btn-primary btn-sm")),
            DTOutput("lta_newscores_table")
          )
        )
      ),

      # =====================================
      # Report Preview
      # =====================================
      tabPanel(
        title = tagList(icon("file-lines"), "Report"),
      pl_report_tab("lta", "lta_generate_preview", "download_report_lta", "lta_report_preview_frame")
    ),
      
      # ===== INFO ======
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
            uiOutput("package_references_lta"),
            br(),
            div(
              style = "text-align:center;",
              tags$p(
                style = "font-size:13px; color:#777;",
                format(Sys.Date(), "%Y"), 
                "projectLSA. All rights reserved."
              ) ))
        )
      ),
    groups = list(
      Model = c("lta_cmp", "lta_items", "lta_load"),
      Items = c("lta_curves", "lta_info", "lta_ld", "info_rel_tab"),
      Persons = c("fs_tab", "lta_thinv"),
      `Group tests` = c("dif_tab", "inv_tab")
    ),
    group_icons = c(Model = "scale-balanced", Items = "list-check", Persons = "user-group", `Group tests` = "people-arrows"),
    needsmodel = c("lta_cmp", "lta_items", "lta_load", "lta_curves", "lta_info", "lta_ld", "info_rel_tab", "fs_tab", "lta_thinv", "dif_tab", "inv_tab")
    )
  list(controls = NULL, nav = tabs$nav, stage = tagList(
    # the model the views below describe: shown on every view that depends on it
    tags$div(class = "pl-modelbar pl-flat pl-selbar",
             tags$div(class = "pl-selbar-row",
                      tags$div(class = "pl-selbar-label", tags$b("Model to inspect"), tags$small("Compare the models first, then choose one.")),
                      uiOutput("itemtype_ui")),
             uiOutput("lta_model_chips")),
    tabs$stage))
}
