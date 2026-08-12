# Fungsi untuk menu Siswa
lta_ui <- function(project) {
    tabsetPanel(
      id = "main_tab_lta",
      
      # --- TAB 1: Prepare Data & Model ----
      tabPanel(
        title = tagList(icon("upload"), "Prepare Data & Model"),
        value = "prepare_tab_lta", 
        sidebarLayout(
          sidebarPanel(
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
                           class = "btn btn-primary", 
                           style = "width: 100% !important;")
          ),
          mainPanel(
            width = 9,
            h5(icon("table"), "Data Preview"),
            DTOutput("data_preview_lta")
          )
        )
      ),
      # ==== Model Comparison
      tabPanel(
        title = tagList(icon("chart-line"), "Model Summary"),
        value = "summary_tab", 
        br(),
        
        column(
          12,
          # === 1️⃣ Model Comparison ===
          h4(icon("table"), "Model Fit Comparison"),
          uiOutput("fit_comparison"),
         
           tags$hr(style = "border-top: 2px solid #bbb;"),
          
          # === 2️⃣ Pilihan Model Terbaik ===
          column(4,
            h4(icon("cog"), "Select the Best Model"),
            p(
              "Choose the model you want to inspect in detail from the options below. ",
              "The best model can be determined by observing the smallest AIC/BIC and the highest item fit count above."),
              uiOutput('itemtype_ui')
            ),
          # === 3️⃣ Item Parameters & Fit ===
          column(8,
          h4(icon("chart-bar"), "Item Parameters & Item Fit Statistics"),
          uiOutput('item_summary')
          ),
          br(),
          br(),
          tags$hr(style = "border-top: 2px solid #bbb;"),
          column(12,
          h4(icon("chart-line"), "Item Characteristic Curve (ICC) and Information Function"),
          uiOutput('icc_info')
          ),
          br(),
          br(),
          br(),
        )
      ),
      tabPanel(
        title=tagList(icon("brain"), "Factor Scores"),
        column(12, uiOutput('fscoreLTA') )
      ),
      tabPanel(
        title = tagList(icon("info"), "Information & Reliability"),
        value = "info_rel_tab",
        uiOutput("info_rel_ui")
      ),
      tabPanel(
        title = tagList(icon("people-arrows"), "DIF Analysis"),
        value = "dif_tab",
        uiOutput("dif_ui")
      ),

      tabPanel(
        title = tagList(icon("file-import"), "Score New Data"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("Score New Data"),
            p("Upload new data to calculate factor scores using the fitted LTA/IRT model."),
            downloadButton("download_lta_template", "Download Data Template (Excel)", class = "btn-info btn-block"),
            br(),br(),
            fileInput("lta_newdata", "Upload New Data (Excel/CSV)", accept = c(".csv", ".xlsx", ".xls")),
            actionButton("lta_score_newdata_btn", "Calculate Scores", icon = icon("calculator"), class = "btn-success btn-block")
          ),
          mainPanel(
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
        title = tagList(icon("file-alt"), " Report Preview"),
        column(12,
               br(),
               div(style = "display: flex; gap: 10px; margin-bottom: 15px;",
                   actionButton("lta_generate_preview", tagList(icon("sync"), " Generate Report Preview"), class = "btn btn-success"),
                   downloadButton("download_report_lta", "Download HTML Report", class = "btn btn-primary")
               ),
               div(
                 style = "border: 1px solid #ddd; border-radius: 4px; padding: 5px; background: #f9f9f9;",
                 uiOutput("lta_report_preview_frame")
               )
        )
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
      )
    )
  }