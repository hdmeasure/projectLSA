# --- Project Latent Structure Analysis (Project LSA)  ---
# Author: Hasan Djidu

# ==== Load UI & Server Components ====
source("ui_module.R")
source("homepage_ui.R")
source("lca_ui.R")
source("lpa_ui.R")
source("efa_ui.R")
source("cfa_ui.R")
source("lta_ui.R")
source("plotinfose.R")
source("lta_info_vis.R")

# ==== Load Logic / Server Modules ====
source("serverEFA.R")
source("serverCFA.R")
source("serverLCA.R")
source("serverLPA.R")
source("serverLTA.R")

# ==== Misc utilities ====
source("reference_list.R")
source("simDataDesc.R")
source("downloadPlot.R")
source("styleCSS.R")

# ==== Main Library ======
library(shiny)
library(shinyWidgets)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(plotly)
library(tidyr)
options(shiny.maxRequestSize = 500 * 1024^2) # 300 MB

# ===== UI =====
ui <- fluidPage(
  styleCSS,
  uiOutput("mainUI")
)
# ==== Server =====
server <- function(input, output, session) {
  observeEvent(TRUE,
    {
      showModal(modalDialog(
        title = NULL,
        div(
          style = "text-align:center; line-height:1.6;",
          tags$img(src = "logoProjectLSA.png", height = "70px"),
          tags$h3("Welcome to projectLSA"),
          tags$p(
            "projectLSA is an interactive R Shiny application designed to support ",
            "latent structure analysis for research & teaching purposes."
          ),
          HTML(
            "
         If you use this application in academic or research work, please cite it as follows:<br>
         <strong>In-text citation:</strong>
         <span style='color:#2563eb;'>(Djidu et al., 2026)</span><br><br>
         <strong>Reference:</strong><br>
         Djidu, H., Retnawati, H., Hadi, S., &amp; Haryanto. (2026).
         <em>projectLSA: A Shiny Application for Integrated Latent Structure Analysis</em>.
         Applied Psychological Measurement.
         <a href='https://doi.org/10.1177/01466216261446305' target='_blank'>
         https://doi.org/10.1177/01466216261446305</a><br><br>
         Djidu, H., Retnawati, H., Hadi, S., Haryanto (2026). <em>projectLSA: R Shiny application for latent structure analysis with a graphical user interface</em>.
         <a href='https://doi.org/10.32614/CRAN.package.projectLSA' target='_blank'>https://doi.org/10.32614/CRAN.package.projectLSA</a>. R package."
          )
        ),
        footer = tagList(
          actionButton("btn_donate_home_modal", "Support Us (Donate)", icon = icon("heart"), class = "btn btn-danger btn-sm btn-glow", onclick = "$(this).removeClass('btn-glow');"),
          modalButton("START")
        ),
        easyClose = TRUE,
        size = "l"
      ))
    },
    once = TRUE
  )

  # Download RIS handler for the homepage
  output$download_ris_home <- downloadHandler(
    filename = function() {
      "projectLSA_citation.ris"
    },
    content = function(file) {
      ris_text <- c(
        "TY  - JOUR",
        "AU  - Djidu, Hasan",
        "AU  - Retnawati, Heri",
        "AU  - Hadi, Samsul",
        "AU  - Haryanto",
        "PY  - 2026",
        "DA  - 2026/04/20",
        "TI  - projectLSA: A Shiny Application for Integrated Latent Structure Analysis",
        "JO  - Applied Psychological Measurement",
        "DO  - 10.1177/01466216261446305",
        "UR  - https://doi.org/10.1177/01466216261446305",
        "ER  - "
      )
      writeLines(ris_text, file)
    }
  )

  # Handlers for donation buttons on homepage / welcome modal
  show_donate_modal <- function() {
    showModal(modalDialog(
      title = "Support projectLSA",
      tags$div(
        style = "text-align: center;",
        tags$h4("Help Us Keep Growing!"),
        tags$p("If you find projectLSA helpful for your research, consider supporting its continuous development."),
        tags$div(
          style = "background-color: #f8f9fa; border: 2px dashed #17a2b8; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
          tags$strong("Support via Bank BNI:", style = "color: #17a2b8; font-size: 16px;"), tags$br(),
          tags$span("0331355050", style = "font-size: 24px; font-weight: bold; color: #d32f2f; letter-spacing: 2px;"), tags$br(),
          tags$em("Account Name: Hasan Djidu"), tags$br(), tags$br(),
          tags$strong("International Transfer (SWIFT):", style = "color: #17a2b8; font-size: 14px;"), tags$br(),
          tags$span("BNINIDJA", style = "font-size: 16px; font-weight: bold; color: #d32f2f;")
        )
      ),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  }

  observeEvent(input$btn_donate_home, {
    show_donate_modal()
  })
  observeEvent(input$btn_donate_home_modal, {
    show_donate_modal()
  })

  # === reactive value to save current project ===
  project <- reactiveVal("home")

  # === observe event from homepage ===
  observeEvent(input$go_lca, {
    project("lca")
  })
  observeEvent(input$go_lpa, {
    project("lpa")
  })
  observeEvent(input$go_lta, {
    project("lta")
  })
  observeEvent(input$go_efa, {
    project("efa")
  })
  observeEvent(input$go_cfa, {
    project("cfa")
  })
  observeEvent(input$go_home, {
    project("home")
  })

  # === Render main UI ===
  output$mainUI <- renderUI({
    req(project())
    fluidPage(
      switch(project(),
        "lca" = lca_mod("lca"),
        "lpa" = lpa_mod("lpa"),
        "lta" = lta_mod("lta"),
        "efa" = efa_mod("efa"),
        "cfa" = cfa_mod("cfa"),
        home_mod("home")
      )
    )
  })
  # === Logic server: jalankan modul sesuai project aktif ===
  observeEvent(project(), {
    current <- project()
    switch(current,
      "lpa" = server_lpa(input, output, session),
      "efa" = server_efa(input, output, session),
      "cfa" = server_cfa(input, output, session),
      "lca" = server_lca(input, output, session),
      "lta" = server_lta(input, output, session),
      NULL
    )
  })
  # LPA
  output$package_references_lpa <- renderUI({
    render_package_refs(c("shiny", "tidyverse", "tidyLPA", "DT", "readxl", "dplyr", "ggplot2"))
  })
  # LCA
  output$package_references_lca <- renderUI({
    render_package_refs(c("shiny", "tidyverse", "poLCA", "glca", "DT", "readxl", "dplyr", "ggplot2", "ggiraph"))
  })
  # LTA / IRT
  output$package_references_lta <- renderUI({
    lta_reference <- list(
      Desjardins_Bulut_2018 = "Desjardins, C. D., & Bulut, O. (2018). <em>Handbook of educational measurement and psychometrics using R</em> (1st ed.). Chapman & Hall/CRC."
    )
    render_package_refs(c("shiny", "tidyverse", "mirt", "DT", "readxl", "dplyr", "ggplot2"),
      manual_refs = lta_reference
    )
  })
  # EFA
  output$package_references_efa <- renderUI({
    render_package_refs(c("shiny", "tidyverse", "psych", "DT", "readxl", "dplyr", "ggplot2"))
  })
  # CFA References
  output$package_references_cfa <- renderUI({
    # Define manual references
    manual_refs <- list(
      Alamer_2025 = "Alamer, A. (2025). Structural equation modeling (SEM) in L2 writing research: Simple tutorial and useful recommendations. <em>Research Methods in Applied Linguistics</em>, 4(2), 100202. <a href='https://doi.org/10.1016/j.rmal.2025.100202' target='_blank'>https://doi.org/10.1016/j.rmal.2025.100202</a>",
      Hair_2019 = "Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2019). <em>Multivariate data analysis</em> (Eighth edition). Cengage.",
      Hu_1999 = "Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. <em>Structural Equation Modeling: A Multidisciplinary Journal</em>, 6(1), 1–55. <a href='https://doi.org/10.1080/10705519909540118' target='_blank'>https://doi.org/10.1080/10705519909540118</a>",
      Schumacker_2008 = "Schumacker, R. E., & Lomax, R. G. (2008). <em>A beginner's guide to structural equation modeling</em> (2nd ed.). Psychology Press.",
      Kline_2016 = "Kline, R. B. (2016). <em>Principles and practice of structural equation modeling</em> (4th ed.). Guilford Press.",
      Brown_2015 = "Brown, T. A. (2015). <em>Confirmatory factor analysis for applied research</em> (2nd ed.). Guilford Press."
    )

    render_package_refs(
      pkgs = c("shiny", "tidyverse", "psych", "lavaan", "DT", "readxl", "dplyr", "semPlot", "semptools", "semTools"),
      manual_refs = manual_refs
    )
  })
}

shinyApp(ui, server)
