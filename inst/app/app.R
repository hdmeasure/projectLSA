# --- Project Latent Structure Analysis (Project LSA)  ---
# Author: Hasan Djidu

# ==== Load UI & Server Components ====
source("ui_helpers.R")
source("ui_module.R")
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
source("plot_theme.R")
source("plot_helpers.R")
source("followup.R")
source("downloadPlot.R")
source("report_refs.R")
source("report_word.R")
source("report_utils.R")

# ==== AI Assistant ====
source("ai_helper.R")
source("ai_settings_ui.R")
source("ai_widget_ui.R")
source("serverCoach.R")
source("serverAISettings.R")
source("serverAIWidget.R")
source("serverReportForm.R")
source("console_widget_ui.R")

# ==== Main Library ======
library(shiny)
library(shinyWidgets)
library(jsonlite)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(plotly)
library(tidyr)
library(shinyBS)
library(haven)

options(shiny.maxRequestSize = 500 * 1024^2) # 300 MB

# ===== UI =====
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"),
    # the file's modification time in the URL makes browsers reload edited assets
    tags$link(rel = "stylesheet", href = paste0("projectlsa.css?v=", as.integer(file.mtime("www/projectlsa.css")))),
    tags$script(src = paste0("projectlsa.js?v=", as.integer(file.mtime("www/projectlsa.js"))))
  ),
  # Permanent HTML shell (templates/app_shell.html); the module is rendered into its slots
  pl_shell(
    "app_shell.html",
    subnav = uiOutput("pl_subnav"),
    controls = uiOutput("pl_controls"),
    stage = uiOutput("pl_stage")
  ),
  ai_settings_ui(),
  ai_widget_ui(),
  console_widget_ui()
)
# ==== Server =====
server <- function(input, output, session) {

  # Every analysis module keeps its own AI context and R console text, so results
  # stay with their module when the user moves around. The global `ai_context` and
  # `console_context` used by the assistant and the console widget mirror whichever
  # module is open.
  module_ids <- c("lpa", "lca", "lta", "efa", "cfa")
  mod_ctx <- stats::setNames(lapply(module_ids, function(m) reactiveValues(results_text = "", module = "", ai_report_text = NULL)), module_ids)
  mod_console <- stats::setNames(lapply(module_ids, function(m) reactiveValues(text = "No R output yet.")), module_ids)
  ai_context <- reactiveValues(results_text = "", module = "", ai_report_text = NULL)

  # AI settings (several models, shared by the whole app) and the floating assistant
  ai_state <- reactiveValues(slots = list(), mode = "auto", primary = NULL, seq = 0L, editing = NULL)
  session$userData$ai_state <- ai_state     # also used by the report forms
  server_ai_settings(input, output, session, ai_state)
  server_ai_widget(input, output, session, ai_context, ai_state)

  # Global R console context
  console_context <- reactiveValues(text = "No R output yet.")
  output$global_console_output <- renderPrint({
    cat(console_context$text)
  })

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

  # === Render the active module into the shell ===
  # Each module returns list(controls, nav, stage): an optional persistent control
  # panel, its sidebar views, and the stage itself.
  module_parts <- reactive({
    switch(project(),
      "lca" = lca_mod("lca"),
      "lpa" = lpa_mod("lpa"),
      "lta" = lta_mod("lta"),
      "efa" = efa_mod("efa"),
      "cfa" = cfa_mod("cfa"),
      home_mod("home")
    )
  })
  output$pl_controls <- renderUI(module_parts()$controls)
  output$pl_subnav <- renderUI(module_parts()$nav)
  output$pl_stage <- renderUI(module_parts()$stage)

  module_titles <- list(
    home = list(title = "Home", sub = "Choose a method to get started."),
    lpa  = list(title = "Latent Profile Analysis", sub = "Subgroups from continuous indicators."),
    lca  = list(title = "Latent Class Analysis", sub = "Subgroups from categorical indicators."),
    lta  = list(title = "Latent Trait Analysis (IRT)", sub = "Item and person parameters."),
    efa  = list(title = "Exploratory Factor Analysis", sub = "The factor structure behind your items."),
    cfa  = list(title = "CFA / SEM", sub = "Test a measurement or structural model.")
  )
  observeEvent(project(), {
    session$sendCustomMessage("pl_module", c(list(id = project()), module_titles[[project()]]))
  })
  # === Module servers: each starts once per session ===
  # Starting a module again every time it is opened would register its observers a
  # second time (every run would then happen twice). Once started, a module keeps its
  # results; only its screen is drawn again when it is reopened.
  started <- character(0)
  observeEvent(project(), {
    current <- project()
    if (current == "home" || current %in% started) return()
    started <<- c(started, current)
    ctx <- mod_ctx[[current]]
    con <- mod_console[[current]]
    switch(current,
      "lpa" = server_lpa(input, output, session, ctx, con),
      "efa" = server_efa(input, output, session, ctx, con),
      "cfa" = server_cfa(input, output, session, ctx, con),
      "lca" = server_lca(input, output, session, ctx, con),
      "lta" = server_lta(input, output, session, ctx, con)
    )
  })

  # Mirror the open module into the global context and console
  observe({
    m <- project()
    if (m %in% module_ids) {
      ai_context$results_text <- mod_ctx[[m]]$results_text
      ai_context$module <- mod_ctx[[m]]$module
      console_context$text <- mod_console[[m]]$text
    } else {
      ai_context$results_text <- ""
      ai_context$module <- ""
      console_context$text <- "No R output yet."
    }
  })
  server_coach(input, output, session, ai_context, ai_state, project, module_ids)
  observeEvent(project(), { if (project() %in% module_ids) ai_context$ai_report_text <- mod_ctx[[project()]]$ai_report_text })
  # a summary added from the assistant belongs to the module that is open
  observeEvent(ai_context$ai_report_text, {
    m <- isolate(project())
    if (m %in% module_ids) mod_ctx[[m]]$ai_report_text <- ai_context$ai_report_text
  }, ignoreNULL = FALSE)
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
