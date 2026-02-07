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
options(shiny.maxRequestSize = 300 * 1024^2)  # 300 MB

# ===== UI =====
ui <- fluidPage(
  styleCSS,
  uiOutput("mainUI")
)
# ==== Server =====
server <- function(input, output, session) {
  observeEvent(TRUE, {
    showModal(modalDialog(
      title = NULL,
      div(
        style="text-align:center; line-height:1.6;",
        tags$img(src="logoProjectLSA.png", height="70px"),
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
         <em>projectLSA: R Shiny application for latent structure analysis with a graphical user interface</em>.
         <a href='https://doi.org/10.32614/CRAN.package.projectLSA' target='_blank'>
         https://doi.org/10.32614/CRAN.package.projectLSA</a>.
         R package (Version 0.0.7)."
        )
      ),
      footer = modalButton("START"),
      easyClose = TRUE,
      size = "l"
    ))
  }, once = TRUE)
  
  
  
  # === reactive value to save current project ===
  project <- reactiveVal("home")

  # === observe event from homepage ===
  observeEvent(input$go_lca, { project("lca") })
  observeEvent(input$go_lpa, { project("lpa") })
  observeEvent(input$go_lta, { project("lta") })
  observeEvent(input$go_efa, { project("efa") })
  observeEvent(input$go_cfa, { project("cfa") })
  observeEvent(input$go_home, { project("home") })

  # === Render main UI ===
  output$mainUI <- renderUI({
    req(project())
    fluidPage(
      switch(
        project(),
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
    switch(
      current,
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
      render_package_refs(c("shiny", "tidyverse", "poLCA","glca", "DT", "readxl", "dplyr","ggplot2", "ggiraph"))
    })
    # LTA / IRT
    output$package_references_lta <- renderUI({
      lta_reference <- list(
        Desjardins_Bulut_2018 = "Desjardins, C. D., & Bulut, O. (2018). <em>Handbook of educational measurement and psychometrics using R</em> (1st ed.). Chapman & Hall/CRC."
      )
      render_package_refs(c("shiny", "tidyverse", "mirt", "DT", "readxl", "dplyr","ggplot2"),
                          manual_refs = lta_reference)
    })
    # EFA
    output$package_references_efa <- renderUI({
      render_package_refs(c("shiny", "tidyverse", "psych","DT", "readxl", "dplyr", "ggplot2"))
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
        pkgs = c("shiny", "tidyverse", "psych", "lavaan", "DT", "readxl", "dplyr", "semPlot", "semptools",'semTools'),
        manual_refs = manual_refs
      )
    })

}

shinyApp(ui, server)
