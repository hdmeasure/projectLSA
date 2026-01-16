homepage_ui <- function() {
  fluidPage(

    div(class="landing",
        div(class="hero",
            div(class="welcome","WELCOME TO"),
            div( class = "app-name", "projectLSA" ),            
            
            tags$p(
              style = "font-size: 12px; margin-top: 2px; color: darkblue;",
              "A comprehensive and interactive R Shiny application designed to support Latent Structure Analysis (LSA) through an intuitive graphical interface for running analyses, visualizing models, and interpreting results—all without requiring users to write any code."
            ),
            div(
              style = "
              background-color: #f5f5f5;
              border-left: 4px solid #4c8bf5;
              padding: 3px 12px;
              border-radius: 6px;
              margin-top: 2px;
              box-shadow: 0 1px 3px rgba(0,0,0,0.1);
              font-size: 12px;
  ",
              HTML(
                '<strong>How to Cite:</strong> 
                 Djidu, H., Retnawati, H., Hadi, S., Haryanto (2026). <em>projectLSA: R Shiny application for latent structure analysis with a graphical user interface</em>. 
                 <a href="https://doi.org/10.32614/CRAN.package.projectLSA" target="_blank">https://doi.org/10.32614/CRAN.package.projectLSA/.</a>
                R package version 0.0.6.'
              )
            )
            
        ),
        br(),
        div(class="subtitle","Choose one projectLSA you want to work on:")
    ),
    br(),
    div(class="quad-container",
        div(class="axis-label x-top","Latent = categorical"),
        div(class="axis-label x-bottom","Latent = scale"),
        div(class="axis-label y-left","Observed = scale <<"),
        div(class="axis-label y-right",">> Observed = categorical"),
        div(class="center-logo", tags$img(src="logoProjectLSA.png")),
        
        div(class="quad-card", id="card_lpa", onclick="$('#go_lpa').click();",
            div(class="project-icon", tags$i(class="fa-solid fa-chart-simple")),
            div(class="project-title","LPA - Latent Profile Analysis"),
            div(class="project-desc","Identify latent profile (unobserved subgroups) within your sample using continuous data."),
            actionButton("go_lpa", label="OPEN LPA", class="btn-pill")
        ),
        div(class="quad-card", id="card_lca", onclick="$('#go_lca').click();",
            div(class="project-icon", tags$i(class="fa-solid fa-layer-group")),
            div(class="project-title","LCA - Latent Class Analysis"),
            div(class="project-desc","Identify latent class (unobserved subgroups) within your sample using categorical data."),
            actionButton("go_lca", label="OPEN LCA", class="btn-pill")
        ),
        div(class="quad-card", id="card_fa",
            div(class="project-icon", tags$i(class="fa-solid fa-sitemap")),
            div(class="project-title","FA - Factor Analysis"),
            div(class="project-desc","Explore latent constructs with exploratory or confirmatory FA using continuous data."),
            
            div(style="display: flex; gap: 10px; justify-content: center; margin-top: 10px; width: 100%;",
                actionButton("go_efa", label="OPEN EFA", width = '50%', class="btn-pill btn-primary"),
                actionButton("go_cfa", label="OPEN CFA", width = '50%', class="btn-pill btn-secondary")
            )
            ),
        
        div(class="quad-card", id="card_lta", onclick="$('#go_lta').click();",
            div(class="project-icon", tags$i(class="fa-solid fa-brain")),
            div(class="project-title","LTA - Latent Trait Analysis"),
            div(class="project-desc","Estimate item and person parameters using LTA approaches using categorical data."),
            actionButton("go_lta", label="OPEN LTA", class="btn-pill")
        )
    ),
    div(
      style = "text-align:center;",
      tags$p(
        style = "font-size:13px; color:#777;",
        format(Sys.Date(), "%Y"), 
        "projectLSA. All rights reserved."
      )
    )
  )
}
