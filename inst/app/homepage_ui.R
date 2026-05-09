homepage_ui <- function() {
  
  fluidPage(

    div(class="landing",
        div(class="hero",
            div(class="welcome","WELCOME TO"),
            div( class = "app-name", "projectLSA" ),            
            div(

              HTML(
                '<div style="text-align: left; padding: 8px; background-color: rgba(255,255,255,0.1); border-left: 3px solid #ccc; margin-top: 5px; font-size: 10px; color: #555;">
                 <strong>How to Cite:</strong><br>
                 Djidu, H., Retnawati, H., Hadi, S., &amp; Haryanto. (2026). <em>projectLSA: A Shiny Application for Integrated Latent Structure Analysis</em>. Applied Psychological Measurement. 
                 <a href="https://doi.org/10.1177/01466216261446305" target="_blank" style="color: #0077dd;">https://doi.org/10.1177/01466216261446305</a><br>
                 Djidu, H., Retnawati, H., Hadi, S., Haryanto (2026). <em>projectLSA: R Shiny application for latent structure analysis with a graphical user interface</em>. 
                 <a href="https://doi.org/10.32614/CRAN.package.projectLSA" target="_blank" style="color: #0077dd;">https://doi.org/10.32614/CRAN.package.projectLSA</a>. R package.
                 </div>'
              ),
              br(),
              div(style = "display: flex; gap: 8px; justify-content: center; align-items: center; flex-wrap: wrap; margin-top: 5px;",
                  downloadButton("download_ris_home", "Download RIS", class = "btn btn-info btn-sm", style = "margin: 0 !important;"),
                  actionButton("btn_donate_home", "Support projectLSA", icon = icon("heart"), class = "btn btn-danger btn-sm btn-glow", style = "margin: 0 !important;", onclick="$(this).removeClass('btn-glow');")
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
