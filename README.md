
# projectLSA <img src="man/figures/logoProjectLSA.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/projectLSA)](https://CRAN.R-project.org/package=projectLSA)
[![CRAN
release](https://www.r-pkg.org/badges/last-release/projectLSA)](https://CRAN.R-project.org/package=projectLSA)
[![Downloads](https://cranlogs.r-pkg.org/badges/projectLSA)](https://cranlogs.r-pkg.org/badges/projectLSA)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/projectLSA)](https://cranlogs.r-pkg.org/badges/grand-total/projectLSA)

[![R-CMD-check](https://github.com/hdmeasure/projectLSA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hdmeasure/projectLSA/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

<!-- badges: end -->

projectLSA is an R package that provides a complete graphical user
interface (GUI) for conducting **Latent Structure Analysis (LSA)**
through a Shiny application. It integrates multiple latent variable
methods, including:

- **Latent Profile Analysis (LPA)**
- **Latent Class Analysis (LCA)**
- **Latent Trait Analysis (LTA / IRT)**
- **Exploratory Factor Analysis (EFA)**
- **Confirmatory Factor Analysis (CFA)**

All analyses can be performed **without writing any code**, making the
package accessible for researchers, students, and applied analysts.

------------------------------------------------------------------------

## Installation

``` r
# Install from CRAN (when available)
install.packages("projectLSA")

# Install development version from GitHub (optional)
remotes::install_github("hdmeasure/projectLSA")
```

------------------------------------------------------------------------

## Launch the Application

``` r
library(projectLSA)
run_projectLSA()
```

This opens the full Shiny application, including all LSA modules, data
upload, built-in datasets, interactive plots, and reporting features.

------------------------------------------------------------------------

## Video Tutorial

[![projectLSA – Installation and Quick
Start](https://img.youtube.com/vi/Rqj_ZPSXVaA/maxresdefault.jpg)](https://www.youtube.com/watch?v=Rqj_ZPSXVaA)

🎬 *Click the image to watch the installation and quick-start tutorial
for **projectLSA***.

------------------------------------------------------------------------

## Features

### ✔ Latent Profile Analysis (LPA)

- Upload your own dataset or use built-in examples.
- Fit multiple LPA models automatically.
- Compare AIC, BIC, entropy, and class size.
- Visualize the best model with customizable class names.

### ✔ Latent Class Analysis (LCA)

- Supports categorical indicators.
- Fits multiple class solutions.
- Interactive plots with **ggiraph**.
- Probability tables and class membership export.

### ✔ Latent Trait Analysis (LTA / IRT)

- Supports dichotomous and polytomous items.
- Automatically fits Rasch, 2PL, 3PL (or PCM/GRM/GPCM).
- ICC plots, test information, factor scores.
- Multi-dimensional visualization with 3D surfaces and heatmaps.

### ✔ Exploratory Factor Analysis (EFA)

- KMO, Bartlett test, parallel analysis.
- Factor extraction with rotation.
- Factor scores and loading matrix export.
- Clean HTML summaries for clearer interpretation.

### ✔ Confirmatory Factor Analysis (CFA)

- Lavaan model editor.
- Fit measures, loadings, factor scores.
- Fully customized SEM path diagrams.

------------------------------------------------------------------------

## Live Demo (Shiny Application)

All features of **projectLSA** can be explored through an interactive
Shiny web application.

👉 **Launch the live application:**  
<https://measure.shinyapps.io/ProjectLSA/>

The web interface provides access to Latent Profile Analysis (LPA),
Latent Class Analysis (LCA), Confirmatory Factor Analysis (CFA),
Structural Equation Modeling (SEM), and Latent Trait Analysis (IRT),
allowing users to explore the full workflow without local installation.

------------------------------------------------------------------------

## Citation

If you use projectLSA in publications, please cite:

Djidu, H., Retnawati, H., Hadi, S., & Haryanto (2026). *projectLSA:Shiny
application for latent structure analysis with a graphical user
interface. <https://doi.org/10.32614/CRAN.package.projectLSA>*

------------------------------------------------------------------------

## Contributing

Bug reports and feature requests are welcome:

<https://github.com/hdmeasure/projectLSA/issues>

------------------------------------------------------------------------

## License

MIT License © 2026 Hasan Djidu
