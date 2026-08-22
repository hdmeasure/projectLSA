# Welcome to the projectLSA Wiki

<img src="https://raw.githubusercontent.com/hdmeasure/projectLSA/master/man/figures/logoProjectLSA.png" align="right" width="140" />

[![CRAN status](https://www.r-pkg.org/badges/version/projectLSA)](https://CRAN.R-project.org/package=projectLSA)
[![Downloads](https://cranlogs.r-pkg.org/badges/projectLSA)](https://cranlogs.r-pkg.org/badges/projectLSA)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

**projectLSA** is a comprehensive R Shiny application designed for integrated **Latent Structure Analysis (LSA)**. It provides an intuitive graphical user interface (GUI) for researchers, educators, and psychometricians to perform complex latent variable modeling without writing a single line of code.

🌐 **Try it Live:** [https://measure.shinyapps.io/ProjectLSA/](https://measure.shinyapps.io/ProjectLSA/)

---

## 📖 Wiki Directory

Navigate through our comprehensive guides to get the most out of projectLSA:

### 🚀 Getting Started
* [[Getting Started]] — Installation, first launch, and basic workflow
* [[FAQ]] — Frequently Asked Questions

### 🔬 Analytical Modules
* [[LPA Module]] — Latent Profile Analysis (continuous indicators)
* [[LCA Module]] — Latent Class Analysis (categorical indicators)
* [[IRT Module]] — Item Response Theory (dichotomous/polytomous items)
* [[EFA Module]] — Exploratory Factor Analysis
* [[CFA-SEM Module]] — Confirmatory Factor Analysis & Structural Equation Modeling

---

## ⚡ Quick Installation

### Stable Version (CRAN)
```r
install.packages("projectLSA")
```

### Development Version (GitHub)
```r
# install.packages("remotes")
remotes::install_github("hdmeasure/projectLSA")
```

### Launch App
```r
library(projectLSA)
run_projectLSA()
```

---

## 🏗️ System Architecture

Our application integrates five major analytical methods under a single, unified interface:

```
┌─────────────────────────────────────────────────┐
│   Web-Based GUI (R Shiny Framework)             │
├─────────────────────────────────────────────────┤
│   Data Input Module (CSV, Excel, SPSS, Stata)   │
├─────┬─────┬─────┬─────┬────────────────────────┤
│ LPA │ LCA │ IRT │ EFA │      CFA/SEM           │
├─────┴─────┴─────┴─────┴────────────────────────┤
│   Standardized Model Comparison Framework       │
├─────────────────────────────────────────────────┤
│   Interactive Visualization (plotly, ggiraph)   │
├─────────────────────────────────────────────────┤
│   Automated Reporting System (HTML Export)      │
└─────────────────────────────────────────────────┘
```

---

## 🎥 Video Tutorial

New to projectLSA? Watch our introductory walkthrough:

[![projectLSA Tutorial](https://img.youtube.com/vi/Rqj_ZPSXVaA/maxresdefault.jpg)](https://www.youtube.com/watch?v=Rqj_ZPSXVaA)

---

## 📝 How to Cite

If you use projectLSA in your research or publications, please cite our peer-reviewed journal article:

> Djidu, H., Retnawati, H., Hadi, S., & Haryanto. (2026). projectLSA: A Shiny Application for Integrated Latent Structure Analysis. *Applied Psychological Measurement*. [https://doi.org/10.1177/01466216261446305](https://doi.org/10.1177/01466216261446305)

**BibTeX:**
```bibtex
@article{Djidu2026projectLSA,
  title   = {projectLSA: A Shiny Application for Integrated Latent Structure Analysis},
  author  = {Djidu, Hasan and Retnawati, Heri and Hadi, Samsul and Haryanto},
  journal = {Applied Psychological Measurement},
  year    = {2026},
  doi     = {10.1177/01466216261446305}
}
```

---

## 🤝 Support & Contribution

- **Found a bug?** Open an issue on our [GitHub Issues](https://github.com/hdmeasure/projectLSA/issues) page.
- **Want to contribute?** We welcome pull requests!
- **Support development:** Consider buying the developer a coffee via [Saweria](https://saweria.co/hasandjidu).

*Copyright © 2026 Hasan Djidu, Heri Retnawati, Samsul Hadi, Haryanto.*
