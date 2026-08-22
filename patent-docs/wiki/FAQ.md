# Frequently Asked Questions (FAQ)

---

## Installation & Setup

### How do I install projectLSA?
You can install the stable version from CRAN:
```r
install.packages("projectLSA")
```
Or the development version from GitHub:
```r
remotes::install_github("hdmeasure/projectLSA")
```

### Can I use projectLSA without installing R?
Yes! You can use the online version hosted on ShinyApps.io:
👉 **[https://measure.shinyapps.io/ProjectLSA/](https://measure.shinyapps.io/ProjectLSA/)**

### I'm getting an error when trying to run the app. What should I do?
Make sure all dependencies are installed. Try running:
```r
install.packages("projectLSA", dependencies = TRUE)
```
If you still have issues, please check your R version (4.0.0+ is recommended) or submit a bug report on GitHub.

---

## Data & Formats

### What data formats does projectLSA support?
The application supports four common data formats:
1. **CSV** (`.csv`) — Comma-separated values
2. **Excel** (`.xlsx`, `.xls`) — Microsoft Excel spreadsheets
3. **SPSS** (`.sav`) — SPSS data files
4. **Stata** (`.dta`) — Stata data files

### How should my data be formatted?
- **For LPA and EFA/CFA:** Use continuous/numeric data.
- **For LCA:** Use categorical data (coded as integers like 1, 2, 3).
- **For IRT:** Use dichotomous (0/1) or polytomous (ordered integers) data.
- **Missing Data:** Most modules handle missing data, but it's best to specify how your missing data is coded (e.g., `NA` or `-99`).

### Can I practice without my own data?
Yes! Every module includes built-in example datasets so you can try out the features immediately.

---

## Analysis Modules

### What is the difference between LPA and LCA?
- **Latent Profile Analysis (LPA)** is used when your observed indicator variables are **continuous** (scale/interval data).
- **Latent Class Analysis (LCA)** is used when your observed indicator variables are **categorical** (nominal, ordinal, binary).

### What is the difference between CFA and SEM?
- **CFA (Confirmatory Factor Analysis)** tests a measurement model (how well your items measure latent factors).
- **SEM (Structural Equation Modeling)** includes both the measurement model AND structural paths (regression relationships) between the latent factors themselves.

### How does the app know if I'm running CFA or SEM?
The CFA/SEM module automatically detects your model type based on the lavaan syntax you provide. If you include regression paths (`~` between latent variables), it detects it as an SEM model and adjusts the reporting accordingly.

---

## Features & Customization

### Can I change the decimal separator?
Yes! projectLSA uniquely supports international decimal conventions. You can toggle between a period (`0.95`) and a comma (`0,95`) in the sidebar, which updates all tables, path diagrams, and reports.

### Can I customize the path diagrams?
Absolutely. The CFA/SEM module offers extensive customization for path diagrams, including 12 color palettes (like Blue-Yellow, Vibrant, Monochrome), layout algorithms, node sizes, edge labels, and an embedded fit index box.

### How do I export my results?
- **Plots:** You can download most plots as PNG files.
- **Tables:** Data tables can be copied, printed, or exported as CSV/Excel.
- **Reports:** The CFA/SEM module generates comprehensive, APA-formatted HTML reports summarizing your entire analysis.

---

## Licensing & Citation

### Is projectLSA free?
Yes, projectLSA is open-source software released under the MIT License.

### How should I cite projectLSA in my research?
Please cite our published paper:

> Djidu, H., Retnawati, H., Hadi, S., & Haryanto. (2026). projectLSA: A Shiny Application for Integrated Latent Structure Analysis. *Applied Psychological Measurement*. https://doi.org/10.1177/01466216261446305

---

⬅️ Back to [[Home]]
