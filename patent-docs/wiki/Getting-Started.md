# Getting Started

This guide will help you install **projectLSA** and run your first analysis.

---

## Requirements

- **R** version 4.0.0 or higher
- A modern web browser (Chrome, Firefox, Safari, Edge)

---

## Installation

### Option 1: From CRAN (Recommended)

```r
install.packages("projectLSA")
```

### Option 2: From GitHub (Latest Development Version)

```r
# Install remotes if needed
install.packages("remotes")
remotes::install_github("hdmeasure/projectLSA")
```

### Option 3: Online (No Installation)

Visit the live application directly:
👉 **https://measure.shinyapps.io/ProjectLSA/**

---

## Launching the Application

```r
library(projectLSA)
run_projectLSA()
```

This will open the Shiny application in your default web browser.

---

## First Steps

### 1. Choose a Module

From the homepage, select one of the analytical modules:

| Module | Best For |
|--------|----------|
| **LPA** | Identifying subgroups from continuous/scale data |
| **LCA** | Identifying subgroups from categorical data |
| **IRT** | Evaluating test items and measuring latent traits |
| **EFA** | Discovering factor structures (exploratory) |
| **CFA/SEM** | Testing hypothesized measurement models |

### 2. Load Your Data

Each module provides two options:
- **Upload your own data** — Supports CSV, Excel (.xlsx), SPSS (.sav), and Stata (.dta)
- **Use built-in examples** — Pre-loaded datasets for demonstration

### 3. Configure and Run

- Select your variables
- Set model parameters
- Click the **Run** button
- View results in tables, plots, and diagnostic panels

### 4. Export Results

- Download plots as PNG images
- Export tables and data
- Generate HTML reports (CFA/SEM module)

---

## Supported File Formats

| Format | Extension | R Package Used |
|--------|-----------|---------------|
| CSV | `.csv` | base R |
| Excel | `.xlsx`, `.xls` | `readxl` |
| SPSS | `.sav` | `haven` |
| Stata | `.dta` | `haven` |

---

## Troubleshooting

### Application doesn't start

Make sure all dependencies are installed:
```r
install.packages("projectLSA", dependencies = TRUE)
```

### Plots don't render

Try updating your R and package versions:
```r
update.packages(ask = FALSE)
```

### Need help?

- 🐛 [Report a bug](https://github.com/hdmeasure/projectLSA/issues)
- 📧 Contact: hasandjidu@gmail.com

---

⬅️ Back to [[Home]]
