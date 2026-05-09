# projectLSA 0.0.9

## CFA/SEM Reporting Module
* Added HTML report generation for CFA and SEM models via R Markdown, including
  model summary narrative, fit index comparison tables, and side-by-side path
  diagrams (initial vs. final model).
* Report automatically detects whether the fitted model is CFA or SEM based on
  the presence of regression paths (`~` operator).
* Report supports configurable decimal separator (period or comma) for
  international conventions.

## Path Plot Enhancements
* Added five new color palettes: Vibrant, Monochrome, Sunset, Rose, and Mint.
* Decimal separator setting now also applies to edge labels on the path diagram.

## UI & UX Improvements
* Citation copy and download buttons aligned and centered on the homepage.
* New runtime dependencies added to Imports: `kableExtra`, `knitr`, `magick`,
  `officer`, `rmarkdown`, `scales`.

# projectLSA 0.0.8
* Removed dependency on the semTools package.
* Re-implemented Average Variance Extracted (AVE), Composite Reliability (CR),
  and Heterotrait–Monotrait Ratio (HTMT) internally using lavaan-based
  computations.
* The HTMT implementation follows the default HTMT2 formulation
  (geometric mean) and produces results identical to semTools::htmt().
* No changes to the public API or user-facing outputs.
* Minor internal refactoring of the Shiny server logic for CFA diagnostics.

# projectLSA 0.0.7
* Minor refinements to the Shiny application codebase to support the new exploratory functionality

# projectLSA 0.0.6

* Extended and enhanced the Latent Class Analysis (LCA) and Latent Profile Analysis (LPA) modules with additional tools for in-depth result exploration.
* Added advanced exploratory features to support deeper inspection of class/profile characteristics, model outputs, and interpretation workflows.
* Improved interactivity and usability of LCA and LPA result views within the Shiny application.
* Internal refinements to the Shiny application codebase to support the new exploratory functionality.
* No changes to the public API or core estimation procedures.

# projectLSA 0.0.5

* Documentation updated and expanded, including README.Rmd, README.md, and rendered README.html.
* Figures reorganized and updated to improve clarity of methodological workflows.
* Minor refinements to package metadata and CRAN submission-related files.
* No changes to the public API or core analytical functionality.

# projectLSA 0.0.3

* Initial release to CRAN.
* Includes a full Shiny-based graphical user interface for:
  - Latent Profile Analysis (LPA)
  - Latent Class Analysis (LCA)
  - Latent Trait Analysis (LTA / IRT)
  - Exploratory Factor Analysis (EFA)
  - Confirmatory Factor Analysis (CFA)
* Includes interactive visualizations, downloadable outputs, and built-in example datasets.
* Provides `run_projectLSA()` as the main entry point for launching the application.
