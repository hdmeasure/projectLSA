## Test environments
- local macOS (R 4.6.x)
- GitHub Actions (ubuntu-latest, windows-latest, macOS-latest)

## R CMD check results
0 errors | 0 warnings | 1 note

## Notes
- This release substantially extends the CFA/SEM module: multi-group CFA with
  configural/metric/scalar/strict invariance testing (including an automated
  all-levels comparison), latent growth modelling via `lavaan::growth()`, data
  summary visualisations, composite-variable calculation, and scoring of new data
  from a fitted model.
- Every module (CFA/SEM, EFA, LTA/IRT, LPA, LCA) can now save an analysis session to
  an `.rds` workspace file and restore it later, and can render an HTML report via
  R Markdown with an in-app preview before download.
- A new optional AI assistant lets users interpret their own results through an
  external large language model. No network request is ever made unless the user
  explicitly enters their own API key at runtime; the key is held only in the running
  session, is never written to disk by the package, and no default endpoint or key is
  bundled. The package remains fully functional with the feature unused.
- New runtime dependencies added to Imports: httr, jsonlite (AI assistant HTTP calls),
  shinycssloaders (loading indicators), and writexl (Excel templates for scoring new
  data). semTools and pdftools moved to/added under Suggests and are used only behind
  `requireNamespace()` guards.
- The file README.html is included in the GitHub repository for documentation purposes
  and is excluded from the CRAN build via .Rbuildignore.
- Some packages listed in Imports are used conditionally in Shiny applications and
  vignettes, which may not be detected by static code analysis.
