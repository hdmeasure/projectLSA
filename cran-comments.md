## Test environments
- local macOS (R 4.5.x)
- GitHub Actions (ubuntu-latest, windows-latest, macOS-latest)

## R CMD check results
0 errors | 0 warnings | 1 note

## Notes
- This update introduces an HTML report generation module for CFA and SEM analyses, including automatic model-type detection, fit index interpretation, and comparative path diagrams.
- Path plot visualizations have been enhanced with additional color palettes, configurable decimal separators, and improved fit index display formatting.
- A bug causing "duplicated col_keys" errors when rendering HTMT tables has been resolved.
- Several packages previously used at runtime but listed only under Suggests (knitr, rmarkdown) have been moved to Imports. New runtime dependencies (kableExtra, magick, officer, scales, semTools) have also been added to Imports.
- The file README.html is included in the GitHub repository for documentation purposes and is excluded from the CRAN build via .Rbuildignore.
- Some packages listed in Imports are used conditionally in Shiny applications and vignettes, which may not be detected by static code analysis.