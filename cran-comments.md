## Test environments
- local macOS (R 4.5.x)
- GitHub Actions (ubuntu-latest, windows-latest, macOS-latest)

## R CMD check results
0 errors | 0 warnings | 1 note

## Notes
- This is a maintenance update to improve package robustness and CRAN compatibility.
- The dependency on the semTools package has been removed. Functionality for AVE, composite reliability, and HTMT has been re-implemented internally using lavaan to ensure identical results while avoiding reliance on archived or unmaintained packages.
- The file README.html is included in the GitHub repository for documentation purposes and is excluded from the CRAN build via .Rbuildignore.
- Some packages listed in Imports are used conditionally in Shiny applications and vignettes, which may not be detected by static code analysis.