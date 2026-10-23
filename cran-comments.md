## Test environments
- local macOS (aarch64-apple-darwin23, R 4.6.1)
- GitHub Actions (ubuntu-latest, windows-latest, macOS-latest)
- win-builder (R-devel, R-release)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes on this release (0.2.0)
- The Shiny application has a new HTML interface: one shared layout for all methods, a
  context-aware sidebar (methods on the home screen, a method's own sub-menus inside it) and
  folding panels that leave room for data and plots.
- The AI assistant now manages several models at once (automatic fallback or single model,
  parallel testing, switching and editing). Requests are made with `curl`; `httr` is no longer
  needed. No request is ever made unless the user enters their own API key or endpoint at
  runtime; keys are kept in the running session only and are never written to disk.
- Every method's Report tab has a form whose parts can be drafted with the assistant; the text
  is included in the HTML report.
- The CFA/SEM analysis menu offers an ordinal-data option for categorical estimators.
- New Imports: curl. Removed from Imports: httr.
