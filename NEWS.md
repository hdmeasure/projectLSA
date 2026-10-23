# projectLSA 0.2.0

## Tab guide
* Once an analysis has results, the project avatar offers to explain the tab you are looking at:
  what to look at, what it means and what would be a warning sign. The offer appears in every
  method; the explanation is written by the AI assistant from your own results, and when no model
  is set up yet the avatar asks you to add an API key in the AI settings.

## Reports
* Every method can export a **Word (.docx) report** with the tables, the figures produced by the
  analysis, in-text citations and a reference list in APA 7 format (software, methods and the
  works used to judge the results). The report form has three short parts: objectives, method
  (filled from the analysis you ran) and results with interpretation; the AI assistant can draft
  them following plain scientific style and cites only listed works.

## A new HTML interface
* The whole application has a new, light and clean interface with soft animations and hover
  effects. One shared layout is used by every method, so screens look and behave alike.
* **Navigation follows where you are.** On the home screen the sidebar lists the analysis
  methods; inside a method the views (for example Prepare Data & Model, Fit Model Comparison,
  Report) are tabs along the top, with a link back to all methods and a switch to another method.
  In the CFA/SEM module the Path Plot tab replaces the left panel with the plot settings.
* **Room for your data and plots.** The sidebar folds to an icon rail (each icon shows its name
  when you point at it), the control panel of the CFA/SEM module can be hidden, and the options
  panel of every view can be folded away with the handle on its edge.
* The home screen explains where each method fits on a two-by-two map: what you want to
  uncover (groups or a score) against what you measured (continuous or categorical).
* The **decimal separator** (point or comma) is one switch at the top right and applies to the
  whole application and to the reports.
* Tables look like a spreadsheet (thin grid, tight rows, grey header), button and text colours
  were checked for contrast, and drop-down lists are drawn above every panel so that all options
  can be clicked. Model-comparison plots (AIC, BIC, entropy, class size) share one clean style.
* While an analysis runs, the project avatar blinks and waves; the same avatar appears in the AI
  assistant and in progress dialogs. Tables, dialogs, notifications and charts share one style.
* Moving between methods keeps each method's results; a method is no longer started again
  every time it is opened.

## CFA/SEM Module: Path diagram settings
* The path-diagram settings are grouped by purpose: **Show** (scope, estimates, residuals,
  fit indices in the legend), **Layout** (style, layout, rotation, general factor, curved paths,
  split layout), **Size** (canvas, nodes, labels, lines) and **Colors**. The download button
  sits in a toolbar above the diagram.

## CFA/SEM Module: Data Analysis Menu
* The **estimator settings** of the CFA/SEM analysis menu now include an
  **ordinal data option**. When a categorical estimator is selected (WLS, DWLS, ULS,
  PML, WLSM, WLSMV, WLSMVS, ULSM, ULSMV, or ULSMVS), the indicators can be declared
  as ordered categorical variables, so that the model is estimated from polychoric
  correlations and thresholds. The option is switched on by default for these
  estimators, all selected indicators are treated as ordinal unless a subset is
  chosen, and it is hidden for continuous-data estimators (ML, MLR, ...).
* Heterotrait-Monotrait ratios (HTMT) are computed from polychoric correlations when
  the indicators are ordinal.
* **Multi-group CFA** and the **Auto Invariance Test** support ordinal indicators: the
  scalar level constrains thresholds, and all levels use the theta parameterization
  so that configural, metric, scalar, and strict models are directly comparable.
* With ordinal indicators, selecting FIML for missing data applies pairwise deletion.
* The model comparison table, the path diagram legend, the HTML report, and the AI
  assistant context state when a model was estimated with ordinal indicators, and the
  Methodological Guide describes the new option.
* Saved analysis sessions keep the ordinal-data setting.

## AI settings and assistant
* **AI settings moved to the top right** and now manage several models at once. Add models
  from Google Gemini, Groq, OpenRouter, Cerebras and Mistral (free tiers offered by the
  providers), OpenAI and xAI Grok (paid), or any OpenAI-compatible server such as Ollama or
  LM Studio.
* **Automatic mode** uses your first model and moves to the next one when a quota runs out, a
  key is rejected, a model is missing or a service is down; the first model is used again once
  it has recovered. **Single mode** uses only the model you marked as current.
* **Test all models together**: every configured model is tested at the same time, and each
  shows whether it answered and how fast.
* **Switch the current model** from the assistant at any moment, reorder models, switch them
  off, and **edit a model's name, label or key** in place (for example when a provider
  releases a newer version).
* **Load models from provider** lists what a key can use, so model names do not go stale.
* Keys stay in the running session only. Answers show which model produced them and whether a
  backup model was used.

## Report builder
* The Report tab of every method (CFA/SEM, EFA, LTA, LPA, LCA) has a form with title, authors,
  background, objectives, method, interpretation of the results, conclusion, and limitations.
  Each part has **Write with AI**, which drafts it from the method's results or polishes what
  you wrote, and one button drafts every empty part. The text appears in the HTML report.

## Other
* `curl` is now used for the AI requests, replacing `httr`.

# projectLSA 0.1.1

## CFA/SEM Module (major update)
* Added an **Advanced Analysis** tab supporting **Multi-group CFA (MGCFA)** with
  configural, metric (weak), scalar (strong), and strict invariance levels, plus a
  one-click **Auto Invariance Test** that fits and compares all levels.
* Added **Latent Growth Modelling (LGM)** via `lavaan::growth()`.
* Added built-in example datasets for the new modes: `PoliticalDemocracy` (SEM) and
  `Demo.growth` (LGM).
* Added a **Data Summary** panel with descriptive tables, data-type composition, and
  a missingness/correlation heatmap.
* Added **Calculate Variable**: build composite (mean/sum) scores from a simple
  `NewVar = var1, var2, ...` syntax, both for the analysis data and for new data.
* Added **Score New Data**: download an Excel template, upload new cases, and compute
  factor scores from the fitted model.
* Added a **Methodological Guide** with the reporting conventions used by the module.
* Added editable model-comparison table, Heywood-case status flags, and an option to
  use robust/scaled fit indices.
* Reworked the parameter output into a single tab covering factor loadings, regression
  paths, variances, and covariances.
* Path diagram gains a **Display Scope** option (full model vs. structural model only).

## Save & Restore Analysis Sessions
* Every module (CFA, EFA, LTA, LPA, LCA) can now **save the whole analysis session**
  to an `.rds` workspace file and restore it later by uploading that file as the data
  source. The workspace stores the data, the fitted model objects, and the relevant
  input settings.

## HTML Reports for Every Module
* CFA/SEM, EFA, LTA/IRT, LPA, and LCA each gain a **Report Preview** tab: render the
  report in-app first, then download the finished HTML.
* Reports embed the R console output and, when requested, the AI interpretive summary.

## AI Assistant (new)
* Added a floating **AI Assistant** widget available in every module, with three tabs:
  *Ask AI* (conversational Q&A about the current results), *Create Summary*
  (manuscript, APA 7, paragraph, bullet, or table format, in English or Indonesian),
  and *Settings*.
* Supports Google Gemini, OpenAI, Groq, and OpenRouter. The API key is supplied by the
  user at runtime, is never stored by the package, and no request is made unless the
  user provides one.
* The assistant is grounded with reporting rules of thumb for mixture models
  (Nylund et al., 2007; Masyn, 2013), IRT, and CFA/SEM (Hu & Bentler, 1999;
  Hair et al., 2017; DiStefano & Morgan, 2014).
* Optional research context can be typed in or uploaded (txt/pdf/docx) to tailor the
  interpretation, and the generated summary can be pushed into the HTML report.
* The widget can be docked as a tab inside the active module or floated, and the chat
  transcript can be exported as HTML.

## LTA/IRT Module
* Added an **Information & Reliability** tab (test information, marginal reliability,
  conditional standard error of measurement).
* Added a **DIF Analysis** tab with group and anchor-item selection.
* Added **Score New Data** with an Excel template download.

## EFA Module
* Added data summary visualisations, composite-variable calculation, and scoring of
  new data.

## Other
* Added a floating **R Console** widget showing the raw R output behind the results.
* Report previews are now written to a per-session directory and served under a
  per-session resource prefix, removed when the session ends. Previously every
  module wrote to a fixed path inside `tempdir()` and published the whole of
  `tempdir()` over HTTP; because `tempdir()` is shared by all sessions served by
  one R process, concurrent users of a hosted deployment could overwrite and read
  each other's reports.
* New runtime dependencies: `httr`, `jsonlite`, `shinycssloaders`, `writexl`.
  `semTools` and `pdftools` are optional (Suggests).

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
