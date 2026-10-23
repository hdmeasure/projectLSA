# ==== report_utils.R ====
# Per-session scratch space for generated HTML reports.
#
# tempdir() is shared by every Shiny session served by the same R process. When
# the application is hosted (rather than launched locally by a single user via
# run_projectLSA()), writing reports to a fixed path inside tempdir() means two
# concurrent users overwrite each other's file, and one user's report - which
# embeds their data, model output and any AI summary - can be served into
# another user's browser. addResourcePath() on the raw tempdir() also exposes
# every file in it over HTTP under a guessable name.
#
# Each session therefore gets its own subdirectory, published under its own
# resource prefix, and both are removed when the session ends.
#
# Returns a list with:
#   prefix - the resource prefix to use in a URL (e.g. "cfa_reports_<token>")
#   path   - the filesystem directory to render into
session_report_dir <- function(session, module) {
  token <- session$token
  if (is.null(token) || !nzchar(token)) {
    # testServer() and some non-browser sessions have no token; fall back to a
    # value that is still unique per session object.
    token <- sprintf("%s", format(as.numeric(Sys.time()) * 1000, scientific = FALSE))
    token <- gsub("[^0-9]", "", token)
  }

  prefix <- paste0(module, "_reports_", token)
  path <- file.path(tempdir(), prefix)

  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    addResourcePath(prefix, path)
    session$onSessionEnded(function() {
      try(removeResourcePath(prefix), silent = TRUE)
      unlink(path, recursive = TRUE)
    })
  }

  list(prefix = prefix, path = path)
}

# ==== Word (.docx) report ====
# Renders one of the *_report_word.Rmd templates into a Word file with the APA 7 reference
# document. `params` are the template parameters; the helper files and the figure folder are
# added here. Returns the path of the .docx, or stops with the rendering error.
find_app_file <- function(rel) {
  if (file.exists(rel)) return(normalizePath(rel))
  p <- file.path(system.file("app", package = "projectLSA"), rel)
  if (file.exists(p)) p else stop("Missing report file: ", rel)
}

render_word_report <- function(template, params, out_file, workdir) {
  dir.create(workdir, showWarnings = FALSE, recursive = TRUE)
  tpl <- file.path(workdir, basename(template))
  file.copy(find_app_file(template), tpl, overwrite = TRUE)
  helpers <- file.path(workdir, "helpers.R")
  writeLines(c(readLines(find_app_file("report_refs.R"), warn = FALSE), "", readLines(find_app_file("report_word.R"), warn = FALSE)), helpers, useBytes = TRUE)
  figdir <- file.path(workdir, "figures")
  unlink(figdir, recursive = TRUE)
  ref <- find_app_file(file.path("templates", "apa_reference.docx"))
  rmarkdown::render(
    tpl,
    output_format = rmarkdown::word_document(reference_docx = ref, keep_md = nzchar(Sys.getenv("PL_KEEP_MD"))),
    output_file = out_file, quiet = TRUE,
    params = c(list(helpers = helpers, figdir = figdir), params),
    envir = new.env(parent = globalenv())
  )
  out_file
}
