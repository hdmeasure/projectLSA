# serverReportForm.R
# Server side of the report form (see pl_report_form()). Returns a reactive list
# with the text of every part, for the report template.
#   mod         module id used as the prefix of the input ids ("cfa", "lpa", ...)
#   ai_context  the module's context: results_text and module name

server_report_form <- function(input, output, session, mod, ai_context, method_ctx = NULL) {
  keys <- vapply(pl_report_fields(), function(f) f$key, character(1))
  ai_keys <- vapply(Filter(function(f) !identical(f$ai, FALSE), pl_report_fields()), function(f) f$key, character(1))
  needs_results <- c("results")
  id <- function(prefix, k) paste0(mod, prefix, k)

  notes <- reactive({
    vals <- lapply(keys, function(k) trimws(input[[id("_rf_", k)]] %||% ""))
    stats::setNames(vals, keys)
  })

  guidance <- c(
    objectives = "The research objective or questions of this analysis as two to four short sentences, written from the module and the results provided. State what the analysis sets out to find out; do not describe results.",
    results    = "The results and their interpretation in two to four paragraphs: report the key statistics, judge them against the usual cut-offs (name the source from the allowed list), and say what they mean for the objective. Use only the numbers provided."
  )
  names_pretty <- c(objectives = "objectives", method = "method", results = "results and interpretation")

  # Writing rules taken from the author's style standard for scientific prose
  style_rules <- pl_writing_rules

  clean <- function(x, key) {
    x <- gsub("\\*\\*|__|`", "", x)
    x <- gsub("(?m)^\\s{0,3}#{1,6}\\s*", "", x, perl = TRUE)
    x <- trimws(x)
    if (identical(key, "title")) x <- trimws(gsub("^[\"'“]|[\"'”]$", "", strsplit(x, "\n")[[1]][1]))
    x
  }

  write_field <- function(k) {
    st <- session$userData$ai_state
    if (is.null(st) || !any(vapply(isolate(st$slots), ai_slot_usable, logical(1)))) {
      showNotification("Add an AI model first: open the AI settings at the top right.", type = "warning")
      return(FALSE)
    }
    res_text <- isolate(ai_context$results_text) %||% ""
    if (k %in% needs_results && !nzchar(trimws(res_text))) {
      showNotification("Run the analysis first, so the assistant has results to work from.", type = "warning")
      return(FALSE)
    }
    lang <- input[[paste0(mod, "_rf_lang")]] %||% "English"
    own <- trimws(input[[id("_rf_", k)]] %||% "")
    others <- notes()
    others <- others[setdiff(names(others), c(k, "authors"))]
    others <- others[nzchar(unlist(others))]
    ctx_lines <- if (length(others)) paste0("- ", names(others), ": ", unlist(others), collapse = "\n") else "(nothing written yet)"
    prompt <- paste0(
      "Module: ", isolate(ai_context$module) %||% mod, "\n",
      "Write the following part of a research report: ", names_pretty[[k]], ".\n",
      "What to write: ", guidance[[k]], "\n",
      "Language: ", lang, "\n",
      if (nzchar(own)) paste0("The author has already written this text. Improve it and keep their intent:\n", own, "\n") else "",
      "Other parts already written by the author:\n", ctx_lines, "\n\n",
      "Analysis results (the only source of numbers you may use):\n", substr(res_text, 1, 12000)
    )
    hint <- module_cite_hint(mod)
    extra <- paste0("Write only the requested text: plain paragraphs, no preamble and no closing remarks. Never invent data, numbers or references.\n",
                    style_rules, "\nAllowed citations (author-date, in parentheses): ", if (length(hint)) paste(hint, collapse = "; ") else "none", ".")
    r <- ai_ask_state(st, prompt, extra)
    if (!isTRUE(r$ok)) {
      showNotification(r$message, type = "error", duration = 8)
      return(FALSE)
    }
    txt <- clean(r$text, k)
    if (identical(k, "title")) updateTextInput(session, id("_rf_", k), value = txt) else updateTextAreaInput(session, id("_rf_", k), value = txt)
    session$userData$rf_last <- list(mod = mod, key = k, text = txt, via = r$via$model, prompt = prompt)   # handy for tests
    TRUE
  }

  for (k in ai_keys) local({
    key <- k
    observeEvent(input[[id("_rf_ai_", key)]], {
      showNotification(paste0("Writing the ", names_pretty[[key]], " ..."), id = "rf_note", duration = NULL, type = "default")
      ok <- write_field(key)
      removeNotification("rf_note")
      if (ok) showNotification("Done. Read it through and edit as you like.", type = "message", duration = 4)
    })
  })

  fill_method <- function() {
    ctx <- if (is.function(method_ctx)) tryCatch(method_ctx(), error = function(e) NULL) else NULL
    if (is.null(ctx)) { showNotification("Run the analysis first, so the method can be described.", type = "warning"); return(FALSE) }
    txt <- tryCatch(method_text(mod, ctx), error = function(e) "")
    if (!nzchar(txt)) return(FALSE)
    updateTextAreaInput(session, id("_rf_", "method"), value = txt)
    TRUE
  }
  observeEvent(input[[id("_rf_fill_", "method")]], {
    if (fill_method()) showNotification("The method was filled from the analysis. Edit it as you like.", type = "message", duration = 4)
  })

  observeEvent(input[[paste0(mod, "_rf_fill_all")]], {
    if (!nzchar(notes()[["method"]])) fill_method()
    empty <- Filter(function(k) !nzchar(notes()[[k]]), ai_keys)
    if (!length(empty)) return(showNotification("Every part already has text. Use the buttons beside a part to improve it.", type = "message"))
    done <- 0L
    withProgress(message = "Drafting the report", value = 0, {
      for (k in empty) {
        incProgress(0, detail = names_pretty[[k]])
        if (write_field(k)) done <- done + 1L else break
        incProgress(1 / length(empty))
      }
    })
    showNotification(sprintf("Drafted %d of %d empty parts.", done, length(empty)), type = if (done == length(empty)) "message" else "warning")
  })

  observeEvent(input[[paste0(mod, "_rf_clear")]], {
    for (k in keys) {
      updateTextAreaInput(session, id("_rf_", k), value = "")
    }
  })

  notes
}
