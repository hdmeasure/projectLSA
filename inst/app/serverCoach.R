# serverCoach.R
# The avatar that offers to explain the tab the user is looking at, once an analysis has results.
# The offer is free; the explanation needs an AI model, and without one the user is pointed to
# the AI settings.

server_coach <- function(input, output, session, ai_context, ai_state, project, module_ids) {
  # tell the page whether the open module has results worth explaining
  observe({
    ready <- project() %in% module_ids && nzchar(trimws(ai_context$results_text %||% ""))
    session$sendCustomMessage("pl_coach", list(ready = isTRUE(ready)))
  })

  observeEvent(input$pl_coach_ask, {
    a <- input$pl_coach_ask
    mod <- isolate(project())
    st <- ai_state
    if (!any(vapply(isolate(st$slots), ai_slot_usable, logical(1)))) {
      session$sendCustomMessage("pl_coach_answer", list(state = "nokey"))
      return()
    }
    res_text <- isolate(ai_context$results_text) %||% ""
    tab <- as.character(a$title %||% "this tab")
    lang <- input[[paste0(mod, "_rf_lang")]] %||% "English"
    prompt <- paste0(
      "Module: ", isolate(ai_context$module) %||% mod, "\n",
      "The user has finished the analysis and is now looking at the tab \"", tab, "\".\n",
      "Explain this tab to a researcher who is not a statistician, in ", lang, ": (1) what to look at first, (2) what it means, using the numbers in the results below, ",
      "and (3) what would be a warning sign. Two or three short paragraphs, about 150 words in all. If the results below do not cover this tab, say what the tab shows in general and what to check.\n\n",
      "Analysis results (the only source of numbers you may use):\n", substr(res_text, 1, 12000)
    )
    extra <- paste0("Write plain paragraphs only: no headings, no lists, no markdown, no preamble. Never invent numbers or references.\n", pl_writing_rules)
    r <- tryCatch(ai_ask_state(st, prompt, extra), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    if (!isTRUE(r$ok)) {
      session$sendCustomMessage("pl_coach_answer", list(state = "error", html = htmltools::htmlEscape(r$message %||% "The assistant could not answer.")))
      return()
    }
    html <- paste0("<p>", gsub("\n+", "</p><p>", htmltools::htmlEscape(gsub("\\*\\*|__|`", "", r$text))), "</p>")
    session$sendCustomMessage("pl_coach_answer", list(state = "answer", html = html, via = paste(r$via$provider, r$via$model, sep = " · ")))
  })
}
