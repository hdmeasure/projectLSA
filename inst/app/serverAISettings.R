# serverAISettings.R
# State and actions behind the AI settings drawer. `ai_state` is a reactiveValues
# created in app.R:
#   slots    list of list(id, provider, model, key, base_url, label, active,
#                         cool_until, cool_reason, status)
#   mode     "auto" | "single"
#   primary  id of the model that is tried first (auto) or used alone (single)
#   seq      counter for new ids
#   editing  id of the model card whose editor is open (or NULL)

ai_slot_usable <- function(s) {
  isTRUE(s$active) && (nzchar(s$key %||% "") || identical(s$provider, "custom"))
}

# Applies the cool-down / status changes returned by ai_ask() or the tests
ai_apply_updates <- function(ai_state, upd) {
  if (!length(upd)) return(invisible(NULL))
  sl <- ai_state$slots
  ids <- vapply(sl, function(s) s$id, character(1))
  for (id in names(upd)) {
    i <- match(id, ids)
    if (is.na(i)) next
    sl[[i]]$cool_until <- upd[[id]]$cool_until %||% 0
    sl[[i]]$cool_reason <- upd[[id]]$reason %||% ""
    sl[[i]]$status <- upd[[id]]$status
  }
  ai_state$slots <- sl
  invisible(NULL)
}

# Calls the assistant with the current settings and stores what the call learned
ai_ask_state <- function(ai_state, prompt, extra_system = NULL) {
  r <- ai_ask(isolate(prompt), isolate(ai_state$slots), isolate(ai_state$mode), isolate(ai_state$primary),
              system = ai_system_prompt(extra_system))
  ai_apply_updates(ai_state, r$updates)
  r
}

server_ai_settings <- function(input, output, session, ai_state) {

  mask <- function(key) if (!nzchar(key %||% "")) "no key" else paste0("••••", substr(key, max(1, nchar(key) - 3), nchar(key)))
  now <- function() as.numeric(Sys.time())

  # NA for a missing or empty id, so a malformed click can never break an observer
  find_i <- function(id) {
    if (length(id) != 1L || is.na(id) || !nzchar(id)) return(NA_integer_)
    match(id, vapply(ai_state$slots, function(s) s$id, character(1)))
  }
  update_slot <- function(id, f) {
    i <- find_i(id)
    if (is.na(i)) return(invisible(NULL))
    sl <- ai_state$slots
    sl[[i]] <- f(sl[[i]])
    ai_state$slots <- sl
  }

  # ---- provider choice fills the model list and the hint --------------------
  observeEvent(input$ai_new_provider, {
    p <- ai_providers()[[input$ai_new_provider]]
    if (is.null(p)) return()
    updateSelectizeInput(session, "ai_new_model", choices = p$examples, selected = p$default_model, server = FALSE)
  }, ignoreNULL = TRUE)

  output$ai_provider_hint <- renderUI({
    p <- ai_providers()[[input$ai_new_provider %||% "gemini"]]
    req(p)
    tags$div(
      class = "pl-ai-hint",
      if (isTRUE(p$free)) tags$span(class = "pl-tag-free", "Free tier"),
      if (identical(p$free, FALSE)) tags$span(class = "pl-tag-paid", "Paid"),
      tags$span(p$note),
      if (nzchar(p$link)) tags$a(href = p$link, target = "_blank", rel = "noopener", paste("Get a key at", p$link_name), tags$i(class = "fa-solid fa-arrow-up-right-from-square"))
    )
  })

  # ---- add -----------------------------------------------------------------------
  observeEvent(input$ai_add_slot, {
    prov <- input$ai_new_provider
    key <- trimws(input$ai_new_key %||% "")
    model <- trimws(input$ai_new_model %||% "")
    base <- trimws(input$ai_new_base %||% "")
    if (!nzchar(model)) return(showNotification("Choose or type a model name.", type = "warning"))
    if (identical(prov, "custom")) {
      if (!grepl("^https?://", base)) return(showNotification("Enter the base URL, for example http://localhost:11434/v1", type = "warning"))
    } else if (nchar(key) < 8 || grepl("\\s", key)) {
      return(showNotification("That API key looks too short or contains spaces.", type = "warning"))
    }
    ai_state$seq <- ai_state$seq + 1L
    id <- paste0("m", ai_state$seq)
    lbl <- trimws(input$ai_new_label %||% "")
    if (!nzchar(lbl)) lbl <- paste0(ai_provider_label(prov), " · ", model)
    slot <- list(
      id = id, provider = prov, model = model, key = key, base_url = if (identical(prov, "custom")) sub("/+$", "", base) else "",
      label = lbl, active = TRUE, cool_until = 0, cool_reason = "", status = list(state = "untested")
    )
    ai_state$slots <- c(ai_state$slots, list(slot))
    if (is.null(ai_state$primary) || !(ai_state$primary %in% vapply(ai_state$slots, function(s) s$id, ""))) ai_state$primary <- id
    updateTextInput(session, "ai_new_key", value = "")
    updateTextInput(session, "ai_new_label", value = "")
    showNotification(paste("Added:", lbl), type = "message")
  })

  # ---- load the model list from the provider ----------------------------------------
  observeEvent(input$ai_fetch_models, {
    prov <- input$ai_new_provider
    key <- trimws(input$ai_new_key %||% "")
    if (!identical(prov, "custom") && !nzchar(key)) return(showNotification("Paste the API key first, then load the models.", type = "warning"))
    showNotification("Asking the provider for its models ...", id = "ai_models_note", type = "default", duration = NULL)
    r <- ai_list_models(list(provider = prov, key = key, base_url = trimws(input$ai_new_base %||% ""), model = ""))
    removeNotification("ai_models_note")
    if (!isTRUE(r$ok)) return(showNotification(r$message, type = "error"))
    cur <- input$ai_new_model
    updateSelectizeInput(session, "ai_new_model", choices = r$models, selected = if (!is.null(cur) && cur %in% r$models) cur else r$models[1], server = FALSE)
    showNotification(sprintf("%d models available from this key.", length(r$models)), type = "message")
  })

  # ---- mode, primary and per-model actions ------------------------------------------------
  observeEvent(input$ai_mode, { ai_state$mode <- input$ai_mode }, ignoreInit = TRUE)

  move <- function(id, by) {
    sl <- ai_state$slots
    i <- find_i(id)
    j <- i + by
    if (is.na(i) || j < 1 || j > length(sl)) return()
    sl[c(i, j)] <- sl[c(j, i)]
    ai_state$slots <- sl
  }

  test_slots <- function(slots) {
    res <- ai_run_parallel(slots, "Reply with the single word OK.", system = NULL, max_tokens = 16, timeout = 45)
    upd <- list()
    for (k in seq_along(slots)) {
      r <- res[[k]]
      upd[[slots[[k]]$id]] <- if (isTRUE(r$ok)) {
        list(cool_until = 0, reason = "", status = list(state = "ok", ms = r$ms, at = now()))
      } else {
        list(cool_until = if (isTRUE(r$follow)) now() + (r$cool %||% 0) else 0, reason = r$message,
             status = list(state = "error", cls = r$cls, message = r$message, ms = r$ms, at = now()))
      }
    }
    ai_apply_updates(ai_state, upd)
    vapply(res, function(r) isTRUE(r$ok), logical(1))
  }

  observeEvent(input$ai_slot_action, {
    a <- input$ai_slot_action
    req(a$action)
    id <- a$id
    switch(a$action,
      delete = {
        ai_state$slots <- Filter(function(s) !identical(s$id, id), ai_state$slots)
        if (identical(ai_state$primary, id)) ai_state$primary <- if (length(ai_state$slots)) ai_state$slots[[1]]$id else NULL
      },
      up = move(id, -1L),
      down = move(id, 1L),
      toggle = update_slot(id, function(s) { s$active <- !isTRUE(s$active); s }),
      primary = {
        ai_state$primary <- id
        update_slot(id, function(s) { s$active <- TRUE; s })
        i <- find_i(id)
        if (!is.na(i)) showNotification(paste("Now using:", ai_state$slots[[i]]$label), type = "message", duration = 4)
      },
      edit = { ai_state$editing <- id },
      cancel = { ai_state$editing <- NULL },
      retry = update_slot(id, function(s) { s$cool_until <- 0; s$cool_reason <- ""; s$status <- list(state = "untested"); s }),
      test = {
        i <- find_i(id)
        if (is.na(i)) return()
        update_slot(id, function(s) { s$status <- list(state = "testing"); s })
        ok <- test_slots(list(ai_state$slots[[i]]))
        showNotification(if (ok) "The model answered." else "The model did not answer. See its status in the list.", type = if (ok) "message" else "warning", duration = 5)
      }
    )
  })

  # Save from the card editor: model name, label, base URL, and optionally a new key
  observeEvent(input$ai_slot_edit, {
    e <- input$ai_slot_edit
    i <- find_i(e$id)
    if (is.na(i)) return()
    model <- trimws(e$model %||% "")
    key <- trimws(e$key %||% "")
    if (!nzchar(model)) return(showNotification("The model name cannot be empty.", type = "warning"))
    if (nzchar(key) && (nchar(key) < 8 || grepl("\\s", key))) return(showNotification("That API key looks too short or contains spaces.", type = "warning"))
    base <- trimws(e$base_url %||% "")
    if (identical(ai_state$slots[[i]]$provider, "custom") && !grepl("^https?://", base)) return(showNotification("Enter a valid base URL.", type = "warning"))
    changed_model <- !identical(model, ai_state$slots[[i]]$model)
    update_slot(e$id, function(s) {
      s$model <- model
      s$label <- if (nzchar(trimws(e$label %||% ""))) trimws(e$label) else paste0(ai_provider_label(s$provider), " \u00B7 ", model)
      if (nzchar(key)) s$key <- key
      if (identical(s$provider, "custom")) s$base_url <- sub("/+$", "", base)
      # a different model or key starts from a clean slate
      if (changed_model || nzchar(key)) { s$cool_until <- 0; s$cool_reason <- ""; s$status <- list(state = "untested") }
      s
    })
    ai_state$editing <- NULL
    showNotification(paste0("Saved: ", model), type = "message", duration = 4)
  })

  observeEvent(input$ai_test_all, {
    todo <- Filter(ai_slot_usable, ai_state$slots)
    if (!length(todo)) return(showNotification("Add at least one active model first.", type = "warning"))
    for (s in todo) update_slot(s$id, function(x) { x$status <- list(state = "testing"); x })
    showNotification(sprintf("Testing %d models at the same time ...", length(todo)), id = "ai_test_note", duration = NULL)
    ok <- test_slots(todo)
    removeNotification("ai_test_note")
    showNotification(sprintf("%d of %d models answered.", sum(ok), length(ok)), type = if (all(ok)) "message" else "warning", duration = 7)
  })

  # ---- the list of models ----------------------------------------------------------------------
  output$ai_slots_ui <- renderUI({
    sl <- ai_state$slots
    # keep the cool-down countdown fresh, but never redraw a card while it is being edited
    if (is.null(ai_state$editing) && any(vapply(sl, function(s) (s$cool_until %||% 0) > as.numeric(Sys.time()), logical(1)))) invalidateLater(30000)
    if (!length(sl)) {
      return(tags$div(class = "pl-ai-none", tags$i(class = "fa-regular fa-face-smile"), "No model yet. Add one below; a free-tier provider is a good start."))
    }
    t_now <- now()
    cards <- lapply(seq_along(sl), function(i) {
      s <- sl[[i]]
      st <- s$status %||% list(state = "untested")
      cooling <- (s$cool_until %||% 0) > t_now
      state <- if (!isTRUE(s$active)) "off" else if (cooling) "cooling" else st$state %||% "untested"
      detail <- switch(state,
        off = "Switched off",
        cooling = sprintf("Paused after an error, back in about %d min%s", max(1L, ceiling((s$cool_until - t_now) / 60)), if (nzchar(s$cool_reason %||% "")) paste0(" (", s$cool_reason, ")") else ""),
        ok = sprintf("Working%s", if (!is.null(st$ms) && !is.na(st$ms)) sprintf(" · %d ms", as.integer(st$ms)) else ""),
        error = st$message %||% "Failed",
        testing = "Testing ...",
        "Not tested yet"
      )
      is_primary <- identical(ai_state$primary, s$id)
      act <- function(action, icon_name, title, cls = "") {
        tags$button(type = "button", class = paste("pl-slot-btn", cls), title = title, `aria-label` = title,
                    onclick = sprintf("plAiAct('%s','%s')", s$id, action), tags$i(class = paste0("fa-solid fa-", icon_name)))
      }
      editing <- identical(ai_state$editing, s$id)
      prov <- ai_providers()[[s$provider]]
      editor <- if (editing) {
        dl_id <- paste0("pl-models-", s$id)
        tags$div(
          class = "pl-slot-edit",
          tags$label("Model"),
          tags$input(type = "text", class = "pl-slot-input", `data-f` = "model", value = s$model, list = dl_id, autocomplete = "off",
                     placeholder = "e.g. gemini-3.0-flash", onkeydown = sprintf("if(event.key==='Enter'){plAiSave('%s')}", s$id)),
          tags$datalist(id = dl_id, lapply(unique(c(s$model, prov$examples)), function(x) tags$option(value = x))),
          tags$label("Label"),
          tags$input(type = "text", class = "pl-slot-input", `data-f` = "label", value = s$label, autocomplete = "off"),
          tags$label("New API key (leave empty to keep the current one)"),
          tags$input(type = "password", class = "pl-slot-input", `data-f` = "key", value = "", autocomplete = "new-password", placeholder = mask(s$key)),
          if (identical(s$provider, "custom")) tagList(
            tags$label("Base URL"),
            tags$input(type = "text", class = "pl-slot-input", `data-f` = "base_url", value = s$base_url %||% "", autocomplete = "off")
          ),
          tags$div(
            class = "pl-slot-edit-actions",
            tags$button(type = "button", class = "btn btn-primary btn-xs", onclick = sprintf("plAiSave('%s')", s$id), icon("check"), " Save"),
            tags$button(type = "button", class = "btn btn-default btn-xs", onclick = sprintf("plAiAct('%s','cancel')", s$id), "Cancel")
          )
        )
      }
      tags$div(
        class = paste("pl-slot", if (is_primary) "is-primary", if (editing) "is-editing"), `data-state` = state, `data-id` = s$id,
        tags$div(
          class = "pl-slot-main",
          tags$span(class = "pl-slot-dot"),
          tags$div(
            class = "pl-slot-text",
            tags$b(s$label),
            tags$small(paste0(ai_provider_label(s$provider), " · ", s$model, " · ", mask(s$key))),
            tags$span(class = "pl-slot-status", detail)
          ),
          if (is_primary) tags$span(class = "pl-slot-cur", if (identical(ai_state$mode, "single")) "In use" else "First choice")
        ),
        tags$div(
          class = "pl-slot-actions",
          if (!is_primary) tags$button(type = "button", class = "pl-slot-use", onclick = sprintf("plAiAct('%s','primary')", s$id),
                                       tags$i(class = "fa-solid fa-check"), " Use"),
          act("edit", "pen", "Edit the model name, label or key"),
          act("test", "bolt", "Test this model"),
          if (cooling) act("retry", "rotate-right", "Try again now"),
          act("up", "chevron-up", "Move up (tried earlier)"),
          act("down", "chevron-down", "Move down (tried later)"),
          act("toggle", if (isTRUE(s$active)) "toggle-on" else "toggle-off", if (isTRUE(s$active)) "Switch off" else "Switch on",
              if (isTRUE(s$active)) "on" else ""),
          act("delete", "trash-can", "Remove", "danger")
        ),
        editor
      )
    })
    tags$div(class = "pl-slots", cards)
  })

  # ---- readiness, switcher and the status dot ---------------------------------------------------------
  # "1" when at least one model can be used, "" otherwise: a falsy string that
  # conditionalPanel(condition = "output.ai_ready") can test
  output$ai_ready <- renderText({ if (any(vapply(ai_state$slots, ai_slot_usable, logical(1)))) "1" else "" })
  outputOptions(output, "ai_ready", suspendWhenHidden = FALSE)

  output$ai_switcher_ui <- renderUI({
    sl <- Filter(ai_slot_usable, ai_state$slots)
    if (length(sl) < 2) return(NULL)          # nothing to switch between
    choices <- stats::setNames(vapply(sl, function(s) s$id, ""), vapply(sl, function(s) s$label, ""))
    sel <- if (!is.null(ai_state$primary) && ai_state$primary %in% choices) ai_state$primary else choices[[1]]
    tags$div(
      class = "pl-ai-switch",
      selectInput("ai_switch", NULL, choices = choices, selected = sel, width = "100%")
    )
  })
  observeEvent(input$ai_switch, {
    if (!is.null(input$ai_switch) && !identical(input$ai_switch, ai_state$primary)) ai_state$primary <- input$ai_switch
  }, ignoreInit = TRUE)

  observe({
    sl <- ai_state$slots
    usable <- Filter(ai_slot_usable, sl)
    t_now <- now()
    state <- if (!length(usable)) "none" else if (all(vapply(usable, function(s) (s$cool_until %||% 0) > t_now, logical(1)))) "cooling" else "ready"
    cur <- Filter(function(s) identical(s$id, ai_state$primary), sl)
    text <- switch(state,
      none = "AI settings: no model yet",
      cooling = "AI settings: every model is paused after an error",
      sprintf("AI settings: %s%s", if (length(cur)) cur[[1]]$label else usable[[1]]$label, if (length(usable) > 1) sprintf(" (+%d more)", length(usable) - 1L) else "")
    )
    session$sendCustomMessage("pl_ai_status", list(state = state, text = text))
  })
}
