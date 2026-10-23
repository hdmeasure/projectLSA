# ai_helper.R
# Engine of the AI assistant: several models can be configured at once, tried in
# priority order (automatic mode) or used one at a time (single mode), tested in
# parallel, and switched at any moment. Keys live only in the running session.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a[1]))) b else a

# ---- providers ------------------------------------------------------------------
# Free-tier providers come first. "Free tier" means the provider offers a no-cost
# allowance; its limits and availability are set by the provider, not by this app.
ai_providers <- function() {
  list(
    gemini = list(
      label = "Google Gemini", kind = "gemini", free = TRUE,
      base_url = "https://generativelanguage.googleapis.com/v1beta", default_model = "gemini-2.5-flash",
      examples = c("gemini-2.5-flash", "gemini-2.5-flash-lite", "gemini-2.5-pro", "gemini-2.0-flash"),
      link = "https://aistudio.google.com/apikey", link_name = "Google AI Studio",
      note = "Free tier available. A good place to start."
    ),
    groq = list(
      label = "Groq", kind = "openai", free = TRUE,
      base_url = "https://api.groq.com/openai/v1", default_model = "llama-3.3-70b-versatile",
      examples = c("llama-3.3-70b-versatile", "llama-3.1-8b-instant", "openai/gpt-oss-120b"),
      link = "https://console.groq.com/keys", link_name = "Groq Console",
      note = "Free tier available. Very fast."
    ),
    openrouter = list(
      label = "OpenRouter", kind = "openai", free = TRUE,
      base_url = "https://openrouter.ai/api/v1", default_model = "meta-llama/llama-3.3-70b-instruct:free",
      examples = c("meta-llama/llama-3.3-70b-instruct:free", "deepseek/deepseek-chat-v3-0324:free", "google/gemini-2.0-flash-exp:free"),
      link = "https://openrouter.ai/keys", link_name = "OpenRouter",
      note = "Many models; those ending in :free cost nothing."
    ),
    cerebras = list(
      label = "Cerebras", kind = "openai", free = TRUE,
      base_url = "https://api.cerebras.ai/v1", default_model = "llama-3.3-70b",
      examples = c("llama-3.3-70b", "llama3.1-8b", "qwen-3-32b"),
      link = "https://cloud.cerebras.ai", link_name = "Cerebras Cloud",
      note = "Free tier available."
    ),
    mistral = list(
      label = "Mistral", kind = "openai", free = TRUE,
      base_url = "https://api.mistral.ai/v1", default_model = "mistral-small-latest",
      examples = c("mistral-small-latest", "open-mistral-nemo", "mistral-large-latest"),
      link = "https://console.mistral.ai/api-keys", link_name = "Mistral Console",
      note = "Free tier available."
    ),
    openai = list(
      label = "OpenAI", kind = "openai", free = FALSE,
      base_url = "https://api.openai.com/v1", default_model = "gpt-4o-mini",
      examples = c("gpt-4o-mini", "gpt-4o", "gpt-4.1-mini"),
      link = "https://platform.openai.com/api-keys", link_name = "OpenAI Platform",
      note = "Paid. Needs credit on your OpenAI account."
    ),
    grok = list(
      label = "xAI Grok", kind = "openai", free = FALSE,
      base_url = "https://api.x.ai/v1", default_model = "grok-3-mini",
      examples = c("grok-3-mini", "grok-3", "grok-4"),
      link = "https://console.x.ai", link_name = "xAI Console",
      note = "Paid. Needs credit on your xAI account."
    ),
    custom = list(
      label = "Custom (OpenAI-compatible)", kind = "openai", free = NA,
      base_url = "", default_model = "",
      examples = character(0),
      link = "", link_name = "",
      note = "Any OpenAI-compatible endpoint, for example a local Ollama or LM Studio server. Enter its base URL."
    )
  )
}

ai_provider_label <- function(id) { p <- ai_providers()[[id]]; if (is.null(p)) id else p$label }

# ---- system prompt ----------------------------------------------------------------
ai_base_system_prompt <- function() {
  paste0(
    "You are an assistant for projectLSA, an R package for latent structure analysis (Latent Profile Analysis, Latent Class Analysis, Latent Trait Analysis/IRT, Exploratory Factor Analysis, and Confirmatory Factor Analysis/Structural Equation Modeling). You MUST strictly limit your responses to the context of data analysis, statistics, psychometrics, latent variable modelling, and educational measurement. If the user asks about ANY other topic, you MUST politely refuse to answer and state that you are an AI Assistant exclusively for projectLSA. IMPORTANT: NEVER use LaTeX math symbols (like $\\chi^2$, $p < .001$, etc). Write all numbers, formulas, and statistical symbols in plain text only (e.g. 'Chi-square(300) = 18146.07, p < .001'). Do not use dollar signs for math. Do NOT use markdown formatting such as asterisks (** or *) for bolding or italics in your response.\n\n",
    "CRITICAL GUIDELINES FOR EVALUATING MIXTURE MODELS (LPA/LCA/LTA):\n",
    "1. Class/Profile Enumeration (Nylund et al., 2007; Masyn, 2013):\n",
    "   - Compare solutions using BIC, aBIC (sample-size adjusted BIC), and AIC; lower values indicate better relative fit.\n",
    "   - The Bootstrap Likelihood Ratio Test (BLRT) and the Vuong-Lo-Mendell-Rubin (VLMR/LMR) test compare a k-class model against a (k-1)-class model; a significant p-value favours the k-class solution.\n",
    "   - Entropy summarises classification precision: values >= .80 indicate clear class separation, but entropy alone must NOT be used to select the number of classes.\n",
    "   - Always weigh statistical criteria against theoretical interpretability, class size (avoid very small classes, e.g. < 5 percent of the sample), and parsimony.\n",
    "2. Latent Profile Analysis (Rosenberg et al., 2018; Spurk et al., 2020):\n",
    "   - Report the model variance/covariance specification (e.g. equal variances and zero covariances vs. free variances) alongside fit statistics.\n",
    "   - Interpret profiles from the pattern of indicator means (preferably standardised), not from raw means alone.\n",
    "3. Latent Class Analysis (Linzer & Lewis, 2011; Collins & Lanza, 2010):\n",
    "   - Interpret item-response probabilities (conditional probabilities) and class prevalence, and label classes only when the response pattern is theoretically coherent.\n",
    "4. Latent Trait Analysis / IRT (Chalmers, 2012; de Ayala, 2009):\n",
    "   - Check unidimensionality and local independence before interpreting item parameters.\n",
    "   - Discuss item discrimination (a), difficulty (b), and where relevant guessing (c) parameters, plus item/test information and standard error of measurement across the theta continuum.\n",
    "   - Report item fit statistics (e.g. S-X2, infit/outfit) and flag misfitting items.\n\n",
    "CRITICAL GUIDELINES FOR EVALUATING CFA AND SEM MODELS:\n",
    "When interpreting Confirmatory Factor Analysis (CFA) or Structural Equation Modeling (SEM) results, you MUST use the following established literature rules of thumb:\n",
    "1. Fit Indices (Hu & Bentler, 1999; Schreiber et al., 2006; Thakkar, 2020):\n",
    "   - RMSEA: <= .06 indicates close fit, .06 to .08 indicates fair/mediocre fit.\n",
    "   - SRMR: <= .08 indicates good fit.\n",
    "   - CFI and TLI: >= .95 indicates good fit, >= .90 indicates acceptable fit.\n",
    "   - CMIN/DF (Chi-square/df): < 3 indicates good fit.\n",
    "   - Note: Be mindful that exact cutoffs can be dynamic depending on model complexity and sample size.\n",
    "2. Estimation for Ordinal Data (DiStefano & Morgan, 2014):\n",
    "   - Diagonal Weighted Least Squares (DWLS) is highly recommended for ordinal data (e.g., Likert scales), especially when data is non-normal or has few categories.\n",
    "3. Construct Validity and Reliability (Hair et al., 2017):\n",
    "   - Outer factor loadings should ideally be > 0.708.\n",
    "   - Average Variance Extracted (AVE) should be > 0.50 for convergent validity.\n",
    "   - Composite Reliability (CR) should be > 0.70.\n",
    "4. Local Fit (Raykov et al., 2013):\n",
    "   - Remind the user to consider individual case residuals for assessing local fit rather than relying solely on overall global fit statistics.\n",
    "5. Model Modification and Comparison:\n",
    "   - If the results include model modifications or multiple models (e.g., Original vs Modified), you MUST compare their fit indices comprehensively.\n",
    "   - Explain the improvements in fit and interpret the modification indices logically based on theoretical justification, adhering to CFA/SEM reporting guidelines."
  )
}

ai_system_prompt <- function(extra = NULL) {
  base <- ai_base_system_prompt()
  if (is.null(extra) || !nzchar(extra)) base else paste(base, extra, sep = "\n")
}

# ---- request building ---------------------------------------------------------------
ai_base_url <- function(slot) {
  p <- ai_providers()[[slot$provider]]
  sub("/+$", "", slot$base_url %||% p$base_url %||% "")
}

# One HTTP request for one slot. Gemini carries its key in a header, so the key
# never appears in a URL (and therefore never in a log or an error message).
ai_request <- function(slot, prompt, system = NULL, max_tokens = 4096, temperature = 0.4) {
  p <- ai_providers()[[slot$provider]]
  base <- ai_base_url(slot)
  if (identical(p$kind, "gemini")) {
    body <- list(
      contents = list(list(role = "user", parts = list(list(text = prompt)))),
      generationConfig = list(temperature = temperature, maxOutputTokens = max_tokens)
    )
    if (!is.null(system) && nzchar(system)) body$systemInstruction <- list(parts = list(list(text = system)))
    list(
      kind = "gemini",
      url = sprintf("%s/models/%s:generateContent", base, utils::URLencode(slot$model, reserved = TRUE)),
      headers = c(`Content-Type` = "application/json", `x-goog-api-key` = slot$key),
      body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
    )
  } else {
    msgs <- list(list(role = "user", content = prompt))
    if (!is.null(system) && nzchar(system)) msgs <- c(list(list(role = "system", content = system)), msgs)
    body <- list(model = slot$model, messages = msgs, temperature = temperature, max_tokens = max_tokens)
    hdr <- c(`Content-Type` = "application/json")
    if (nzchar(slot$key %||% "")) hdr[["Authorization"]] <- paste("Bearer", slot$key)
    list(
      kind = "openai",
      url = paste0(base, "/chat/completions"),
      headers = hdr,
      body = as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
    )
  }
}

ai_handle <- function(req, timeout = 60) {
  h <- curl::new_handle()
  curl::handle_setopt(h, url = req$url, post = TRUE, postfields = req$body, timeout = timeout, connecttimeout = 15, followlocation = TRUE)
  curl::handle_setheaders(h, .list = as.list(req$headers))
  h
}

# ---- reading a response ----------------------------------------------------------------
ai_header <- function(raw, name) {
  if (is.null(raw) || !length(raw)) return(NA_character_)
  txt <- tryCatch(rawToChar(raw), error = function(e) "")
  m <- regmatches(txt, regexpr(paste0("(?i)\\r?\\n", name, ":[^\\r\\n]*"), txt, perl = TRUE))
  if (!length(m)) NA_character_ else trimws(sub("^[^:]*:", "", m[1]))
}

# Maps an HTTP outcome to an error class, the wait before this slot is tried again,
# and whether another slot is worth trying (follow = TRUE) or the request itself is
# at fault (follow = FALSE). The classes follow the SIMETRIK assistant.
ai_classify <- function(status, text, retry_after = NA, model = "") {
  msg <- tryCatch({
    j <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    e <- j$error %||% j$message %||% ""
    if (is.list(e)) e <- e$message %||% jsonlite::toJSON(e, auto_unbox = TRUE)
    as.character(e)
  }, error = function(e) substr(text %||% "", 1, 300))
  msg <- substr(gsub("\\s+", " ", msg %||% ""), 1, 280)
  low <- tolower(msg)
  wait <- suppressWarnings(as.numeric(retry_after))
  if (is.na(wait)) {
    m <- regmatches(msg, regexpr("(?i)retry in ([0-9.]+)\\s*s", msg, perl = TRUE))
    if (length(m)) wait <- ceiling(as.numeric(sub("(?i).*retry in ([0-9.]+)\\s*s.*", "\\1", m, perl = TRUE)))
  }
  if (status %in% c(401L, 403L) || grepl("api key|permission|unauthori", low))
    return(list(cls = "key", message = "The provider rejected the API key. Check it in the AI settings.", follow = TRUE, cool = 6 * 3600))
  if (status %in% c(400L, 404L) && grepl("model", low) && grepl("not found|not exist|not supported|does not|decommission|invalid|unknown|deprecated", low))
    return(list(cls = "model", message = sprintf("Model \"%s\" is not available for this key.", model), follow = TRUE, cool = 6 * 3600))
  if (status == 402L || grepl("insufficient|billing|balance|credit", low))
    return(list(cls = "billing", message = "The provider account has no credit or billing problem.", follow = TRUE, cool = 3600))
  if (status == 429L || grepl("quota|rate.?limit|resource.?exhausted|too many", low)) {
    daily <- grepl("per ?day|daily|perday", low)
    cool <- max(30, min(if (is.na(wait)) (if (daily) 1800 else 90) else wait, 3 * 3600))
    return(list(cls = "quota", message = if (daily) "Daily quota used up." else "Rate limit reached.", follow = TRUE, cool = cool))
  }
  if (status >= 500L) return(list(cls = "server", message = sprintf("The provider service is having trouble (%d).", status), follow = TRUE, cool = 120))
  if (status >= 400L) return(list(cls = "request", message = sprintf("The provider returned an error (%d). %s", status, msg), follow = FALSE, cool = 0))
  list(cls = "other", message = "Unexpected response.", follow = TRUE, cool = 120)
}

ai_extract_text <- function(kind, text) {
  j <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j)) return(NULL)
  out <- if (identical(kind, "gemini")) {
    parts <- tryCatch(j$candidates[[1]]$content$parts, error = function(e) NULL)
    # thinking models send their reasoning as parts flagged thought = TRUE; only the answer is wanted
    if (is.null(parts)) NULL else paste(vapply(parts, function(x) if (isTRUE(x$thought)) "" else as.character(x$text %||% ""), character(1)), collapse = "")
  } else {
    ct <- tryCatch(j$choices[[1]]$message$content, error = function(e) NULL)
    if (is.list(ct)) ct <- paste(vapply(ct, function(x) if (is.list(x)) as.character(x$text %||% "") else as.character(x), character(1)), collapse = "")
    ct
  }
  if (is.null(out) || !nzchar(trimws(out))) NULL else trimws(out)
}

# Why an answer is empty: "length" (the token limit was used up, often by a thinking model),
# "blocked" (safety filter) or "" (unknown); `detail` is the provider's own reason
ai_empty_reason <- function(kind, text) {
  j <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j)) return(list(why = "", detail = ""))
  if (identical(kind, "gemini")) {
    fr <- tryCatch(as.character(j$candidates[[1]]$finishReason %||% ""), error = function(e) "")
    br <- tryCatch(as.character(j$promptFeedback$blockReason %||% ""), error = function(e) "")
    if (nzchar(br) || fr %in% c("SAFETY", "PROHIBITED_CONTENT", "BLOCKLIST", "SPII", "RECITATION")) return(list(why = "blocked", detail = if (nzchar(br)) br else fr))
    if (identical(fr, "MAX_TOKENS")) return(list(why = "length", detail = fr))
    list(why = "", detail = fr)
  } else {
    fr <- tryCatch(as.character(j$choices[[1]]$finish_reason %||% ""), error = function(e) "")
    if (identical(fr, "length")) return(list(why = "length", detail = fr))
    if (fr %in% c("content_filter")) return(list(why = "blocked", detail = fr))
    list(why = "", detail = fr)
  }
}

# Turns the raw result of one request into list(ok, text, ms, cls, message, follow, cool)
ai_interpret <- function(req, status, text, headers, ms, model, net_error = NULL) {
  if (!is.null(net_error))
    return(list(ok = FALSE, ms = ms, cls = "network", message = "Could not reach the provider.", follow = TRUE, cool = 120))
  if (status >= 200L && status < 300L) {
    out <- ai_extract_text(req$kind, text)
    if (is.null(out)) {
      er <- ai_empty_reason(req$kind, text)
      if (identical(er$why, "length"))
        return(list(ok = FALSE, ms = ms, cls = "truncated", message = "The model used its whole token limit before it wrote an answer (thinking models do this).", follow = FALSE, cool = 0))
      if (identical(er$why, "blocked"))
        return(list(ok = FALSE, ms = ms, cls = "blocked", message = sprintf("The provider blocked this answer (%s). Try another model or rephrase.", er$detail), follow = TRUE, cool = 0))
      return(list(ok = FALSE, ms = ms, cls = "empty", message = sprintf("The model returned no text%s.", if (nzchar(er$detail)) paste0(" (", er$detail, ")") else ""), follow = TRUE, cool = 0))
    }
    return(list(ok = TRUE, text = out, ms = ms))
  }
  c(list(ok = FALSE, ms = ms), ai_classify(status, text, ai_header(headers, "retry-after"), model))
}

# One slot, one request, blocking
ai_run_one <- function(slot, prompt, system = NULL, max_tokens = 4096, timeout = 120) {
  req <- ai_request(slot, prompt, system, max_tokens)
  t0 <- Sys.time()
  res <- tryCatch(curl::curl_fetch_memory(req$url, handle = ai_handle(req, timeout)), error = function(e) e)
  ms <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000)
  if (inherits(res, "error")) return(ai_interpret(req, 0L, "", NULL, ms, slot$model, net_error = conditionMessage(res)))
  r <- ai_interpret(req, res$status_code, rawToChar(res$content), res$headers, ms, slot$model)
  # a thinking model can spend the whole limit on reasoning: ask once more with a larger one
  if (identical(r$cls, "truncated") && max_tokens < 16000) return(ai_run_one(slot, prompt, system, min(16384, max_tokens * 4), timeout))
  if (identical(r$cls, "truncated")) r$message <- "The model used its whole token limit before it wrote an answer. Pick a model without long reasoning, or a shorter question."
  r
}

# Several slots at the same time (one connection each, run together)
ai_run_parallel <- function(slots, prompt, system = NULL, max_tokens = 256, timeout = 45) {
  n <- length(slots)
  out <- vector("list", n)
  if (n == 0) return(out)
  pool <- curl::new_pool(total_con = max(1L, n), host_con = max(1L, n))
  start <- rep(Sys.time(), n)
  for (i in seq_len(n)) local({
    k <- i
    req <- ai_request(slots[[k]], prompt, system, max_tokens)
    start[k] <<- Sys.time()
    ms_now <- function() round(as.numeric(difftime(Sys.time(), start[k], units = "secs")) * 1000)
    curl::multi_add(
      ai_handle(req, timeout), pool = pool,
      done = function(res) out[[k]] <<- ai_interpret(req, res$status_code, rawToChar(res$content), res$headers, ms_now(), slots[[k]]$model),
      fail = function(msg) out[[k]] <<- ai_interpret(req, 0L, "", NULL, ms_now(), slots[[k]]$model, net_error = msg)
    )
  })
  curl::multi_run(timeout = timeout + 15, pool = pool)
  for (i in seq_len(n)) if (is.null(out[[i]])) out[[i]] <- list(ok = FALSE, ms = NA, cls = "network", message = "No answer in time.", follow = TRUE, cool = 120)
  # in a test the short limit can be eaten by a thinking model: the model answered, so it works
  for (i in seq_len(n)) if (identical(out[[i]]$cls, "truncated")) out[[i]] <- list(ok = TRUE, text = "(reachable; the test reply was cut at its short limit)", ms = out[[i]]$ms)
  out
}

# ---- list the models a key can use ------------------------------------------------------
ai_list_models <- function(slot) {
  p <- ai_providers()[[slot$provider]]
  base <- ai_base_url(slot)
  if (!nzchar(base)) return(list(ok = FALSE, message = "Enter the base URL first."))
  if (identical(p$kind, "gemini")) {
    url <- paste0(base, "/models?pageSize=200"); hdr <- c(`x-goog-api-key` = slot$key)
  } else {
    url <- paste0(base, "/models"); hdr <- if (nzchar(slot$key %||% "")) c(Authorization = paste("Bearer", slot$key)) else character(0)
  }
  h <- curl::new_handle(); curl::handle_setopt(h, timeout = 30, connecttimeout = 15, followlocation = TRUE)
  if (length(hdr)) curl::handle_setheaders(h, .list = as.list(hdr))
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(res)) return(list(ok = FALSE, message = "Could not reach the provider."))
  txt <- rawToChar(res$content)
  if (res$status_code >= 300L) return(list(ok = FALSE, message = ai_classify(res$status_code, txt, NA, slot$model)$message))
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  ids <- if (identical(p$kind, "gemini")) {
    m <- j$models
    unlist(lapply(m, function(x) if ("generateContent" %in% unlist(x$supportedGenerationMethods)) sub("^models/", "", x$name) else NULL))
  } else {
    unlist(lapply(j$data %||% j$models, function(x) x$id %||% x$name))
  }
  ids <- sort(unique(as.character(ids)))
  if (!length(ids)) list(ok = FALSE, message = "The provider returned no models.") else list(ok = TRUE, models = ids)
}

# ---- run a request according to the mode -----------------------------------------------
# slots   list of list(id, provider, model, key, base_url, label, active, cool_until, ...)
# mode    "auto": active slots in order, skipping those cooling down, falling through on
#                 quota / rejected key / missing model / outage and coming back to the top
#                 once a slot has recovered
#         "single": only the primary slot
# Returns list(ok, text, via, fallback, skipped, updates). `updates` maps slot id to
# list(cool_until, reason, status) for the caller to store.
ai_ask <- function(prompt, slots, mode = "auto", primary = NULL, system = NULL, now = as.numeric(Sys.time()), runner = ai_run_one) {
  ready <- Filter(function(s) isTRUE(s$active) && (nzchar(s$key %||% "") || identical(s$provider, "custom")), slots)
  if (!length(ready)) {
    return(list(ok = FALSE, message = "No AI model is set up yet. Open the AI settings (top right) and add one."))
  }
  if (identical(mode, "single")) {
    first <- Filter(function(s) identical(s$id, primary), ready)
    ready <- if (length(first)) first[1] else ready[1]
  } else if (!is.null(primary)) {
    # the primary slot is tried first, the others keep their order
    ids <- vapply(ready, function(s) s$id, character(1))
    if (primary %in% ids) ready <- c(ready[ids == primary], ready[ids != primary])
  }
  usable <- Filter(function(s) (s$cool_until %||% 0) <= now, ready)
  if (!length(usable)) {
    soon <- min(vapply(ready, function(s) (s$cool_until %||% 0) - now, numeric(1)))
    return(list(ok = FALSE, message = sprintf("Every configured model is cooling down after an error; the first one is back in about %d min. Add another model or try again later.", max(1L, ceiling(soon / 60)))))
  }
  updates <- list(); skipped <- character(0)
  last <- NULL
  for (s in usable) {
    r <- runner(s, prompt, system)
    if (isTRUE(r$ok)) {
      updates[[s$id]] <- list(cool_until = 0, reason = "", status = list(state = "ok", ms = r$ms, at = now))
      return(list(
        ok = TRUE, text = r$text,
        via = list(id = s$id, label = s$label %||% ai_provider_label(s$provider), provider = ai_provider_label(s$provider), model = s$model, ms = r$ms),
        fallback = !identical(s$id, ready[[1]]$id), skipped = skipped, updates = updates
      ))
    }
    last <- r
    if (!isTRUE(r$follow)) return(list(ok = FALSE, message = r$message, skipped = skipped, updates = updates))
    if (identical(mode, "auto") && length(usable) > 1)
      updates[[s$id]] <- list(cool_until = now + r$cool, reason = r$message, status = list(state = "error", cls = r$cls, message = r$message, at = now))
    skipped <- c(skipped, paste0(s$label %||% ai_provider_label(s$provider), ": ", r$message))
  }
  list(ok = FALSE, message = last$message %||% "The AI service could not be used right now.", skipped = skipped, updates = updates)
}

# Compatibility wrapper: the answer text, or an "Error: ..." string
ask_ai <- function(prompt, slots, mode = "auto", primary = NULL, system_prompt = NULL) {
  r <- ai_ask(prompt, slots, mode, primary, ai_system_prompt(system_prompt))
  if (isTRUE(r$ok)) r$text else paste("Error:", r$message)
}

# Writing rules for any text the assistant produces for the author (from the author's style standard)
pl_writing_rules <- paste(

    "Write like a published methods article, not like an AI assistant.",
    "Open each paragraph with its claim or finding, never with a scene-setting phrase such as 'in recent years' or 'dewasa ini'.",
    "Do not narrate the writing or defend the findings: no 'it is important to note', 'it must be emphasised', 'perlu ditegaskan', 'penting untuk dicatat', 'honestly'.",
    "Avoid hype and stock vocabulary: crucial, pivotal, essential, comprehensive, holistic, underscore, leverage, delve, navigate, intricate, tapestry, landscape, showcase, meticulous, elevate, foster, realm, nuanced, valuable insights, 'plays a key role'. Prefer plain verbs: use, show, indicate, suggest, estimate.",
    "Report numbers without hedging; hedge only interpretations (may, suggest, appear) and never stack hedges. Never write 'prove'.",
    "Write statistics in APA 7 form: p < .001, no leading zero for values bounded by 1, e.g. CFI = .96.",
    "No bullet lists, no headings, no bold or other markdown. No decorative triplets, no 'not only ... but also', no closing sentence that restates the paragraph, no two consecutive sentences opening with the same connector. Split sentences longer than about 40 words.",
    "Claims need numbers or a citation. Cite only works from the allowed list and never invent a reference."
  )
