# ui_helpers.R - small building blocks shared by the HTML-shell modules

# A numbered card for the control panel
pl_card <- function(step, title, ..., hint = NULL) {
  tags$section(
    class = "pl-card",
    tags$div(
      class = "pl-card-head",
      tags$span(class = "pl-step", HTML(step)),
      tags$h3(title),
      if (!is.null(hint)) tags$small(hint)
    ),
    ...
  )
}

# Fill an HTML shell from templates/<file>. The template is plain HTML; named
# arguments replace the {{ slot }} placeholders.
pl_shell <- function(file, ...) {
  htmltools::htmlTemplate(file.path("templates", file), ..., document_ = FALSE)
}

# ---- layout building blocks for module screens ------------------------------

# "options | results" screen. Replaces shiny::sidebarLayout() in module UIs;
# the width arguments of sidebarPanel()/mainPanel() are accepted and ignored.
pl_split <- function(side, main) {
  tags$div(
    class = "pl-split",
    # edge handle: folds the options away so the results get the full width
    tags$button(type = "button", class = "pl-side-toggle", title = "Hide or show the options",
                `aria-label` = "Hide or show the options", tags$i(class = "fa-solid fa-angles-left")),
    side, main
  )
}
pl_side  <- function(..., width = NULL) tags$div(class = "pl-side", ...)
pl_main  <- function(..., width = NULL) tags$div(class = "pl-main", ...)

# Collapsible group of related settings (native <details>; its inputs stay bound
# to Shiny whether the group is open or closed)
pl_fold <- function(title, ..., icon_name = NULL, open = TRUE, hint = NULL) {
  tags$details(
    class = "pl-fold", open = if (open) NA else NULL,
    tags$summary(
      if (!is.null(icon_name)) tags$i(class = paste0("fa-solid fa-", icon_name)),
      tags$span(title),
      if (!is.null(hint)) tags$small(hint)
    ),
    tags$div(class = "pl-fold-body", ...)
  )
}

# n equal columns of controls
pl_grid <- function(..., cols = 2) {
  tags$div(class = "pl-grid", style = paste0("--cols:", cols), ...)
}

# Row above a result: title on the left, actions on the right
pl_toolbar <- function(title = NULL, sub = NULL, ...) {
  tags$div(
    class = "pl-toolbar",
    tags$div(class = "pl-toolbar-text", if (!is.null(title)) tags$b(title), if (!is.null(sub)) tags$span(sub)),
    tags$div(class = "pl-toolbar-actions", ...)
  )
}

# ---- module screens: hidden tabset + sidebar navigation -----------------------

# Builds the stage-level tabset of a module (its navigation tabs are hidden) and
# the matching sidebar buttons. It reads each tabPanel's icon, title and value,
# so the older module files only need to call it instead of tabsetPanel().
#   id    id of the tabset (the server uses it with updateTabsetPanel())
#   ...   tabPanel() objects
#   subs  optional named list: tab value -> one-line description for the heading
#   noctl tab values that do not use the module's control panel; it hides itself there
# Returns list(nav = <buttons>, stage = <tabsetPanel>).
pl_tabs <- function(id, ..., subs = list(), noctl = character(0), groups = list(), group_icons = character(0), needsmodel = character(0)) {
  tabs <- list(...)
  # a plain list of tabPanel()s may be passed as one argument (for example followup_tabs())
  tabs <- unlist(lapply(tabs, function(x) if (inherits(x, "shiny.tag")) list(x) else x), recursive = FALSE)
  buttons <- list()
  for (i in seq_along(tabs)) {
    x <- tabs[[i]]
    val <- x$attribs[["data-value"]]
    if (!is.character(val) || length(val) != 1L || !nzchar(val)) val <- paste0(id, "_tab", i)
    x$attribs[["data-value"]] <- val
    tabs[[i]] <- x

    title <- x$attribs$title
    parts <- if (inherits(title, "shiny.tag")) list(title) else as.list(title)
    ic <- NULL
    label <- ""
    for (e in parts) {
      if (inherits(e, "shiny.tag")) {
        if (is.null(ic) && grepl("fa", paste(e$attribs$class, collapse = " "))) ic <- e
      } else if (is.character(e)) {
        label <- paste0(label, e)
      }
    }
    label <- trimws(label)
    if (!nzchar(label)) label <- paste("View", i)
    sub <- if (!is.null(subs[[val]])) subs[[val]] else ""
    buttons[[i]] <- tags$button(
      type = "button", `data-tab` = val, `data-title` = label, `data-sub` = sub,
      `data-noctl` = if (val %in% noctl) "1" else NULL,
      `data-needsmodel` = if (val %in% needsmodel) "1" else NULL,
      if (!is.null(ic)) ic else tags$i(class = "fa-solid fa-circle"),
      tags$span(label)
    )
  }
  # tabs listed in `groups` share one dropdown button in the top row, so a method with many views
  # does not need a very long row: groups = list(Items = c("tab_a", "tab_b"), ...)
  vals <- vapply(tabs, function(x) x$attribs[["data-value"]], character(1))
  group_of <- function(v) { for (g in names(groups)) if (v %in% groups[[g]]) return(g); NA_character_ }
  nav <- list(); done <- character(0)
  for (i in seq_along(tabs)) {
    g <- group_of(vals[i])
    if (is.na(g)) { nav[[length(nav) + 1]] <- buttons[[i]]; next }
    if (g %in% done) next
    done <- c(done, g)
    members <- buttons[match(intersect(groups[[g]], vals), vals)]
    ic <- if (g %in% names(group_icons)) group_icons[[g]] else "layer-group"
    nav[[length(nav) + 1]] <- tags$div(
      class = "pl-tabgroup", `data-group` = g,
      tags$button(type = "button", class = "pl-tabgroup-btn", `aria-haspopup` = "true", `aria-expanded` = "false",
                  tags$i(class = paste0("fa-solid fa-", ic)), tags$span(class = "pl-tg-label", g), tags$span(class = "pl-tg-cur"),
                  tags$i(class = "fa-solid fa-chevron-down pl-tg-caret")),
      tags$div(class = "pl-tabmenu", role = "menu", members)
    )
  }
  list(
    nav = tagList(nav),
    stage = do.call(tabsetPanel, c(list(id = id, type = "hidden"), tabs))
  )
}


# ---- version badge -----------------------------------------------------------------------------
# "Version 0.2.0 - 23 October 2026", read from the DESCRIPTION of the installed package or, while
# developing from the app folder, from the DESCRIPTION two levels up. The date is the Date field.
pl_version_info <- function(pkg) {
  d <- NA
  f <- file.path("..", "..", "DESCRIPTION")                      # running from the app folder of the source tree
  if (file.exists(f)) { x <- tryCatch(as.list(read.dcf(f)[1, ]), error = function(e) NA); if (is.list(x) && identical(x$Package, pkg)) d <- x }
  if (!is.list(d)) d <- tryCatch(as.list(utils::packageDescription(pkg)), error = function(e) NA, warning = function(w) NA)
  if (!is.list(d) || is.null(d$Version)) return(NULL)
  dt <- suppressWarnings(as.Date(d$Date %||% NA))
  list(version = d$Version, when = if (!is.na(dt)) sub("^0", "", format(dt, "%d %B %Y")) else NULL)
}
pl_version_badge <- function(pkg) {
  v <- pl_version_info(pkg)
  if (is.null(v)) return(NULL)
  tags$span(class = "pl-ver", tags$i(class = "fa-solid fa-tag"), paste("Version", v$version), if (!is.null(v$when)) tagList(HTML("&middot;"), v$when))
}

# ---- home screen ----------------------------------------------------------------

# One quadrant: icon, short name, full name. `go` makes the whole card open the method;
# the factor-analysis card has two buttons instead (EFA and CFA).
pl_quad_cell <- function(tone, icon_name, acro, full, title, say, go = NULL, ...) {
  tags$div(
    class = "pl-quad-cell", `data-tone` = tone, `data-title` = title, `data-say` = say,
    `data-go` = go, tabindex = "0", role = if (!is.null(go)) "button",
    tags$span(class = "pl-ico", tags$i(class = paste0("fa-solid fa-", icon_name))),
    tags$div(class = "pl-quad-name", tags$b(HTML(acro)), tags$span(full)),
    tags$p(class = "pl-quad-say", say),
    tags$div(class = "pl-quad-actions",
             if (!is.null(go)) tags$span(class = "pl-open", "Open", tags$i(class = "fa-solid fa-arrow-right")), ...)
  )
}

pl_open_btn <- function(module, label) {
  tags$button(type = "button", class = "pl-open", `data-go` = module,
              label, tags$i(class = "fa-solid fa-arrow-right"))
}

pl_home <- function() {
  tagList(
    tags$div(
      class = "pl-hero",
      tags$img(class = "pl-hero-avatar", src = "avatar_stiker.png", alt = "", `aria-hidden` = "true"),
      tags$div(
        tags$h2("Welcome to projectLSA", pl_version_badge("projectLSA")),
        tags$p("An interactive application for latent structure analysis, for research and teaching. ",
               "Upload your data or use the built-in examples, fit models, compare fit, and export everything, without writing code."),
        tags$div(
          class = "pl-cite",
          tags$b("How to cite "), "(in text: ", tags$span(class = "pl-intext", "Djidu et al., 2026"), ")",
          tags$br(),
          "Djidu, H., Retnawati, H., Hadi, S., & Haryanto. (2026). ",
          tags$em("projectLSA: A Shiny Application for Integrated Latent Structure Analysis."),
          " Applied Psychological Measurement. ",
          tags$a(href = "https://doi.org/10.1177/01466216261446305", target = "_blank", "doi:10.1177/01466216261446305"),
          tags$br(),
          "Djidu, H., Retnawati, H., Hadi, S., & Haryanto. (2026). ",
          tags$em("projectLSA: R Shiny application for latent structure analysis with a graphical user interface."),
          " R package. ",
          tags$a(href = "https://doi.org/10.32614/CRAN.package.projectLSA", target = "_blank", "doi:10.32614/CRAN.package.projectLSA")
        ),
        tags$div(
          class = "pl-row",
          downloadButton("download_ris_home", "Download citation (RIS)", class = "btn btn-info btn-sm"),
          actionButton("btn_donate_home", "Support projectLSA", icon = icon("heart"), class = "btn btn-danger btn-sm")
        )
      )
    ),

    tags$div(
      class = "pl-section-title",
      tags$h2("Which method fits your question?")
    ),

    # the four methods, laid out by what was measured and what is looked for
    tags$div(
      class = "pl-quad",
      tags$div(class = "pl-quad-corner"),
      tags$div(class = "pl-quad-col", tags$span("You measured"), tags$b("Continuous scores")),
      tags$div(class = "pl-quad-col", tags$span("You measured"), tags$b("Categorical answers")),

      tags$div(class = "pl-quad-row", tags$span("You look for"), tags$b("Groups")),
      pl_quad_cell("brand", "chart-simple", "LPA", "Latent Profile Analysis", "LPA · Latent Profile Analysis",
                   "Hidden groups of people, found from continuous scores.", go = "lpa"),
      pl_quad_cell("ok", "layer-group", "LCA", "Latent Class Analysis", "LCA · Latent Class Analysis",
                   "Hidden groups of people, found from categorical answers.", go = "lca"),

      tags$div(class = "pl-quad-row", tags$span("You look for"), tags$b("A score")),
      pl_quad_cell("bad", "sitemap", "EFA &middot; CFA", "Factor analysis", "Factor analysis · EFA and CFA / SEM",
                   "A score behind continuous measures: explore it with EFA, test it with CFA or SEM.",
                   go = NULL, pl_open_btn("efa", "EFA"), pl_open_btn("cfa", "CFA / SEM")),
      pl_quad_cell("warn", "brain", "LTA", "Latent Trait / IRT", "LTA · Latent Trait / IRT",
                   "A score behind categorical answers, as in tests and questionnaires.", go = "lta")
    ),

    tags$div(
      class = "pl-tip",
      tags$span(class = "pl-emoji", HTML("&#129302;")),
      tags$span(tags$b("AI Assistant. "),
                "Every method has an assistant (bottom right) that answers questions about your results and drafts report sections. ",
                "Add a model in the AI settings at the top right; free-tier providers are enough to start.")
    )
  )
}

# ---- report form: every part can be drafted or improved by the AI assistant --------

pl_report_fields <- function() {
  list(
    list(key = "objectives", label = "Objectives (the context of the analysis)", type = "area", rows = 3, ph = "What question does this analysis answer?"),
    list(key = "method",     label = "Method", type = "area", rows = 5, ph = "Press \"Fill from analysis\" to take it from the methods you used.", fill = TRUE, ai = FALSE),
    list(key = "results",    label = "Results and interpretation", type = "area", rows = 7, ph = "What do the results mean for your question?")
  )
}

pl_report_form <- function(mod) {
  fields <- lapply(pl_report_fields(), function(f) {
    id <- paste0(mod, "_rf_", f$key)
    ai <- !identical(f$ai, FALSE)
    tags$div(
      class = "pl-rfield",
      tags$div(
        class = "pl-rfield-head",
        tags$label(`for` = id, f$label),
        if (ai) actionButton(paste0(mod, "_rf_ai_", f$key), tagList(icon("wand-magic-sparkles"), " Write with AI"), class = "pl-rf-ai"),
        if (isTRUE(f$fill)) actionButton(paste0(mod, "_rf_fill_", f$key), tagList(icon("list-check"), " Fill from analysis"), class = "pl-rf-ai")
      ),
      if (identical(f$type, "text")) {
        textInput(id, NULL, placeholder = f$ph, width = "100%")
      } else {
        textAreaInput(id, NULL, rows = f$rows, placeholder = f$ph, width = "100%", resize = "vertical")
      }
    )
  })
  tags$div(
    class = "pl-rform",
    tags$div(
      class = "pl-rform-head",
      tags$img(src = "avatar_kepala.png", alt = ""),
      tags$div(tags$b("Report text"), tags$small("Three short parts. The tables, figures and references are added for you."))
    ),
    selectInput(paste0(mod, "_rf_lang"), "Language of the AI text", choices = c("English", "Indonesian"), width = "100%"),
    fields,
    tags$div(
      class = "pl-rform-actions",
      actionButton(paste0(mod, "_rf_fill_all"), tagList(icon("wand-magic-sparkles"), " Draft the empty parts"), class = "btn btn-info btn-sm"),
      actionButton(paste0(mod, "_rf_clear"), tagList(icon("eraser"), " Clear"), class = "btn btn-default btn-sm")
    ),
    tags$p(class = "pl-hint", "The assistant works from this module's results and never invents numbers. Always check its text before you use it.")
  )
}

# The whole Report tab: the form on the left, the preview on the right
pl_report_tab <- function(mod, generate_id, download_id, frame_id, word_id = paste0("download_word_", mod)) {
  pl_split(
    pl_side(pl_report_form(mod)),
    pl_main(
      pl_toolbar(
        "Report", "Preview it here, then download it as a Word file (tables, figures and APA 7 references included) or as HTML.",
        actionButton(generate_id, tagList(icon("rotate"), " Generate preview"), class = "btn btn-default btn-sm"),
        downloadButton(word_id, "Download Word (.docx)", class = "btn btn-primary btn-sm"),
        downloadButton(download_id, "Download HTML", class = "btn btn-default btn-sm")
      ),
      tags$div(class = "pl-frame", uiOutput(frame_id))
    )
  )
}
