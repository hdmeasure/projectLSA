# report_word.R
# Helpers shared by the Word (.docx) report templates (*_report_word.Rmd). The templates are
# rendered with pandoc and templates/apa_reference.docx, which carries the APA 7 look
# (Times New Roman 12, 1.5 line spacing, APA heading levels, table rules only, hanging-indent
# reference list). Every citation goes through cite()/cite_pkg(), so the reference list at the
# end contains exactly what was cited in the text.

W <- new.env()
W$tab <- 0L; W$fig <- 0L; W$keys <- character(0); W$pkgs <- character(0); W$typed <- character(0); W$dec <- "."; W$figdir <- tempdir()

w_init <- function(dec = ".", figdir = tempdir()) {
  W$tab <- 0L; W$fig <- 0L; W$keys <- character(0); W$pkgs <- character(0); W$typed <- character(0)
  W$dec <- if (is.null(dec) || !nzchar(dec)) "." else dec
  W$figdir <- figdir
  dir.create(figdir, showWarnings = FALSE, recursive = TRUE)
  invisible(NULL)
}

# ---- text ------------------------------------------------------------------
w_esc <- function(x) gsub("([\\\\`*_{}\\[\\]<>#|$])", "\\\\\\1", x, perl = TRUE)
w_style <- function(style, text) cat("::: {custom-style=\"", style, "\"}\n", text, "\n:::\n\n", sep = "")
w_h <- function(text, level = 1) cat(strrep("#", level), " ", text, "\n\n", sep = "")
w_p <- function(text) {
  text <- trimws(text); if (!nzchar(text)) return(invisible(NULL))
  for (para in strsplit(text, "\n+")[[1]]) cat(para, "\n\n", sep = "")
}
w_has <- function(nt, k) is.list(nt) && !is.null(nt[[k]]) && nzchar(trimws(nt[[k]]))
w_user <- function(text) { W$typed <- c(W$typed, text); w_p(w_esc(text)) }                   # the author's own text: never read as markup
w_fmt <- function(x, digits = 3, lead0 = TRUE) {
  if (!is.numeric(x)) return(as.character(x))
  out <- formatC(x, format = "f", digits = digits, decimal.mark = W$dec)
  out[is.na(x)] <- ""
  out
}
# APA p value: "< .001" or ".034" (no leading zero)
w_p_apa <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "< .001", sub("^0", "", formatC(p, format = "f", digits = 3, decimal.mark = W$dec))))
w_noz <- function(x, digits = 2) sub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))   # statistics bounded by 1

# ---- citations -------------------------------------------------------------
cite <- function(..., narrative = FALSE) {
  keys <- c(...); W$keys <- unique(c(W$keys, keys))
  lab <- unname(apa_intext_db[keys]); lab <- sort(lab)
  if (narrative) paste(lab, collapse = "; ") else paste0("(", paste(lab, collapse = "; "), ")")
}
cite_pkg <- function(...) {
  pk <- c(...); W$pkgs <- unique(c(W$pkgs, pk))
  paste0("(", paste(sort(vapply(pk, apa_pkg_intext, character(1))), collapse = "; "), ")")
}
w_refs <- function(extra_pkgs = character(0)) {
  # a work the author (or the assistant) cited in their own text belongs in the list too
  typed <- paste(W$typed, collapse = " \n ")
  for (k in names(apa_intext_db)) if (grepl(apa_intext_db[[k]], typed, fixed = TRUE)) W$keys <- unique(c(W$keys, k))
  for (pk in c("lavaan", "semTools", "semPlot", "psych", "mirt", "poLCA", "glca", "tidyLPA", "mclust"))
    if (grepl(apa_pkg_intext(pk), typed, fixed = TRUE)) W$pkgs <- unique(c(W$pkgs, pk))
  keys <- unique(c("djidu2026a", "djidu2026b", W$keys))
  refs <- c(unlist(apa_ref_db[intersect(keys, names(apa_ref_db))]), apa_r_ref(),
            stats::na.omit(vapply(unique(c(W$pkgs, extra_pkgs)), apa_pkg_ref, character(1))))
  refs <- unname(refs[nzchar(refs)])
  refs <- refs[order(tolower(gsub("[^A-Za-z, ]", "", refs)))]
  w_h("References", 1)
  for (r in refs) w_style("Bibliography", r)
}
w_software <- function() {
  paste0("The analysis was run in R (R Core Team, ", R.version$year, ") through the projectLSA application ",
         cite("djidu2026a", "djidu2026b"), ".")
}

# ---- tables ----------------------------------------------------------------
# df: a data frame, already rounded/formatted or numeric (numbers are formatted here)
w_table <- function(df, title, note = NULL, digits = 3, align = NULL) {
  if (is.null(df) || NROW(df) == 0) return(invisible(NULL))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  W$tab <- W$tab + 1L
  num <- vapply(df, is.numeric, logical(1))
  if (is.null(align)) align <- ifelse(num, "r", "l")
  for (j in which(num)) {
    d <- if (length(digits) > 1) digits[min(j, length(digits))] else digits
    x <- df[[j]]
    txt <- w_fmt(x, d)
    # APA: statistics that cannot exceed 1 are written without the leading zero
    if (d > 0 && any(!is.na(x)) && all(abs(x[!is.na(x)]) <= 1) && W$dec == ".") txt <- sub("^(-?)0\\.", "\\1.", txt)
    df[[j]] <- txt
  }
  df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- ""; gsub("\\|", "\\\\|", x) })
  w_style("TableLabel", paste0("Table ", W$tab))
  w_style("TableTitle", w_esc(title))
  cat(knitr::kable(df, format = "pipe", align = align, row.names = FALSE), sep = "\n")
  cat("\n\n")
  if (!is.null(note) && nzchar(note)) w_style("NoteText", paste0("*Note.* ", note))
}

# ---- figures ---------------------------------------------------------------
# draw: a ggplot object or a function that draws a base graphic
w_figure <- function(draw, title, note = NULL, width = 6.5, height = 4.2) {
  if (is.null(draw)) return(invisible(NULL))
  W$fig <- W$fig + 1L
  f <- file.path(W$figdir, sprintf("figure_%02d.png", W$fig))
  grDevices::png(f, width = width, height = height, units = "in", res = 220, bg = "white")
  dev <- grDevices::dev.cur()
  ok <- tryCatch({
    if (inherits(draw, "ggplot")) print(draw) else if (is.function(draw)) draw() else print(draw)
    TRUE
  }, error = function(e) { W$last_error <- conditionMessage(e); FALSE })
  if (dev %in% grDevices::dev.list()) grDevices::dev.off(dev)      # the file is complete only once the device is closed
  if (!ok || !file.exists(f)) { W$fig <- W$fig - 1L; return(invisible(NULL)) }
  w_style("FigureLabel", paste0("Figure ", W$fig))
  w_style("FigureTitle", w_esc(title))
  cat("::: {custom-style=\"FigureImage\"}\n![](", f, "){width=", round(width * 0.97, 2), "in}\n:::\n\n", sep = "")
  if (!is.null(note) && nzchar(note)) w_style("NoteText", paste0("*Note.* ", note))
}

# ---- front and back matter -------------------------------------------------
w_cover <- function(nt, default_title) {
  has <- function(k) is.list(nt) && !is.null(nt[[k]]) && nzchar(nt[[k]])
  w_style("Title", w_esc(if (has("title")) nt$title else default_title))
  if (has("authors")) w_style("Author", w_esc(nt$authors))
  w_style("Date", format(Sys.Date(), "%B %d, %Y"))
}
w_notes <- function(nt, keys) {
  lab <- c(background = "Background", objectives = "Objectives", method = "Method",
           results = "Interpretation of the Results", conclusion = "Conclusion", limits = "Limitations and Recommendations")
  for (k in keys) if (is.list(nt) && !is.null(nt[[k]]) && nzchar(nt[[k]])) { w_h(lab[[k]], 1); w_user(nt[[k]]) }
}
w_ai <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(invisible(NULL))
  w_h("AI-Assisted Interpretation", 1)
  w_p("*The text below was drafted by an AI assistant from the analysis output. It has not been verified; check it against the results before use.*")
  w_p(txt)
}
w_console <- function(txt) {
  if (is.null(txt) || identical(txt, NA) || !nzchar(paste(txt, collapse = ""))) return(invisible(NULL))
  w_h("Appendix: R Console Output", 1)
  lines <- unlist(strsplit(paste(txt, collapse = "\n"), "\n"))
  lines <- gsub("\033\\[[0-9;]*m", "", lines)
  lead <- function(l) { n <- nchar(l) - nchar(sub("^ +", "", l)); paste0(strrep("&#160;", n), sub("^ +", "", l)) }
  for (l in lines) w_style("Console", if (nzchar(l)) w_esc(lead(l)) else "&#160;")
}

# ---- the Method paragraph, written from what was actually run -------------------------
# Used by the Word templates and by the "Fill from analysis" button of the report form.
method_text <- function(mod, ctx = list()) {
  sw <- paste0("The analysis was run in R (R Core Team, ", R.version$year, ") through the projectLSA application ",
               cite("djidu2026a", "djidu2026b"), ".")
  vars <- ctx$vars; nv <- length(vars)
  vtxt <- if (nv) paste0(" The ", nv, " indicators were ", paste(vars, collapse = ", "), ".") else ""
  out <- switch(mod,
    lpa = paste0(
      "Latent profile analysis (LPA) was used to identify subgroups that share a similar pattern on continuous indicators ", cite("masyn2013"), ". ", sw,
      " Profile models were fitted with the tidyLPA package ", cite_pkg("tidyLPA"), ", which estimates finite Gaussian mixture models through mclust ", cite_pkg("mclust"), ".", vtxt,
      " The number of profiles was chosen from AIC, BIC, entropy, the size of the smallest profile and the bootstrap likelihood ratio test, together with interpretability ", cite("nylund2007"), ".",
      if (!is.null(ctx$k) && !is.na(ctx$k)) paste0(" The retained solution has ", ctx$k, " profiles.") else ""),
    lca = paste0(
      "Latent class analysis (LCA) was used to identify unobserved subgroups from categorical responses ", cite("collins2010"), ". ", sw,
      " Class models were estimated by maximum likelihood with the poLCA package ", cite_pkg("poLCA"), ".", vtxt,
      " The number of classes was chosen from AIC, BIC, entropy, average posterior probabilities, the size of the smallest class and interpretability ", cite("nylund2007"), ".",
      if (!is.null(ctx$k) && !is.na(ctx$k)) paste0(" The retained solution has ", ctx$k, " classes.") else ""),
    efa = {
      fa <- ctx$fa
      paste0("Exploratory factor analysis (EFA) was used to explore the number and nature of the factors behind the items ", cite("fabrigar1999"), ". ", sw,
             " Factors were extracted with the psych package ", cite_pkg("psych"), ".",
             if (!is.null(fa)) paste0(" The solution has ", ncol(fa$loadings), " factor", if (ncol(fa$loadings) > 1) "s" else "",
                                      ", extracted by the \"", fa$fm, "\" method", if (!is.null(fa$rotation) && nzchar(fa$rotation)) paste0(" with \"", fa$rotation, "\" rotation") else "",
                                      ", on ", fa$n.obs, " observations.") else "")
    },
    cfa = {
      fit <- ctx$fit; sem <- isTRUE(ctx$sem)
      ord <- isTRUE(tryCatch(lavaan::lavInspect(fit, "categorical"), error = function(e) FALSE))
      paste0(if (sem) "Structural equation modeling (SEM)" else "Confirmatory factor analysis (CFA)", " was used to test the hypothesised measurement structure",
             if (sem) " and the relations among the latent variables" else "", ". ", sw, " The model was estimated with the lavaan package ", cite_pkg("lavaan"),
             " using the ", toupper(fit@Options$estimator), " estimator",
             if (ord) paste0(", with the indicators treated as ordered categorical variables ", cite("li2016")) else "", ".",
             " Model fit was judged against commonly used criteria ", cite("hu1999", "kline2015"), ".")
    },
    lta = {
      it <- tryCatch(paste(unique(mirt::extract.mirt(ctx$res, "itemtype")), collapse = ", "), error = function(e) "")
      paste0("Item response theory (IRT) relates the probability of each response to a latent trait ", cite("embretson2000"), ". ", sw,
             " Item parameters were estimated with the mirt package ", cite_pkg("mirt"), if (nzchar(it)) paste0(" (item type: ", it, ")") else "", ".")
    },
    "")
  out
}
