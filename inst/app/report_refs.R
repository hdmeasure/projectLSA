# report_refs.R
# APA 7 references for the Word report. Method references are written out by hand; package
# references are built from each package's own citation() record, so they match the installed
# version. Entries use markdown (*italic*) for pandoc.

apa_ref_db <- list(
  djidu2026a = "Djidu, H., Retnawati, H., Hadi, S., & Haryanto. (2026a). projectLSA: A Shiny application for integrated latent structure analysis. *Applied Psychological Measurement*. https://doi.org/10.1177/01466216261446305",
  djidu2026b = "Djidu, H., Retnawati, H., Hadi, S., & Haryanto. (2026b). *projectLSA: R Shiny application for latent structure analysis with a graphical user interface* [R package]. https://doi.org/10.32614/CRAN.package.projectLSA",
  nylund2007 = "Nylund, K. L., Asparouhov, T., & Muth&#233;n, B. O. (2007). Deciding on the number of classes in latent class analysis and growth mixture modeling: A Monte Carlo simulation study. *Structural Equation Modeling, 14*(4), 535&#8211;569. https://doi.org/10.1080/10705510701575396",
  masyn2013 = "Masyn, K. E. (2013). Latent class analysis and finite mixture modeling. In T. D. Little (Ed.), *The Oxford handbook of quantitative methods: Vol. 2. Statistical analysis* (pp. 551&#8211;611). Oxford University Press.",
  rosenberg2018 = "Rosenberg, J. M., Beymer, P. N., Anderson, D. J., Van Lissa, C. J., & Schmidt, J. A. (2018). tidyLPA: An R package to easily carry out latent profile analysis (LPA) using open-source or commercial software. *Journal of Open Source Software, 3*(30), 978. https://doi.org/10.21105/joss.00978",
  collins2010 = "Collins, L. M., & Lanza, S. T. (2010). *Latent class and latent transition analysis: With applications in the social, behavioral, and health sciences*. Wiley.",
  hu1999 = "Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Conventional criteria versus new alternatives. *Structural Equation Modeling, 6*(1), 1&#8211;55. https://doi.org/10.1080/10705519909540118",
  kline2015 = "Kline, R. B. (2015). *Principles and practice of structural equation modeling* (4th ed.). Guilford Press.",
  hair2010 = "Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2010). *Multivariate data analysis* (7th ed.). Pearson.",
  chen2001 = "Chen, F., Bollen, K. A., Paxton, P., Curran, P. J., & Kirby, J. B. (2001). Improper solutions in structural equation models: Causes, consequences, and strategies. *Sociological Methods & Research, 29*(4), 468&#8211;508. https://doi.org/10.1177/0049124101029004003",
  li2016 = "Li, C.-H. (2016). Confirmatory factor analysis with ordinal data: Comparing robust maximum likelihood and diagonally weighted least squares. *Behavior Research Methods, 48*(3), 936&#8211;949. https://doi.org/10.3758/s13428-015-0619-7",
  henseler2015 = "Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for assessing discriminant validity in variance-based structural equation modeling. *Journal of the Academy of Marketing Science, 43*(1), 115&#8211;135. https://doi.org/10.1007/s11747-014-0403-8",
  fornell1981 = "Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models with unobservable variables and measurement error. *Journal of Marketing Research, 18*(1), 39&#8211;50. https://doi.org/10.1177/002224378101800104",
  kaiser1974 = "Kaiser, H. F. (1974). An index of factorial simplicity. *Psychometrika, 39*(1), 31&#8211;36. https://doi.org/10.1007/BF02291575",
  horn1965 = "Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. *Psychometrika, 30*(2), 179&#8211;185. https://doi.org/10.1007/BF02289447",
  fabrigar1999 = "Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J. (1999). Evaluating the use of exploratory factor analysis in psychological research. *Psychological Methods, 4*(3), 272&#8211;299. https://doi.org/10.1037/1082-989X.4.3.272",
  muraki1992 = "Muraki, E. (1992). A generalized partial credit model: Application of an EM algorithm. *Applied Psychological Measurement, 16*(2), 159&#8211;176. https://doi.org/10.1177/014662169201600206",
  embretson2000 = "Embretson, S. E., & Reise, S. P. (2000). *Item response theory for psychologists*. Lawrence Erlbaum Associates.",
  orlando2000 = "Orlando, M., & Thissen, D. (2000). Likelihood-based item-fit indices for dichotomous item response theory models. *Applied Psychological Measurement, 24*(1), 50&#8211;64. https://doi.org/10.1177/01466216000241003"
)

# One person as "Family, G. H."
apa_person <- function(p) {
  fam <- paste(p$family, collapse = " ")
  giv <- unlist(strsplit(paste(p$given, collapse = " "), "[ ]+"))
  if (!nzchar(fam) && length(giv) > 1) { fam <- giv[length(giv)]; giv <- giv[-length(giv)] }
  ini <- vapply(giv, function(g) paste0(paste0(substr(strsplit(g, "-")[[1]], 1, 1), collapse = ".-"), "."), character(1))
  if (!length(ini) || !nzchar(fam)) return(paste(c(p$given, p$family), collapse = " "))
  paste0(fam, ", ", paste(ini, collapse = " "))
}

apa_authors <- function(persons) {
  a <- vapply(persons, apa_person, character(1))
  n <- length(a)
  if (n == 0) return("")
  if (n == 1) return(a)
  if (n == 2) return(paste0(a[1], ", & ", a[2]))
  if (n <= 20) return(paste0(paste(a[-n], collapse = ", "), ", & ", a[n]))
  paste0(paste(a[1:19], collapse = ", "), ", ... ", a[n])
}

# APA 7 entry for an installed package, from its citation() record
apa_pkg_ref <- function(pkg) {
  cit <- tryCatch(suppressWarnings(utils::citation(pkg))[1], error = function(e) NULL)
  if (is.null(cit)) return(NA_character_)
  g <- function(f) { v <- tryCatch(do.call("$", list(cit, f)), error = function(e) NULL); if (is.null(v) || !length(v)) "" else paste(v, collapse = " ") }
  auth <- apa_authors(cit$author)
  if (!nzchar(auth)) auth <- paste0(pkg, " authors")
  yr <- g("year"); if (!nzchar(yr)) yr <- "n.d."
  tidy <- function(x) gsub("\\s+", " ", gsub("\\\\texttt\\{([^}]*)\\}|\\{|\\}", "\\1", x))
  title <- tidy(g("title"))
  doi <- g("doi"); url <- g("url")
  link <- if (nzchar(doi)) paste0("https://doi.org/", sub("^https?://(dx\\.)?doi\\.org/", "", doi)) else url
  if (nzchar(g("journal"))) {
    vol <- g("volume"); num <- g("number"); pg <- gsub("--|-", "&#8211;", g("pages"))
    src <- paste0("*", g("journal"), if (nzchar(vol)) paste0(", ", vol, "*") else "*", if (nzchar(num)) paste0("(", num, ")") else "", if (nzchar(pg)) paste0(", ", pg) else "")
    paste0(auth, " (", yr, "). ", sub("\\.$", "", title), ". ", src, ".", if (nzchar(link)) paste0(" ", link) else "")
  } else {
    ver <- as.character(utils::packageVersion(pkg))
    paste0(auth, " (", yr, "). *", sub("\\.$", "", title), "* (Version ", ver, ") [R package]. ",
           if (nzchar(g("organization"))) paste0(g("organization"), ". ") else "", if (nzchar(link)) link else "")
  }
}

apa_r_ref <- function() {
  yr <- R.version$year
  paste0("R Core Team. (", yr, "). *R: A language and environment for statistical computing* [Computer software]. R Foundation for Statistical Computing. https://www.R-project.org/")
}

# Reference list (sorted, as APA requires) for a module.
#   keys  names in apa_ref_db        pkgs  installed packages cited through citation()
report_reference_list <- function(keys = character(0), pkgs = character(0)) {
  refs <- c(unlist(apa_ref_db[unique(c("djidu2026a", "djidu2026b", keys))]), apa_r_ref(),
            stats::na.omit(vapply(pkgs, apa_pkg_ref, character(1))))
  refs <- unname(refs[nzchar(refs)])
  key <- tolower(gsub("[^A-Za-z, ]", "", refs))
  refs[order(key)]
}

# In-text form of each hand-written reference
apa_intext_db <- c(
  djidu2026a = "Djidu et al., 2026a", djidu2026b = "Djidu et al., 2026b", nylund2007 = "Nylund et al., 2007",
  masyn2013 = "Masyn, 2013", collins2010 = "Collins & Lanza, 2010", hu1999 = "Hu & Bentler, 1999",
  kline2015 = "Kline, 2015", hair2010 = "Hair et al., 2010", chen2001 = "Chen et al., 2001", li2016 = "Li, 2016",
  henseler2015 = "Henseler et al., 2015", fornell1981 = "Fornell & Larcker, 1981", kaiser1974 = "Kaiser, 1974",
  horn1965 = "Horn, 1965", fabrigar1999 = "Fabrigar et al., 1999", muraki1992 = "Muraki, 1992",
  embretson2000 = "Embretson & Reise, 2000", orlando2000 = "Orlando & Thissen, 2000", rosenberg2018 = "Rosenberg et al., 2018"
)

# In-text form of an installed package's citation: "Rosseel et al., 2025"
apa_pkg_intext <- function(pkg) {
  cit <- tryCatch(suppressWarnings(utils::citation(pkg))[1], error = function(e) NULL)
  if (is.null(cit)) return(pkg)
  pe <- cit$author; yr <- tryCatch(as.character(cit$year), error = function(e) "n.d.")
  if (!length(yr) || !nzchar(yr[1])) yr <- "n.d."
  fam <- vapply(pe, function(p) { f <- paste(p$family, collapse = " "); if (nzchar(f)) f else { g <- unlist(strsplit(paste(p$given, collapse = " "), " ")); g[length(g)] } }, character(1))
  a <- if (length(fam) == 1) fam else if (length(fam) == 2) paste(fam, collapse = " & ") else paste(fam[1], "et al.")
  paste0(a, ", ", yr[1])
}

# In-text citations the assistant may use in a module's report text (and nothing else)
module_cite_hint <- function(mod) {
  keys <- switch(mod,
    lpa = c("masyn2013", "nylund2007"), lca = c("collins2010", "nylund2007", "masyn2013"),
    efa = c("fabrigar1999", "kaiser1974", "horn1965"),
    cfa = c("hu1999", "kline2015", "hair2010", "fornell1981", "henseler2015", "chen2001", "li2016"),
    lta = c("embretson2000", "muraki1992", "orlando2000"), character(0))
  unname(apa_intext_db[keys])
}
