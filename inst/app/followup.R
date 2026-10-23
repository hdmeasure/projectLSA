# followup.R
# Follow-up analyses after a latent profile / latent class solution has been chosen. Both methods share
# these five views:
#   Classification  how reliably respondents are assigned (sizes, posterior probabilities, entropy)
#   Compare         the classes on other numeric variables (outcomes): descriptives, omnibus tests,
#                   effect sizes, pairwise comparisons, a plot
#   Cross-tabs      the classes by categorical variables: counts, percentages, chi-square, Cramer's V,
#                   standardized residuals
#   Predictors      who belongs to which class: multinomial logistic regression with odds ratios
#   Data & export   the data with the class and the posterior probabilities
# The caller passes assign(): list(data = data frame with the class column, cls = name of that column,
# post = matrix of posterior probabilities (rows aligned with data, NA for rows not classified)).

# ---- UI -----------------------------------------------------------------------------------------
followup_tabs <- function(p, unit = "profile") {
  id <- function(x) paste0(p, "_fu_", x)
  Unit <- paste0(toupper(substring(unit, 1, 1)), substring(unit, 2))
  list(
    tabPanel(
      title = tagList(icon("bullseye"), "Classification"), value = id("class_tab"),
      uiOutput(id("chips")),
      tags$div(
        class = "pl-two pl-two-flat",
        tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", paste(Unit, "sizes and classification accuracy")), uiOutput(id("sizes"))),
        tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "Average posterior probabilities"), uiOutput(id("matrix")))
      ),
      tags$section(class = "pl-flat", tags$h4(class = "pl-flat-title", "How certain are the assignments?"), plotOutput(id("cert"), height = "230px"))
    ),
    tabPanel(
      title = tagList(icon("scale-balanced"), paste("Compare", paste0(unit, "s"))), value = id("cmp_tab"),
      pl_split(
        pl_side(
          width = 3,
          selectizeInput(id("cmp_vars"), "Variables to compare", choices = NULL, multiple = TRUE, options = list(placeholder = "Choose numeric variables")),
          radioButtons(id("cmp_test"), "Test", choices = c("ANOVA (Welch)" = "anova", "Kruskal-Wallis (rank based)" = "kw"), selected = "anova"),
          selectInput(id("cmp_adj"), "Pairwise adjustment", choices = c("Holm" = "holm", "Bonferroni" = "bonferroni", "Benjamini-Hochberg" = "BH", "None" = "none")),
          selectInput(id("cmp_plot"), "Plot", choices = c("Means with 95% CI" = "ci", "Bars (means)" = "bar", "Boxplot" = "box")),
          uiOutput(id("cmp_pair_var")),
          tags$p(class = "pl-hint", paste0("Compare the ", unit, "s on outcomes that were not used to form them. ",
                                           "Effect sizes: eta squared (ANOVA) or epsilon squared (Kruskal-Wallis)."))
        ),
        pl_main(width = 9, uiOutput(id("cmp_out")))
      )
    ),
    tabPanel(
      title = tagList(icon("table-cells"), "Cross-tabs"), value = id("xtab_tab"),
      pl_split(
        pl_side(
          width = 3,
          selectInput(id("xt_var"), "Categorical variable", choices = NULL),
          radioButtons(id("xt_base"), "Percentages", choices = c(paste("Within each", unit), "Within each category"), selected = paste("Within each", unit)),
          checkboxInput(id("xt_res"), "Show standardized residuals", TRUE),
          tags$p(class = "pl-hint", "A standardized residual beyond ±1.96 marks a cell that has more or fewer respondents than chance would give.")
        ),
        pl_main(width = 9, uiOutput(id("xt_out")))
      )
    ),
    tabPanel(
      title = tagList(icon("arrow-trend-up"), "Predictors"), value = id("pred_tab"),
      pl_split(
        pl_side(
          width = 3,
          selectizeInput(id("pr_vars"), "Predictors (covariates)", choices = NULL, multiple = TRUE, options = list(placeholder = "Choose one or more")),
          uiOutput(id("pr_ref")),
          actionButton(id("pr_run"), tagList(icon("play"), "Run regression"), class = "btn btn-success btn-block"),
          tags$p(class = "pl-hint", paste0("Multinomial logistic regression of ", unit, " membership on the predictors. ",
                                           "It uses the most likely ", unit, " of each respondent and so ignores classification error; ",
                                           "treat the odds ratios as approximate when entropy is below .80."))
        ),
        pl_main(width = 9, uiOutput(id("pr_out")))
      )
    ),
    tabPanel(
      title = tagList(icon("file-export"), "Data & Export"), value = id("data_tab"),
      tags$div(class = "pl-toolbar",
               tags$div(class = "pl-toolbar-text", tags$b(paste("Data with", unit, "membership")), tags$span("The most likely class and the posterior probability of every class.")),
               tags$div(class = "pl-toolbar-actions", downloadButton(id("dl_csv"), "Download CSV", class = "btn btn-default btn-sm"))),
      DT::DTOutput(id("data"))
    )
  )
}

# ---- helpers ------------------------------------------------------------------------------------
fu_fmt <- function(x, d = 2) ifelse(is.na(x), "", formatC(x, format = "f", digits = d))
fu_p <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "< .001", sub("^0", "", formatC(p, format = "f", digits = 3))))
fu_wilson <- function(x, n, z = 1.96) {
  ph <- x / n; den <- 1 + z^2 / n
  ctr <- (ph + z^2 / (2 * n)) / den; hw <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / den
  cbind(lo = pmax(0, ctr - hw), hi = pmin(1, ctr + hw))
}
fu_tone <- function(v, good, ok) ifelse(is.na(v), NA, ifelse(v >= good, "good", ifelse(v >= ok, "warn", "bad")))
fu_table <- function(df, class = NULL, tones = NULL, align_left = 1) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  th <- tags$tr(lapply(names(df), tags$th))
  body <- lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(seq_along(df), function(j) {
    tn <- if (!is.null(tones) && !is.null(tones[[j]])) tones[[j]][i] else NA
    st <- if (!is.na(tn)) c(good = "background-color:#d9f1ee;color:#065f58;font-weight:700", warn = "background-color:#fbefd2;color:#8a5a0b;font-weight:700", bad = "background-color:#fbe0e3;color:#8f1d2a;font-weight:700")[[tn]] else NULL
    if (j %in% align_left) tags$td(class = "pl-lbl", df[i, j]) else tags$td(style = st, df[i, j])
  })))
  tags$div(class = "pl-table-wrap", tags$table(class = paste("pl-table", class), tags$thead(th), tags$tbody(body)))
}
fu_note <- function(...) tags$p(class = "pl-stat-note", ...)
fu_chip <- function(k, v) tags$span(class = "pl-chip-kv", tags$small(k), tags$b(v))

# ---- server -------------------------------------------------------------------------------------
server_followup <- function(input, output, session, p, assign, unit = "profile") {
  id <- function(x) paste0(p, "_fu_", x)
  inp <- function(x) input[[id(x)]]
  Unit <- paste0(toupper(substring(unit, 1, 1)), substring(unit, 2))
  pal <- function(n) rep_len(pl_palettes$brand, max(n, 1))

  A <- reactive({ a <- assign(); req(a, a$data, a$cls, a$post); a })
  cls <- reactive({ a <- A(); f <- a$data[[a$cls]]; factor(f, levels = unique(as.character(f[!is.na(f)]))) })
  classified <- reactive({ a <- A(); !is.na(a$data[[a$cls]]) & stats::complete.cases(a$post) })
  skip <- function(a) unique(c(a$cls, a$skip, grep("^(Posterior_Prob_|CPROB|Respon$|id_auto$)", names(a$data), value = TRUE)))
  num_vars <- reactive({ a <- A(); d <- a$data; n <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], skip(a)); n })
  cat_vars <- reactive({
    a <- A(); d <- a$data
    names(d)[vapply(names(d), function(v) !(v %in% skip(a)) && (is.factor(d[[v]]) || is.character(d[[v]]) || (is.numeric(d[[v]]) && length(unique(stats::na.omit(d[[v]]))) <= 8)), logical(1))]
  })

  observe({
    updateSelectizeInput(session, id("cmp_vars"), choices = num_vars(), selected = isolate(inp("cmp_vars")), server = FALSE)
    updateSelectInput(session, id("xt_var"), choices = cat_vars(), selected = isolate(inp("xt_var")))
    updateSelectizeInput(session, id("pr_vars"), choices = names(A()$data)[!(names(A()$data) %in% skip(A()))], selected = isolate(inp("pr_vars")), server = FALSE)
  })

  # ---- classification quality ----
  qual <- reactive({
    a <- A(); ok <- classified(); P <- a$post[ok, , drop = FALSE]; g <- cls()[ok]
    K <- ncol(P); n <- nrow(P)
    ent <- -rowSums(ifelse(P > 0, P * log(P), 0))
    rel_ent <- if (K > 1) 1 - sum(ent) / (n * log(K)) else NA
    maxp <- apply(P, 1, max)
    list(P = P, g = g, K = K, n = n, rel_ent = rel_ent, maxp = maxp)
  })
  output[[id("chips")]] <- renderUI({
    q <- qual()
    tags$div(class = "pl-chips", fu_chip("Classified", q$n), fu_chip(paste0(Unit, "s"), q$K),
             fu_chip("Relative entropy", sub("^0", "", fu_fmt(q$rel_ent, 3))), fu_chip("Mean top probability", sub("^0", "", fu_fmt(mean(q$maxp), 3))),
             fu_chip("Assigned with ≥ .70", sprintf("%d%%", round(100 * mean(q$maxp >= .7)))))
  })
  output[[id("sizes")]] <- renderUI({
    q <- qual(); lv <- levels(q$g); tab <- table(q$g)[lv]; ci <- fu_wilson(as.numeric(tab), q$n)
    avepp <- vapply(seq_along(lv), function(k) mean(q$P[q$g == lv[k], k]), numeric(1))
    df <- data.frame(Class = lv, n = as.integer(tab), `%` = fu_fmt(100 * as.numeric(tab) / q$n, 1),
                     `95% CI` = paste0(fu_fmt(100 * ci[, 1], 1), " – ", fu_fmt(100 * ci[, 2], 1)), AvePP = sub("^0", "", fu_fmt(avepp, 3)), check.names = FALSE)
    names(df)[1] <- Unit
    tagList(fu_table(df, tones = list(NULL, NULL, NULL, NULL, fu_tone(avepp, .8, .7))),
            fu_note("AvePP is the average posterior probability of the assigned ", unit, ". Values of .70 or higher are usually considered acceptable and .80 or higher good (Nagin, 2005). Percentage CIs are Wilson intervals."))
  })
  output[[id("matrix")]] <- renderUI({
    q <- qual(); lv <- levels(q$g)
    M <- t(vapply(lv, function(l) colMeans(q$P[q$g == l, , drop = FALSE]), numeric(q$K)))
    df <- data.frame(lv, matrix(sub("^0", "", fu_fmt(M, 3)), nrow = length(lv)), check.names = FALSE)
    names(df) <- c(paste("Assigned to", unit), paste("P:", lv))
    tn <- lapply(seq_len(ncol(df)), function(j) if (j == 1) NULL else vapply(seq_along(lv), function(i) if (i == j - 1) fu_tone(M[i, j - 1], .8, .7) else NA_character_, ""))
    tagList(fu_table(df, tones = tn), fu_note("Rows are the assigned ", unit, ", columns the average probability of each ", unit, ". The diagonal should be high and the off-diagonal cells low."))
  })
  output[[id("cert")]] <- renderPlot({
    q <- qual(); lv <- levels(q$g)
    band <- cut(q$maxp, c(0, .7, .9, 1.0001), labels = c("below .70", ".70 to .89", ".90 and above"), right = FALSE, include.lowest = TRUE)
    d <- as.data.frame(prop.table(table(factor(q$g, levels = lv), band), 1) * 100)
    names(d) <- c("cls", "band", "pct")
    ggplot2::ggplot(d, ggplot2::aes(cls, pct, fill = band)) +
      geom_col(width = 0.6, colour = "white", linewidth = 0.5) +
      ggplot2::scale_fill_manual(values = c("below .70" = "#e9a3ab", ".70 to .89" = "#f2cf86", ".90 and above" = "#3aa59a"), name = "Top posterior probability") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .03))) +
      ggplot2::labs(x = NULL, y = "% of respondents") + theme_pl(11) + ggplot2::coord_flip() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_line(colour = "#e7e5f4", linewidth = .4), plot.margin = ggplot2::margin(4, 10, 2, 6))
  })

  # ---- compare ----
  output[[id("cmp_pair_var")]] <- renderUI({
    v <- inp("cmp_vars"); req(length(v) > 0)
    selectInput(id("cmp_focus"), "Show pairwise tests and plot for", choices = v, selected = isolate(inp("cmp_focus")))
  })
  cmp_stats <- reactive({
    v <- inp("cmp_vars"); req(length(v) > 0)
    a <- A(); ok <- classified(); g <- droplevels(cls()[ok]); d <- a$data[ok, , drop = FALSE]
    test <- inp("cmp_test") %||% "anova"
    desc <- do.call(rbind, lapply(v, function(x) {
      y <- d[[x]]; do.call(rbind, lapply(levels(g), function(l) {
        yy <- y[g == l & !is.na(y)]; n <- length(yy); m <- mean(yy); s <- stats::sd(yy); se <- s / sqrt(n)
        tq <- if (n > 1) stats::qt(.975, n - 1) else NA
        data.frame(Variable = x, Class = l, n = n, M = m, SD = s, lo = m - tq * se, hi = m + tq * se, stringsAsFactors = FALSE)
      }))
    }))
    omni <- do.call(rbind, lapply(v, function(x) {
      y <- d[[x]]; ok2 <- !is.na(y) & !is.na(g); y <- y[ok2]; gg <- droplevels(g[ok2]); N <- length(y)
      if (nlevels(gg) < 2 || N < 3) return(data.frame(Variable = x, Test = "", Statistic = NA, df = NA, p = NA, Effect = NA, Measure = "", stringsAsFactors = FALSE))
      if (identical(test, "kw")) {
        k <- stats::kruskal.test(y ~ gg); eps <- unname(k$statistic) * (N + 1) / (N^2 - 1)
        data.frame(Variable = x, Test = "Kruskal-Wallis", Statistic = unname(k$statistic), df = unname(k$parameter), p = k$p.value, Effect = eps, Measure = "ε²", stringsAsFactors = FALSE)
      } else {
        o <- stats::oneway.test(y ~ gg, var.equal = FALSE); ss <- summary(stats::aov(y ~ gg))[[1]]; eta <- ss[1, 2] / sum(ss[, 2])
        data.frame(Variable = x, Test = "Welch ANOVA", Statistic = unname(o$statistic), df = unname(o$parameter[1]), p = o$p.value, Effect = eta, Measure = "η²", stringsAsFactors = FALSE)
      }
    }))
    list(desc = desc, omni = omni, d = d, g = g, test = test)
  })
  output[[id("cmp_out")]] <- renderUI({
    req(length(inp("cmp_vars")) > 0)
    s <- cmp_stats(); desc <- s$desc; adj <- inp("cmp_adj") %||% "holm"
    dd <- data.frame(Variable = desc$Variable, Class = desc$Class, n = desc$n, M = fu_fmt(desc$M), SD = fu_fmt(desc$SD), `95% CI` = paste0(fu_fmt(desc$lo), ", ", fu_fmt(desc$hi)), check.names = FALSE)
    names(dd)[2] <- Unit
    om <- s$omni
    oo <- data.frame(Variable = om$Variable, Test = om$Test, Statistic = fu_fmt(om$Statistic), df = fu_fmt(om$df, 1), p = fu_p(om$p), Effect = paste0(om$Measure, " = ", sub("^0", "", fu_fmt(om$Effect, 3))), check.names = FALSE)
    oo$Effect[is.na(om$Effect)] <- ""
    tn <- list(NULL, NULL, NULL, NULL, ifelse(is.na(om$p), NA, ifelse(om$p < .05, "good", NA)), NULL)
    focus <- inp("cmp_focus") %||% inp("cmp_vars")[1]
    pw <- tryCatch({
      y <- s$d[[focus]]; g <- s$g
      m <- if (identical(s$test, "kw")) suppressWarnings(stats::pairwise.wilcox.test(y, g, p.adjust.method = adj, exact = FALSE))$p.value
           else stats::pairwise.t.test(y, g, p.adjust.method = adj, pool.sd = FALSE)$p.value
      lv <- levels(g); out <- NULL
      for (i in seq_along(lv)) for (j in seq_along(lv)) if (j < i) {
        pv <- m[lv[i], lv[j]]; md <- mean(y[g == lv[i]], na.rm = TRUE) - mean(y[g == lv[j]], na.rm = TRUE)
        out <- rbind(out, data.frame(Comparison = paste(lv[i], "vs", lv[j]), `Mean difference` = fu_fmt(md), `p (adjusted)` = fu_p(pv), check.names = FALSE, stringsAsFactors = FALSE))
        attr(out, "p") <- c(attr(out, "p"), pv)
      }
      pvals <- attr(out, "p"); attr(out, "p") <- NULL
      list(df = out, tone = ifelse(is.na(pvals), NA, ifelse(pvals < .05, "good", NA)))
    }, error = function(e) NULL)
    tagList(
      tags$h4(class = "pl-flat-title", "Descriptives"), fu_table(dd, align_left = 1:2),
      tags$h4(class = "pl-flat-title", "Omnibus tests"), fu_table(oo, tones = tn),
      fu_note("Cells in green: p < .05. Effect sizes of about .01, .06 and .14 are conventionally small, medium and large for η² (Cohen, 1988)."),
      if (!is.null(pw)) tagList(tags$h4(class = "pl-flat-title", paste0("Pairwise comparisons: ", focus)), fu_table(pw$df, tones = list(NULL, NULL, pw$tone))),
      tags$h4(class = "pl-flat-title", paste0("Plot: ", focus)), plotOutput(id("cmp_plot"), height = "300px")
    )
  })
  output[[id("cmp_plot")]] <- renderPlot({
    s <- cmp_stats(); focus <- inp("cmp_focus") %||% inp("cmp_vars")[1]; req(focus)
    d <- s$desc[s$desc$Variable == focus, ]; d$Class <- factor(d$Class, levels = levels(s$g)); type <- inp("cmp_plot") %||% "ci"
    cols <- pal(nlevels(s$g))
    base <- ggplot2::ggplot() + theme_pl(11) + ggplot2::labs(x = NULL, y = focus)
    if (type == "box") {
      raw <- data.frame(y = s$d[[focus]], Class = s$g)
      base + ggplot2::geom_boxplot(data = raw, ggplot2::aes(Class, y, fill = Class), width = 0.5, outlier.size = 1, linewidth = 0.4, alpha = 0.85, show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = cols)
    } else if (type == "bar") {
      base + geom_col(data = d, ggplot2::aes(Class, M, fill = Class), width = 0.55, show.legend = FALSE) +
        ggplot2::geom_errorbar(data = d, ggplot2::aes(Class, ymin = lo, ymax = hi), width = 0.12, linewidth = 0.5) + ggplot2::scale_fill_manual(values = cols)
    } else {
      base + ggplot2::geom_errorbar(data = d, ggplot2::aes(Class, ymin = lo, ymax = hi, colour = Class), width = 0.1, linewidth = 0.6, show.legend = FALSE) +
        ggplot2::geom_point(data = d, ggplot2::aes(Class, M, colour = Class), size = 2.6, show.legend = FALSE) + ggplot2::scale_colour_manual(values = cols)
    }
  })

  # ---- cross-tabs ----
  xt <- reactive({
    v <- inp("xt_var"); req(v)
    a <- A(); ok <- classified(); g <- droplevels(cls()[ok]); x <- a$data[[v]][ok]; x <- factor(x)
    keep <- !is.na(x); g <- g[keep]; x <- droplevels(x[keep]); req(nlevels(x) >= 2, nlevels(g) >= 2)
    tab <- table(g, x); ch <- suppressWarnings(stats::chisq.test(tab))
    n <- sum(tab); V <- sqrt(unname(ch$statistic) / (n * (min(dim(tab)) - 1)))
    list(tab = tab, ch = ch, V = V, n = n, v = v, g = g, x = x)
  })
  output[[id("xt_out")]] <- renderUI({
    s <- xt(); tab <- s$tab; within_cls <- (inp("xt_base") %||% "") == paste("Within each", unit)
    pc <- if (within_cls) prop.table(tab, 1) * 100 else prop.table(tab, 2) * 100
    cells <- matrix(paste0(tab, " (", fu_fmt(as.numeric(pc), 1), "%)"), nrow = nrow(tab), dimnames = dimnames(tab))
    df <- data.frame(rownames(tab), cells, check.names = FALSE, stringsAsFactors = FALSE); names(df) <- c(Unit, colnames(tab))
    tn <- NULL
    res <- s$ch$stdres
    if (isTRUE(inp("xt_res"))) tn <- c(list(NULL), lapply(seq_len(ncol(tab)), function(j) ifelse(res[, j] >= 1.96, "good", ifelse(res[, j] <= -1.96, "bad", NA))))
    stat <- paste0("χ²(", s$ch$parameter, ", N = ", s$n, ") = ", fu_fmt(unname(s$ch$statistic), 2), ", p ", ifelse(s$ch$p.value < .001, "< .001", paste0("= ", sub("^0", "", fu_fmt(s$ch$p.value, 3)))), "; Cramér's V = ", sub("^0", "", fu_fmt(s$V, 2)))
    tagList(
      tags$div(class = "pl-chips", fu_chip("Test", HTML(stat))),
      tags$h4(class = "pl-flat-title", paste0(s$v, " by ", unit)), fu_table(df, tones = tn),
      fu_note(if (isTRUE(inp("xt_res"))) "Green: more respondents than expected (standardized residual ≥ 1.96); red: fewer (≤ −1.96). " else "", "Counts, with percentages ", if (within_cls) paste("within each", unit) else "within each category", ". Cramér's V of about .10, .30 and .50 is small, medium and large."),
      tags$h4(class = "pl-flat-title", "Plot"), plotOutput(id("xt_plot"), height = "280px")
    )
  })
  output[[id("xt_plot")]] <- renderPlot({
    s <- xt(); within_cls <- (inp("xt_base") %||% "") == paste("Within each", unit)
    d <- as.data.frame(if (within_cls) prop.table(s$tab, 1) * 100 else prop.table(s$tab, 2) * 100); names(d) <- c("cls", "x", "pct")
    if (within_cls) ggplot2::ggplot(d, ggplot2::aes(cls, pct, fill = x)) + geom_col(width = 0.6, colour = "white", linewidth = 0.5) + ggplot2::scale_fill_manual(values = pal(nlevels(s$x)), name = s$v) + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = paste("% within each", unit)) + theme_pl(11) + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    else ggplot2::ggplot(d, ggplot2::aes(x, pct, fill = cls)) + geom_col(position = "dodge", width = 0.65, colour = "white", linewidth = 0.4) + ggplot2::scale_fill_manual(values = pal(nlevels(s$g)), name = NULL) + ggplot2::labs(x = s$v, y = "% within each category") + theme_pl(11)
  })

  # ---- predictors (multinomial logistic regression) ----
  output[[id("pr_ref")]] <- renderUI({
    selectInput(id("pr_refsel"), paste("Reference", unit), choices = levels(cls()), selected = isolate(inp("pr_refsel")))
  })
  pr_fit <- eventReactive(inp("pr_run"), {
    vars <- inp("pr_vars"); req(length(vars) > 0)
    a <- A(); ok <- classified(); d <- a$data[ok, vars, drop = FALSE]; g <- droplevels(cls()[ok])
    ref <- inp("pr_refsel") %||% levels(g)[1]; g <- stats::relevel(g, ref = ref)
    for (v in names(d)) if (is.character(d[[v]])) d[[v]] <- factor(d[[v]])
    dat <- cbind(.cls = g, d); dat <- dat[stats::complete.cases(dat), , drop = FALSE]
    fml <- stats::as.formula(paste(".cls ~", paste(sprintf("`%s`", vars), collapse = " + ")))
    m <- nnet::multinom(fml, data = dat, trace = FALSE, maxit = 300)
    m0 <- nnet::multinom(.cls ~ 1, data = dat, trace = FALSE)
    list(m = m, m0 = m0, n = nrow(dat), ref = ref, dat = dat)
  })
  output[[id("pr_out")]] <- renderUI({
    if (is.null(inp("pr_run")) || inp("pr_run") < 1) return(fu_note("Choose the predictors and press Run regression."))
    f <- tryCatch(pr_fit(), error = function(e) e)
    if (inherits(f, "error")) return(fu_note("The regression could not be fitted: ", conditionMessage(f)))
    m <- f$m; sm <- summary(m); co <- sm$coefficients; se <- sm$standard.errors
    if (is.null(dim(co))) { co <- matrix(co, nrow = 1, dimnames = list(m$lev[2], names(co))); se <- matrix(se, nrow = 1, dimnames = dimnames(co)) }
    z <- co / se; pv <- 2 * (1 - stats::pnorm(abs(z)))
    rows <- list()
    for (r in rownames(co)) for (cn in setdiff(colnames(co), "(Intercept)")) {
      b <- co[r, cn]; s_ <- se[r, cn]; rows[[length(rows) + 1]] <- data.frame(cls = r, term = cn, OR = exp(b), lo = exp(b - 1.96 * s_), hi = exp(b + 1.96 * s_), p = pv[r, cn], stringsAsFactors = FALSE)
    }
    R <- do.call(rbind, rows)
    df <- data.frame(R$cls, R$term, fu_fmt(R$OR, 2), paste0(fu_fmt(R$lo, 2), " – ", fu_fmt(R$hi, 2)), fu_p(R$p), check.names = FALSE, stringsAsFactors = FALSE)
    names(df) <- c(paste(Unit, "(vs", f$ref, ")"), "Predictor", "OR", "95% CI", "p")
    lr <- 2 * (as.numeric(stats::logLik(m)) - as.numeric(stats::logLik(f$m0))); dfl <- length(stats::coef(m)) - length(stats::coef(f$m0))
    mcf <- 1 - as.numeric(stats::logLik(m)) / as.numeric(stats::logLik(f$m0))
    tagList(
      tags$div(class = "pl-chips", fu_chip("N", f$n), fu_chip("LR χ²", paste0(fu_fmt(lr, 2), " (df = ", dfl, ", p ", ifelse(stats::pchisq(lr, dfl, lower.tail = FALSE) < .001, "< .001", paste0("= ", sub("^0", "", fu_fmt(stats::pchisq(lr, dfl, lower.tail = FALSE), 3)))), ")")),
               fu_chip("McFadden R²", sub("^0", "", fu_fmt(mcf, 3))), fu_chip("AIC", fu_fmt(stats::AIC(m), 1))),
      tags$h4(class = "pl-flat-title", "Odds ratios"), fu_table(df, tones = list(NULL, NULL, NULL, NULL, ifelse(R$p < .05, "good", NA)), align_left = 1:2),
      fu_note("An odds ratio above 1 means that a higher value of the predictor goes with a higher chance of being in this ", unit, " rather than in the reference ", unit, ". Categorical predictors show each level against its reference level."),
      tags$h4(class = "pl-flat-title", "Forest plot"), plotOutput(id("pr_plot"), height = "300px")
    )
  })
  output[[id("pr_plot")]] <- renderPlot({
    f <- pr_fit(); m <- f$m; sm <- summary(m); co <- sm$coefficients; se <- sm$standard.errors
    if (is.null(dim(co))) { co <- matrix(co, nrow = 1, dimnames = list(m$lev[2], names(co))); se <- matrix(se, nrow = 1, dimnames = dimnames(co)) }
    rows <- list()
    for (r in rownames(co)) for (cn in setdiff(colnames(co), "(Intercept)")) rows[[length(rows) + 1]] <- data.frame(cls = r, term = cn, OR = exp(co[r, cn]), lo = exp(co[r, cn] - 1.96 * se[r, cn]), hi = exp(co[r, cn] + 1.96 * se[r, cn]))
    R <- do.call(rbind, rows); R$term <- factor(R$term, levels = rev(unique(R$term)))
    ggplot2::ggplot(R, ggplot2::aes(OR, term, colour = cls, shape = cls)) +
      ggplot2::geom_vline(xintercept = 1, linetype = "22", colour = "#9aa3b2") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), orientation = "y", width = 0.18, linewidth = 0.5, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.5)) +
      ggplot2::scale_x_log10() + ggplot2::scale_colour_manual(values = pal(length(unique(R$cls))), name = NULL) + ggplot2::scale_shape_manual(values = c(16, 15, 17, 18, 4, 3)[seq_len(length(unique(R$cls)))], name = NULL) +
      ggplot2::labs(x = "Odds ratio (log scale)", y = NULL) + theme_pl(11)
  })

  # ---- data & export ----
  out_df <- reactive({
    a <- A(); d <- a$data; P <- a$post; k <- ncol(P)
    lv <- levels(cls()); pp <- as.data.frame(round(P, 3)); names(pp) <- paste0("P_", lv)
    pp <- pp[, !(names(pp) %in% names(d)), drop = FALSE]
    d <- d[, !grepl("^(Posterior_Prob_|CPROB)", names(d)), drop = FALSE]
    cbind(d, pp)
  })
  output[[id("data")]] <- DT::renderDT({
    DT::datatable(out_df(), extensions = "Buttons", rownames = FALSE, options = list(dom = "Bfrtip", scrollX = TRUE, pageLength = 12, buttons = list(list(extend = "excel", text = "Export Excel", filename = paste0("Data with ", unit)))))
  }, server = FALSE)
  output[[id("dl_csv")]] <- downloadHandler(
    filename = function() paste0("Data_with_", unit, "_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(out_df(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
}
