# plot_helpers.R
# renderPlot() for the whole app: plots follow the decimal separator chosen at the top right.
# The plot is drawn with options(OutDec = ",") when the comma is chosen (axis labels, legends and
# formatted values follow), and it redraws as soon as the choice changes.
# formatRound() that leaves a table alone when it has no numeric column (data of categorical
# variables only): DT::formatRound() fails on an empty column list.
formatRound <- function(table, columns = NULL, digits = 2, interval = 3, mark = ",", dec.mark = getOption("OutDec"), zero.print = NULL) {
  if (length(columns) == 0) return(table)
  DT::formatRound(table, columns = columns, digits = digits, interval = interval, mark = mark, dec.mark = dec.mark, zero.print = zero.print)
}

# ---- black and white figures ----------------------------------------------------------------
# Lines keep one colour (black) and differ by line type and marker (the plot builders map both);
# bars and areas get black outlines and hatching instead of colour.
pl_hatch <- function(kind, tile = 2.6) {
  g <- switch(kind,
    diag  = grid::segmentsGrob(0, 0, 1, 1, gp = grid::gpar(lwd = 0.9)),
    anti  = grid::segmentsGrob(0, 1, 1, 0, gp = grid::gpar(lwd = 0.9)),
    horiz = grid::segmentsGrob(0, .5, 1, .5, gp = grid::gpar(lwd = 0.9)),
    vert  = grid::segmentsGrob(.5, 0, .5, 1, gp = grid::gpar(lwd = 0.9)),
    cross = grid::gTree(children = grid::gList(grid::segmentsGrob(0, 0, 1, 1, gp = grid::gpar(lwd = 0.8)), grid::segmentsGrob(0, 1, 1, 0, gp = grid::gpar(lwd = 0.8)))),
    dots  = grid::pointsGrob(.5, .5, pch = 16, size = grid::unit(0.9, "mm")),
    NULL)
  grid::pattern(grid::gTree(children = grid::gList(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA)), g)),
                width = grid::unit(tile, "mm"), height = grid::unit(tile, "mm"), extend = "repeat")
}
pl_hatch_set <- function(n) {
  kinds <- c("diag", "dots", "horiz", "cross", "anti", "vert", "plain")
  lapply(rep_len(kinds, max(n, 1)), pl_hatch)
}
pl_grey_set <- function(n) grDevices::gray.colors(max(n, 1), start = 0.2, end = 0.92)

pl_mono <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  b <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  if (is.null(b)) return(p)
  suppressMessages({
    sc <- b$plot$scales
    col <- sc$get_scales("colour"); fil <- sc$get_scales("fill")
    if (!is.null(col) && !is.null(col$is_discrete()) && col$is_discrete()) {
      lim <- col$get_limits()
      p <- p + ggplot2::scale_colour_manual(values = stats::setNames(rep("black", length(lim)), lim), name = col$name, labels = col$get_labels(lim))
    }
    if (!is.null(fil) && fil$is_discrete()) {
      lim <- fil$get_limits()
      is_bar <- any(vapply(p$layers, function(l) inherits(l$geom, c("GeomBar", "GeomCol", "GeomRect", "GeomTile", "GeomBarInteractive", "GeomArea", "GeomPolygon")), logical(1)))
      if (is_bar) {
        p <- p + ggplot2::scale_fill_manual(values = stats::setNames(pl_hatch_set(length(lim)), lim), name = fil$name, labels = fil$get_labels(lim))
        for (i in seq_along(p$layers)) if (inherits(p$layers[[i]]$geom, c("GeomBar", "GeomCol", "GeomRect", "GeomArea", "GeomPolygon"))) p$layers[[i]]$aes_params$colour <- "black"
      } else {
        p <- p + ggplot2::scale_fill_manual(values = stats::setNames(rep("white", length(lim)), lim), name = fil$name, labels = fil$get_labels(lim), guide = "none")
      }
    }
  })
  p
}

# renderPlot with the colour mode and decimal separator applied (see above); redefined here so
# black and white applies to every plot the app draws
renderPlot <- function(expr, width = "auto", height = "auto", res = 72, ..., alt = NA,
                       env = parent.frame(), quoted = FALSE, execOnResize = FALSE, outputArgs = list()) {
  func <- shiny::installExprFunction(expr, "func", env, quoted, label = "renderPlot")
  shiny::renderPlot({
    dom <- shiny::getDefaultReactiveDomain()
    dec <- if (!is.null(dom) && !is.null(dom$input)) dom$input$dec_sep else NULL
    fig <- if (!is.null(dom) && !is.null(dom$input)) dom$input$fig_mode else NULL
    old <- options(OutDec = if (identical(dec, ",")) "," else ".", pl.mono = identical(fig, "mono"))
    on.exit(options(old), add = TRUE)
    r <- withVisible(func())
    if (isTRUE(r$visible) && !is.null(r$value)) print(if (identical(fig, "mono")) pl_mono(r$value) else r$value)
    invisible(NULL)
  }, width = width, height = height, res = res, ..., alt = alt, execOnResize = execOnResize, outputArgs = outputArgs)
}
