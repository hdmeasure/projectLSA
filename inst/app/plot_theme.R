# plot_theme.R
# Shared look for the model-comparison plots built by the app (AIC/BIC, entropy, class size).
# Plots drawn by other packages (lavaan, mirt, ...) keep their own style.

pl_palette <- c("#5748d9", "#14b8a6", "#f59e0b", "#ec4899", "#0ea5e9", "#84cc16", "#f97316", "#64748b")

theme_pl <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "") +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", color = "#1e1b4b", size = base_size + 3),
      plot.subtitle = ggplot2::element_text(color = "#64748b", size = base_size - 1,
                                            margin = ggplot2::margin(b = 10)),
      axis.title = ggplot2::element_text(color = "#475569", size = base_size - 1),
      axis.text = ggplot2::element_text(color = "#475569"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#e7e5f4", linewidth = 0.4),
      axis.line.x = ggplot2::element_line(color = "#cbd5e1"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", color = "#475569"),
      legend.text = ggplot2::element_text(color = "#475569"),
      plot.margin = ggplot2::margin(12, 16, 8, 12),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

# One colour family per measure, so BIC, AIC, entropy and class size are told apart at a glance
pl_palettes <- list(
  brand  = c("#5748d9", "#14b8a6", "#f59e0b", "#ec4899", "#0ea5e9", "#84cc16", "#f97316", "#64748b"),
  purple = c("#3b2fa6", "#6a5ce0", "#9488ee", "#4a3cc7", "#b8b0f4", "#7f72e8", "#2a2180", "#a79ef0"),
  teal   = c("#0b5f58", "#14a094", "#5fcfc3", "#0e7c74", "#9be2da", "#2bb8aa", "#074a45", "#7bd8cd"),
  amber  = c("#8a5a0b", "#d98e14", "#f2bb54", "#b9791a", "#f8d68c", "#e5a02e", "#6b4306", "#f5c970"),
  rose   = c("#8f2a52", "#d6457f", "#ee8db3", "#b4386a", "#f6bcd2", "#e2639a", "#6e1f3e", "#f1a3c4")
)

# Line + point comparison across the number of classes/profiles. Compact: small points, thin lines.
# `best` = "min" or "max" marks the preferred value of each series with a ring (off by default).
pl_compare_plot <- function(df, x, y, group = NULL, title, xlab, ylab, best = c("none", "min", "max"), digits = 2, legend = NULL,
                            palette = "brand", base_size = 11) {
  best <- match.arg(best)
  d <- as.data.frame(df)
  d$.x <- factor(d[[x]])
  d$.y <- d[[y]]
  d$.g <- if (is.null(group)) "all" else factor(d[[group]])
  d$.lab <- formatC(d$.y, format = "f", digits = digits, big.mark = ",")
  d$.best <- FALSE
  if (best != "none") {
    for (g in unique(d$.g)) {
      i <- which(d$.g == g & is.finite(d$.y))
      if (length(i)) d$.best[i[if (best == "min") which.min(d$.y[i]) else which.max(d$.y[i])]] <- TRUE
    }
  }
  ng <- length(unique(d$.g))
  cols <- rep_len(if (is.null(pl_palettes[[palette]])) pl_palettes$brand else pl_palettes[[palette]], max(ng, 1))
  shapes <- rep_len(c(21, 22, 24, 23, 25, 21, 22, 24), max(ng, 1))
  ltys <- rep_len(c("solid", "22", "42", "solid", "22", "42", "solid", "22"), max(ng, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(.x, .y, color = .g, fill = .g, shape = .g, linetype = .g, group = .g)) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.9) +
    ggplot2::geom_point(size = 1.9, stroke = 0.7, fill = "white")
  if (best != "none") {
    p <- p + ggplot2::geom_point(data = d[d$.best, ], size = 3, stroke = 0.8, show.legend = FALSE)
  }
  p <- p +
    ggplot2::geom_text(ggplot2::aes(label = formatC(.y, format = "f", digits = digits, big.mark = if (identical(getOption("OutDec"), ",")) "." else ",")), vjust = -1.1, size = 2.6, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = cols, name = legend) +
    ggplot2::scale_fill_manual(values = cols, name = legend) +
    ggplot2::scale_shape_manual(values = shapes, name = legend) +
    ggplot2::scale_linetype_manual(values = ltys, name = legend) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.14))) +
    ggplot2::labs(title = title, subtitle = NULL, x = xlab, y = ylab) +
    theme_pl(base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size + 1, face = "bold", color = "#1e1b4b"),
                   plot.margin = ggplot2::margin(6, 10, 4, 8), legend.key.size = grid::unit(0.8, "lines"),
                   legend.margin = ggplot2::margin(0, 0, 0, 0), legend.box.margin = ggplot2::margin(-4, 0, 0, 0))
  if (ng <= 1) p <- p + ggplot2::theme(legend.position = "none")
  p
}

# Colours for the classes of an LCA plot: a named palette or the colours picked by the user
pl_lca_palette <- function(name, n, custom = NULL) {
  base <- switch(name,
    okabe = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#F0E442", "#000000"),
    pastel = c("#8fa8e8", "#f2a6a0", "#8fd3c0", "#f4cf86", "#c3a6e8", "#a8d58f", "#f0a6cf", "#b5b5b5"),
    warm = c("#b23a48", "#e07a30", "#e8b230", "#8f5b3a", "#d9547a", "#f2956a", "#7a2d3b", "#c9a227"),
    cool = c("#1f5f9e", "#2aa198", "#6a5acd", "#3b8ea5", "#5fb49c", "#4b4f9a", "#7fb7d9", "#2d6a4f"),
    grey = c("#1f2937", "#6b7280", "#9ca3af", "#374151", "#d1d5db", "#4b5563", "#111827", "#e5e7eb"),
    custom = if (length(custom) >= 1) custom else pl_palettes$brand,
    pl_palettes$brand)
  rep_len(base, max(n, 1))
}

# ---- rounded bars ---------------------------------------------------------------------------
# geom_col() with rounded corners, to match the rest of the interface. It replaces ggplot2's
# geom_col() everywhere in the app (this file is sourced into the global environment). In polar
# coordinates, or when it cannot draw rounded corners, it falls back to ordinary rectangles.
GeomRoundCol <- ggplot2::ggproto("GeomRoundCol", ggplot2::GeomBar,
  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre", width = NULL, flipped_aes = FALSE) {
    if (!inherits(coord, "CoordCartesian") && !inherits(coord, "CoordFlip"))
      return(ggplot2::ggproto_parent(ggplot2::GeomBar, self)$draw_panel(data, panel_params, coord, lineend = lineend, linejoin = linejoin, width = width, flipped_aes = flipped_aes))
    co <- coord$transform(data, panel_params)
    if (!all(c("xmin", "xmax", "ymin", "ymax") %in% names(co))) return(grid::nullGrob())
    x0 <- pmin(co$xmin, co$xmax); x1 <- pmax(co$xmin, co$xmax); y0 <- pmin(co$ymin, co$ymax); y1 <- pmax(co$ymin, co$ymax)
    fill <- ggplot2::fill_alpha(co$fill, co$alpha)
    kids <- lapply(seq_len(nrow(co)), function(i) grid::roundrectGrob(
      x = x0[i], y = y1[i], width = x1[i] - x0[i], height = y1[i] - y0[i], default.units = "native", just = c("left", "top"),
      r = grid::unit(3.5, "pt"),
      gp = grid::gpar(col = co$colour[i], fill = fill[[i]], lwd = co$linewidth[i] * ggplot2::.pt, lty = co$linetype[i], linejoin = linejoin, lineend = lineend)))
    do.call(grid::gTree, list(children = do.call(grid::gList, kids)))
  }
)
geom_col <- function(mapping = NULL, data = NULL, position = "stack", ..., just = 0.5, lineend = "butt", linejoin = "mitre",
                     na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = ggplot2::StatIdentity, geom = GeomRoundCol, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = rlang::list2(just = just, lineend = lineend, linejoin = linejoin, na.rm = na.rm, ...))
}

# ---- donut: a full ring with rounded ends and an icon in the middle ---------------------------------
# Every slice is a filled polygon (so black and white mode can hatch it), with half-round ends.
# freq: counts per category (in order); cols: one fill per category; labels: legend text
# icon: "bell" draws a small normal curve (suits a score); "none" leaves the hole empty
pl_donut <- function(freq, cols, labels, title = NULL, center = NULL, center_sub = NULL, text_cols = NULL, icon = "bell") {
  mono <- isTRUE(getOption("pl.mono"))
  n <- length(freq); share <- freq / sum(freq)
  rmid <- 0.77; thick <- 0.42; rcap <- thick / 2
  shrink <- rcap / rmid + 0.03           # angular room taken by one half-round end plus a hairline gap
  ang <- cumsum(c(0, share)) * 2 * pi
  arc <- function(r, a0, a1, k = 40) { t <- seq(a0, a1, length.out = k); cbind(r * sin(t), r * cos(t)) }
  # a half circle closing one end of a slice: R is the outward direction, T the direction of travel
  cap <- function(a, forward) {
    Rv <- c(sin(a), cos(a)); Tv <- c(cos(a), -sin(a)); C <- rmid * Rv
    phi <- seq(0, pi, length.out = 24)
    if (forward) cbind(C[1] + rcap * (cos(phi) * Rv[1] + sin(phi) * Tv[1]), C[2] + rcap * (cos(phi) * Rv[2] + sin(phi) * Tv[2]))
    else cbind(C[1] + rcap * (-cos(phi) * Rv[1] - sin(phi) * Tv[1]), C[2] + rcap * (-cos(phi) * Rv[2] - sin(phi) * Tv[2]))
  }
  polys <- lapply(seq_len(n), function(i) {
    a0 <- ang[i] + shrink; a1 <- ang[i + 1] - shrink
    if (a1 <= a0 + 0.02) { m <- (ang[i] + ang[i + 1]) / 2; a0 <- m - 0.01; a1 <- m + 0.01 }
    outer <- arc(rmid + rcap, a0, a1)
    end1 <- cap(a1, TRUE)
    inner <- arc(rmid - rcap, a1, a0)
    end0 <- cap(a0, FALSE)
    m <- rbind(outer, end1, inner, end0)
    data.frame(x = m[, 1], y = m[, 2], cat = factor(labels[i], levels = labels), id = i)
  })
  df <- do.call(rbind, polys)
  mid <- (ang[-1] + ang[-(n + 1)]) / 2
  lab <- data.frame(x = rmid * sin(mid), y = rmid * cos(mid), txt = sprintf("%d%%", round(share * 100)), cat = factor(labels, levels = labels))
  ink <- if (mono) "#000000" else "#3b2fa6"
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = df, ggplot2::aes(x, y, fill = cat, group = id), colour = if (mono) "black" else NA, linewidth = 0.4) +
    ggplot2::geom_label(data = lab, ggplot2::aes(x, y, label = txt), size = 3.3, fontface = "bold", text.colour = "#172033", border.colour = NA, fill = "white", linewidth = 0, label.padding = grid::unit(0.18, "lines"), label.r = grid::unit(0.3, "lines")) +
    ggplot2::scale_fill_manual(values = stats::setNames(cols, labels), name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(colour = if (mono) "black" else NA))) +
    ggplot2::coord_equal(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3), clip = "off") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::labs(title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", colour = "#172033", size = 13, margin = ggplot2::margin(b = 4)),
                   legend.position = "bottom", legend.direction = "vertical", legend.text = ggplot2::element_text(colour = "#2d3648", size = 10),
                   legend.key.size = grid::unit(1, "lines"), plot.background = ggplot2::element_rect(fill = "white", colour = NA))
  if (identical(icon, "bell")) {
    x <- seq(-0.26, 0.26, length.out = 80); y <- 0.2 * exp(-x^2 / (2 * 0.085^2)); y0 <- 0.02
    poly <- data.frame(x = c(x, rev(x)), y = c(y0 + y, rep(y0, length(x))))
    p <- p +
      ggplot2::geom_polygon(data = poly, ggplot2::aes(x, y), fill = if (mono) "#e6e6e6" else "#e7e3fb", colour = NA) +
      ggplot2::geom_path(data = data.frame(x = x, y = y0 + y), ggplot2::aes(x, y), colour = ink, linewidth = 0.8, lineend = "round") +
      ggplot2::geom_segment(ggplot2::aes(x = -0.3, xend = 0.3, y = y0, yend = y0), colour = ink, linewidth = 0.6, lineend = "round")
  }
  if (!is.null(center)) p <- p + ggplot2::annotate("text", x = 0, y = if (identical(icon, "bell")) -0.12 else 0.06, label = center, size = 5, fontface = "bold", colour = ink)
  if (!is.null(center_sub)) p <- p + ggplot2::annotate("text", x = 0, y = if (identical(icon, "bell")) -0.27 else -0.18, label = center_sub, size = 2.6, colour = "#4f5a6e")
  p
}
