#' Create a solar diagram
#'
#' Creates a solar diagram comparing quantitative prediction models with an
#' observation vector. The diagram plots normalized mean error against the
#' standardized unbiased root mean square difference and uses correlation
#' regions to provide an integrated view of model skill.
#'
#' The default geometry, pale-yellow correlation regions, and `viridis`
#' colour scale follow Wadoux, Walvoort, and Brus (2022) and the original
#' implementation in the accompanying repository.
#'
#' @inheritParams diagram_stats
#' @seealso [diagram_stats()], [model_metrics()], [gg_taylor()], [gg_target()]
#' @details Missing pairs are removed separately per model when `na.rm = TRUE`.
#'   Two complete pairs with non-zero observation SD are required. All plotted
#'   statistics can be retrieved with diagram_stats(). Sample SD normalization
#'   and the original constant-prediction convention are documented there.
#' @param colorval Optional finite numeric vector with one value per model,
#'   in model order (names do not reorder values). If NULL, correlation is used.
#' @param colorval.name Optional colour-legend title.
#' @param x.axis_begin Lower endpoint of the manually drawn horizontal axis.
#'   Axis arguments set reference-axis extents, not clipping limits.
#'   To zoom, add [ggplot2::coord_cartesian()].
#' @param x.axis_end Upper endpoint of the manually drawn horizontal axis.
#' @param y.axis_end Upper endpoint of the manually drawn vertical axis.
#' @param by Spacing between manually drawn axis ticks.
#' @param label Logical; draw model labels with `ggrepel`?
#' @param point_size Numeric point size.
#' @param label_size Numeric text size for model labels.
#' @param legend Logical; show the continuous point-colour legend and the
#'   correlation-region key? Defaults to `TRUE` to preserve the original plot.
#' @param reference Logical; draw the original correlation regions and their
#'   outer reference circle? Defaults to `TRUE`.
#'
#' @section Coordinates and labels:
#'   The horizontal coordinate is nME and the vertical coordinate is sde.
#'   Axis-title placement follows the original diagram, where titles are placed
#'   near axis ends rather than in ordinary Cartesian positions. Use labs()
#'   to override titles. The reference regions retain the original rounded radii;
#'   their interpretation requires the normalization assumptions in diagram_stats().
#' @return A `ggplot2` plot object that can be extended with ordinary ggplot2
#'   layers, scales, labels, and themes.
#'
#' @references
#' Wadoux, A. M. J.-C., Walvoort, D. J. J., and Brus, D. J. (2022). An
#' integrated approach for the evaluation of quantitative soil maps through
#' Taylor and solar diagrams. *Geoderma*, 405, 115332.
#' <https://doi.org/10.1016/j.geoderma.2021.115332>
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#' mods <- list(perfect = obs, biased = obs + 1)
#' gg_solar(mods, obs, label = TRUE, x.axis_begin = -1,
#'          x.axis_end = 1, y.axis_end = 2, by = 0.2)
#'
#' # Hide legends or the reference geometry when preparing a custom plot.
#' gg_solar(mods, obs, legend = FALSE, reference = FALSE)
#' @export
gg_solar <- function(mods, obs, colorval = NULL, colorval.name = NULL,
                     x.axis_begin = -1, x.axis_end = 1, y.axis_end = 2,
                     by = 0.1, label = FALSE, point_size = 7,
                     label_size = 4, na.rm = TRUE, legend = TRUE,
                     reference = TRUE) {
  validate_plot_sizes(label, point_size, label_size)
  check_flag(legend, "legend")
  check_flag(reference, "reference")
  data <- diagram_stats(mods, obs, na.rm = na.rm)
  validate_diagram_arguments(colorval, colorval.name, x.axis_begin, x.axis_end,
                             y.axis_end, by, label, nrow(data))
  if (is.null(colorval)) colorval <- data$r
  if (is.null(colorval.name)) colorval.name <- "Correlation"
  data$colvar <- colorval
  data$uRMSDnorm_sigmaD <- data$sde
  data$uRMSDnorm <- data$sde

  circle <- function(radius) {
    data.frame(
      x = radius * cos(seq(0, pi, length.out = 1001)),
      y = radius * sin(seq(0, pi, length.out = 1001)),
      label = radius
    )
  }
  circle2 <- circle(1)
  circle095 <- circle(0.31); circle095$lab <- "r>0.95"
  circle09 <- circle(0.44); circle09$lab <- "r>0.9"
  circle07 <- circle(0.71); circle07$lab <- "r>0.7"
  circle0 <- circle(1); circle0$lab <- "r>0"
  circ_pol <- rbind(circle0, circle07, circle09, circle095)

  x_ticks <- data.frame(ticks = seq(x.axis_begin, x.axis_end, by = by), zero = 0)
  y_ticks <- data.frame(ticks = seq(0, y.axis_end, by = by), zero = 0)
  x_labs <- data.frame(lab = c(0, x.axis_begin, x.axis_end), zero = 0)
  y_labs <- data.frame(lab = c(0, y.axis_end), zero = 0)
  tick_size <- (x.axis_end - x.axis_begin) / 128

  p <- ggplot2::ggplot(data, ggplot2::aes(x = nME, y = uRMSDnorm_sigmaD))

  # Preserve the original pale-yellow correlation regions by default.
  if (isTRUE(reference)) {
    p <- p +
      ggplot2::geom_polygon(
      data = circ_pol,
      ggplot2::aes(x = x, y = y, fill = lab),
      inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_manual(
      values = c("r>0" = "#FEECA4", "r>0.7" = "#FEF4B6",
                 "r>0.9" = "#FFFCD7", "r>0.95" = "#FFFFE5"),
      name = "",
      labels = c(expression(italic(r) > 0), expression(italic(r) > 0.7),
                 expression(italic(r) > 0.9), expression(italic(r) > 0.95))
      ) +
      ggplot2::geom_path(
      data = circle2,
      ggplot2::aes(x = x, y = y, group = label),
      inherit.aes = FALSE, colour = "black", linewidth = 0.8
      )
  }

  p <- p +
    ggplot2::annotate(
      "segment", x = 0, xend = 0, y = 0, yend = y.axis_end,
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "segment", x = x.axis_begin, xend = x.axis_end, y = 0, yend = 0,
      linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = x_ticks,
      ggplot2::aes(x = ticks, xend = ticks, y = zero, yend = zero + tick_size),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = y_ticks,
      ggplot2::aes(x = zero - tick_size, xend = zero + tick_size,
                   y = ticks, yend = ticks),
      inherit.aes = FALSE
    ) +
    ggplot2::xlab(expression(SDE^"*")) +
    ggplot2::scale_x_continuous(position = "top") +
    ggplot2::ylab(expression(ME^"*")) +
    ggplot2::theme_classic() +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme(
      plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 12),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.length.x = grid::unit(-0.2, "cm"),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8), hjust = 0.02),
      legend.position = "right", legend.justification = "center",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 8),
      legend.text = ggplot2::element_text(hjust = 0), legend.title = ggplot2::element_text(vjust = 3)
    ) +
    ggplot2::geom_text(
      data = x_labs, ggplot2::aes(x = lab, y = zero, label = lab),
      inherit.aes = FALSE, vjust = 1.5, size = 3.5
    ) +
    ggplot2::geom_text(
      data = y_labs[-1, , drop = FALSE],
      ggplot2::aes(x = zero, y = lab, label = lab),
      inherit.aes = FALSE, hjust = 1.5, size = 3.5
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = colvar), shape = 19, size = point_size) +
    ggplot2::geom_point(ggplot2::aes(colour = colvar), colour = "black", shape = 21, size = point_size) +
    ggplot2::labs(colour = colorval.name) +
    viridis::scale_color_viridis(option = "A", na.value = "grey50") +
    ggplot2::guides(colour = ggplot2::guide_colourbar(order = 1))

  if (!isTRUE(legend)) {
    p <- p + ggplot2::guides(colour = "none", fill = "none")
  }

  if (isTRUE(label)) {
    p <- p + ggrepel::geom_label_repel(
      ggplot2::aes(label = model), box.padding = 0.35,
      point.padding = 0.5, segment.color = "grey50", size = label_size, seed = 0
    )
  }
  p
}
