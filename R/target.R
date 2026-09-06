#' Create a target diagram
#'
#' Creates a target diagram comparing quantitative prediction models with an
#' observation vector. The signed horizontal coordinate combines standardized
#' unbiased RMSD with the direction of the model standard-deviation difference;
#' the vertical coordinate is normalized mean error. Concentric circles show
#' RMSD thresholds associated with correlation levels.
#'
#' The default geometry and continuous `viridis` colour scale follow Wadoux,
#' Walvoort, and Brus (2022) and the original implementation in the
#' accompanying repository.
#'
#' @param mods A numeric prediction vector, or a named list of numeric
#'   prediction vectors.
#' @param obs A numeric observation vector.
#' @param colorval Optional numeric values used to colour model points. If
#'   `NULL`, Pearson correlation is used.
#' @param colorval.name Optional colour-legend title.
#' @param axis_begin Lower limit for both manually drawn axes.
#' @param axis_end Upper limit for both manually drawn axes.
#' @param by Spacing between manually drawn axis ticks.
#' @param label Logical; draw model labels with `ggrepel`?
#' @param point_size Numeric point size.
#' @param label_size Numeric text size for model labels.
#'
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
#' gg_target(mods, obs, label = TRUE, axis_begin = -2,
#'           axis_end = 2, by = 0.5)
#' @export
gg_target <- function(mods, obs, colorval = NULL, colorval.name = NULL,
                      axis_begin = -2, axis_end = 2, by = 0.1,
                      label = FALSE, point_size = 7, label_size = 4) {
  validate_metrics_inputs(mods, obs, TRUE, NULL)
  mods <- as_model_list(mods)
  validate_diagram_arguments(colorval, colorval.name, axis_begin, axis_end,
                             axis_end, by, label, length(mods))

  model_stats <- lapply(mods, function(pred) {
    keep <- stats::complete.cases(pred, obs)
    pred <- pred[keep]
    obs_use <- obs[keep]
    sigma_ast <- stats::sd(pred) / stats::sd(obs_use)
    r <- suppressWarnings(stats::cor(pred, obs_use))
    if (is.na(r)) r <- 0
    sigma_difference <- sign(stats::sd(pred) - stats::sd(obs_use))
    if (sigma_difference == 0) sigma_difference <- 1
    data.frame(
      nME = (mean(obs_use) - mean(pred)) / stats::sd(obs_use),
      uRMSDnorm_sigmaD = sqrt(1 + sigma_ast^2 - 2 * sigma_ast * r) * sigma_difference,
      r = r
    )
  })
  data <- do.call(rbind, model_stats)
  data$model <- names(mods)
  if (is.null(colorval)) colorval <- data$r
  if (is.null(colorval.name)) colorval.name <- "Correlation"
  data$colvar <- colorval

  circle <- function(radius) {
    theta <- seq(0, 2 * pi, length.out = 101)
    data.frame(x = radius * sin(theta), y = radius * cos(theta), r = radius)
  }
  circle_data <- rbind(circle(0.44), circle(0.71))
  circle_reference <- circle(1)
  circle_labels <- data.frame(
    x = c(circle_data$x[circle_data$r == 0.44][37],
          circle_data$x[circle_data$r == 0.71][37],
          circle_reference$x[37]),
    y = c(circle_data$y[circle_data$r == 0.44][37],
          circle_data$y[circle_data$r == 0.71][37],
          circle_reference$y[37]),
    label = c(0.9, 0.7, 1)
  )

  ticks <- subset(data.frame(ticks = seq(axis_begin, axis_end, by = by), zero = 0), ticks != 0)
  labels <- subset(data.frame(label = c(axis_begin, axis_end), zero = 0), label != 0)
  tick_size <- (axis_end - axis_begin) / 128

  p <- ggplot2::ggplot(data, ggplot2::aes(x = uRMSDnorm_sigmaD, y = nME)) +
    ggplot2::geom_path(
      data = circle_data,
      ggplot2::aes(x = x, y = y, group = r),
      inherit.aes = FALSE, colour = "black", linetype = 2
    ) +
    ggplot2::geom_path(
      data = circle_reference,
      ggplot2::aes(x = x, y = y),
      inherit.aes = FALSE, colour = "black", linewidth = 0.8
    ) +
    ggplot2::geom_text(
      data = circle_labels,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE, size = 3.5, colour = "black"
    ) +
    ggplot2::geom_segment(
      x = 0, xend = 0, y = axis_begin, yend = axis_end, linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      x = axis_begin, xend = axis_end, y = 0, yend = 0, linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = ticks,
      ggplot2::aes(x = ticks, xend = ticks, y = zero, yend = zero + tick_size),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = ticks,
      ggplot2::aes(x = zero, xend = zero + tick_size, y = ticks, yend = ticks),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = labels, ggplot2::aes(x = label, y = zero, label = label),
      inherit.aes = FALSE, vjust = 1.5, size = 3.5
    ) +
    ggplot2::geom_text(
      data = labels, ggplot2::aes(x = zero, y = label, label = label),
      inherit.aes = FALSE, hjust = 1.5, size = 3.5
    ) +
    ggplot2::geom_point(ggplot2::aes(fill = colvar), shape = 21, size = point_size) +
    ggplot2::ylab(latex2exp::TeX("$SDE^* \\cdot \\sign(\\sigma_d)")) +
    ggplot2::xlab(latex2exp::TeX("ME^*")) +
    viridis::scale_fill_viridis(option = "A") +
    ggplot2::labs(fill = colorval.name) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
      axis.line.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 12, family = "Palatino"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = -17, r = 0, b = 0, l = 0)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -17, b = 0, l = 0)),
      legend.position = "right", legend.justification = "right",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(-10, -10, -10, -20),
      legend.text.align = 1, legend.title = ggplot2::element_text(vjust = 3)
    )

  if (isTRUE(label)) {
    p <- p + ggrepel::geom_label_repel(
      ggplot2::aes(label = model), box.padding = 0.35,
      point.padding = 0.5, segment.color = "grey50", size = label_size
    )
  }
  p
}
