#' Create a Taylor diagram
#'
#' Creates a Taylor diagram comparing one or more quantitative prediction
#' models with an observation vector. The radial coordinate is the model
#' standard deviation divided by the observation standard deviation and the
#' polar angle represents the Pearson correlation. The red dashed contours
#' show centred root-mean-square distance from the reference point.
#'
#' The geometry and default styling follow Wadoux, Walvoort, and Brus (2022)
#' and the original implementation in the accompanying repository.
#'
#' @inheritParams diagram_stats
#' @seealso [diagram_stats()], [model_metrics()], [gg_taylor()], [gg_solar()],
#'   [gg_target()]
#' @details Missing pairs are removed separately per model when na.rm = TRUE.
#'   Two complete pairs with non-zero observation SD are required. All plotted
#'   statistics can be retrieved with diagram_stats(). Sample SD normalization
#'   and the original constant-prediction convention are documented there.
#' @param label Logical; draw model labels with `ggrepel`?
#' @param point_size Numeric size of model points.
#' @param label_size Numeric text size for model labels.
#'
#' @return A `ggplot2` plot object. It can be extended with ordinary ggplot2
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
#' mods <- list(perfect = obs, biased = obs + 1, smooth = c(2, 2, 3, 4, 4))
#' gg_taylor(mods, obs, label = TRUE)
#' @export
gg_taylor <- function(mods, obs, label = FALSE, point_size = 6,
                      label_size = 4, na.rm = TRUE) {
  validate_plot_sizes(label, point_size, label_size)
  statistics <- diagram_stats(mods, obs, na.rm = na.rm)
  model_points <- data.frame(
    Cor = statistics$r, Std = statistics$sd_ratio, Model = statistics$model)
  model_points$x <- model_points$Std * model_points$Cor
  model_points$y <- model_points$Std *
    sqrt(pmax(0, (1 - model_points$Cor) * (1 + model_points$Cor)))

  obs_std <- 1
  std_max <- ceiling(max(c(obs_std, model_points$Std, 2), na.rm = TRUE))
  if (std_max > .Machine$double.xmax / 6) {
    stop("Standard deviation ratio is too large to draw Taylor contours.", call. = FALSE)
  }
  std_major <- seq(0, std_max, length.out = 5)
  semicircle <- do.call(rbind, lapply(std_major, function(radius) {
    data.frame(
      x = radius * cos(seq(0, pi, length.out = 1001)),
      y = radius * sin(seq(0, pi, length.out = 1001)),
      label = radius
    )
  }))

  cor_major <- c(seq(-1, 1, 0.1), -0.95, 0.95, -0.99, 0.99)
  rays <- do.call(rbind, lapply(cor_major, function(correlation) {
    data.frame(
      xend = max(std_major) * cos(acos(correlation)),
      yend = max(std_major) * sin(acos(correlation)),
      label = correlation
    )
  }))

  # Circles centred on the reference point (1, 0), clipped to the diagram.
  max_radius <- 1.5 * std_max
  distances <- seq(180, 0, by = -1)
  circle_data <- do.call(rbind, lapply(seq(2 * max_radius / 12, 2 * max_radius,
                                            by = max_radius / 12), function(radius) {
    data.frame(
      xcircle = 1 + cos(distances * pi / 180) * radius,
      ycircle = sin(distances * pi / 180) * radius,
      labelc = radius
    )
  }))
  boundary <- semicircle[semicircle$label == max(semicircle$label), ]
  boundary_fun <- stats::approxfun(boundary$x, boundary$y)
  outside <- circle_data$ycircle > boundary_fun(circle_data$xcircle) |
    is.na(boundary_fun(circle_data$xcircle))
  circle_data[outside, c("xcircle", "ycircle")] <- NA_real_
  circle_labels <- do.call(rbind, lapply(unique(circle_data$labelc), function(radius) {
    subset <- circle_data[circle_data$labelc == radius, ]
      subset[10, ]
  }))
  circle_labels <- circle_labels[stats::complete.cases(circle_labels[, c("xcircle", "ycircle")]), ]

  p <- ggplot2::ggplot(model_points) +
    ggplot2::coord_equal() +
    ggplot2::geom_line(
      data = semicircle,
      ggplot2::aes(x = x, y = y, group = label),
      linetype = "solid", colour = "black", linewidth = 0.6
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::scale_x_continuous(labels = abs) +
    ggplot2::geom_segment(
      data = rays,
      ggplot2::aes(x = 0, y = 0, xend = xend, yend = yend),
      linetype = "dashed", colour = "black"
    ) +
    ggplot2::annotate(
      "segment", x = min(rays$xend), y = 0,
      xend = max(rays$xend), yend = 0, colour = "black"
    ) +
    ggplot2::geom_line(
      data = circle_data,
      ggplot2::aes(x = xcircle, y = ycircle, group = labelc),
      linetype = "dashed", colour = "red3", linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::geom_label(
      data = circle_labels,
      ggplot2::aes(x = xcircle, y = ycircle, label = labelc),
      linewidth = 0, fill = "white", vjust = 0, size = 4, colour = "red3",
      family = "sans",
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = rays,
      ggplot2::aes(x = 1.07 * xend, y = 1.035 * yend, label = label),
      vjust = 0, size = 4, colour = "black", family = "sans"
    ) +
    theme_taylor() +
    ggplot2::xlab(latex2exp::TeX("Standardized standard deviation $\\sigma^*$")) +
    ggplot2::annotate(
      "text", x = 0, y = std_max + 0.25,
      label = latex2exp::TeX("Correlation \\textit{r}"), size = 4,
      family = "sans"
    ) +
    ggplot2::geom_point(
      data = model_points,
      ggplot2::aes(x = x, y = y), size = point_size
    ) +
    ggplot2::annotate("point", x = 1, y = 0, size = 3, colour = "red3")

  if (isTRUE(label)) {
    p <- p + ggrepel::geom_label_repel(
      data = model_points,
      ggplot2::aes(x = x, y = y, label = Model),
      box.padding = 0.35, point.padding = 0.8,
      segment.color = "grey50", size = label_size, family = "sans", seed = 0
    )
  }
  p + ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    text = ggplot2::element_text(size = 12, family = "sans"),
    panel.border = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank()
  )
}

theme_taylor <- function(base_size = 11) {
  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = base_size),
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 12, family = "sans"),
      panel.border = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  )
}
