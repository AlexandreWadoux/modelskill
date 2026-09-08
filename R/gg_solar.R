#' Create a solar diagram
#'
#' Creates a solar diagram comparing quantitative prediction models with an
#' observation vector. The horizontal coordinate is normalized mean error
#' (ME*) and the vertical coordinate is standardized unbiased root mean square
#' difference (SDE*).
#'
#' The default pale-yellow correlation regions follow Wadoux, Walvoort, and
#' Brus (2022). Points are coloured by the model-efficiency coefficient
#' R-squared / NSE / MEC by default.
#'
#' @inheritParams diagram_stats
#' @seealso [diagram_stats()], [model_metrics()], [gg_taylor()], [gg_target()]
#'
#' @details
#' Missing pairs are removed separately per model when `na.rm = TRUE`.
#' Two complete pairs with non-zero observation SD are required.
#'
#' By default, point colour represents the model-efficiency coefficient
#' (uppercase R-squared), equivalent to NSE and MEC in modelskill. This must
#' not be confused with squared Pearson correlation, available with
#' `colour_by = "r2"`. The default legend title for the efficiency colour
#' scale is simply `R²`.
#'
#' The function returns an ordinary ggplot2 object. Styling that belongs to
#' the ggplot2 ecosystem can therefore be applied after the function call.
#' Use `labs()` for titles, `theme()` for typography and legend placement,
#' `scale_colour_*()` to replace the point-colour scale, and `scale_fill_*()`
#' to replace the correlation-region palette.
#'
#' Reference-axis arguments control the manually drawn solar axes rather than
#' clipping limits. Observations outside those reference axes remain visible.
#' To zoom while retaining equal x and y scaling, add a new `coord_fixed()`
#' layer with the desired limits.
#'
#' @param colorval Optional finite numeric vector with one value per model,
#'   in model order (names do not reorder values). When supplied, it overrides
#'   the continuous metric selected by `colour_by`.
#' @param colorval.name Optional point-colour legend title. The title can also
#'   be replaced afterwards with `labs(colour = ...)`.
#' @param colour_by Character string defining point colouring. `"efficiency"`
#'   uses R-squared / NSE / MEC and is the default; `"model"` gives every model
#'   a categorical colour and model-name legend; `"correlation"` uses Pearson
#'   correlation; and `"r2"` uses squared Pearson correlation.
#' @param x.axis_begin Lower endpoint of the manually drawn horizontal
#'   reference axis.
#' @param x.axis_end Upper endpoint of the manually drawn horizontal reference
#'   axis.
#' @param y.axis_end Upper endpoint of the manually drawn vertical reference
#'   axis. Defaults to 1.1 so that the outer correlation circle at 1 and points
#'   lying on it remain clearly visible.
#' @param by Spacing between manually drawn reference-axis ticks.
#' @param label Logical; draw model names beside points using `ggrepel`?
#' @param point_size Numeric point size.
#' @param label_size Numeric size of model labels.
#' @param legend Logical; show the point-colour and correlation-region legends?
#' @param reference Logical; draw the original correlation regions and outer
#'   reference circle?
#'
#' @section Coordinates and labels:
#' The horizontal coordinate is normalized mean error (nME; ME*) and the
#' vertical coordinate is standardized unbiased root mean square difference
#' (sde; SDE*). Correlation-region radii are calculated exactly from their
#' correlation thresholds as `sqrt(1 - r^2)`; their interpretation requires
#' the normalization assumptions documented in [diagram_stats()].
#'
#' @section Interpretation:
#' The solar diagram uses the error decomposition
#' \deqn{\mathrm{RMSE}^{*2} = \mathrm{ME}^{*2} + \mathrm{SDE}^{*2},}
#' where ME*, SDE*, and RMSE* are respectively mean error, centred root mean
#' square difference, and RMSE divided by the population-moment observation
#' standard deviation. The distance from the origin is RMSE*. The origin is
#' perfect prediction; points on the vertical axis have no mean error; negative
#' ME* indicates overprediction under the package convention `obs - pred`; and
#' positive ME* indicates underprediction.
#'
#' Points inside the outer RMSE* = 1 circle improve on predicting the observed
#' mean (equivalently, MEC/NSE/R2 is positive). The pale-yellow regions give
#' lower bounds on correlation, rather than exact correlation values. For
#' example, a point near the origin and inside the correlation-greater-than-0.9
#' region has low total error and strong pattern agreement; a point far left or right is
#' systematically biased; and a point high on the vertical axis is unbiased but
#' has substantial pattern or spread disagreement.
#'
#' @section ggplot2 customization:
#' Arguments that change the statistical content or core diagram construction
#' are exposed directly by `gg_solar()`. Ordinary appearance is intentionally
#' left to ggplot2. For example, users can add `labs()`, `theme()`, a replacement
#' `scale_colour_*()` or `scale_fill_*()`, or a replacement `coord_fixed()`.
#'
#' @return A `ggplot2` plot object that can be extended with ordinary ggplot2
#'   layers, scales, labels, themes, and coordinates.
#'
#' @references
#' Wadoux, A. M. J.-C., Walvoort, D. J. J., and Brus, D. J. (2022). An
#' integrated approach for the evaluation of quantitative soil maps through
#' Taylor and solar diagrams. *Geoderma*, 405, 115332.
#' <doi:10.1016/j.geoderma.2021.115332>
#'
#' Jolliff, J. K., Kindle, J. C., Shulman, I., Penta, B., Friedrichs, M. A. M.,
#' Helber, R., and Arnone, R. A. (2009). Summary diagrams for coupled
#' hydrodynamic-ecosystem model skill assessment. *Journal of Marine Systems*,
#' 76, 64-82. <doi:10.1016/j.jmarsys.2008.05.014>
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#' mods <- list(perfect = obs, biased = obs + 1)
#'
#' # Default: point colour represents R-squared / NSE / MEC.
#' gg_solar(mods, obs)
#'
#' # Give every model a categorical colour and a model-name legend.
#' gg_solar(mods, obs, colour_by = "model")
#'
#' # Write model names directly beside points.
#' gg_solar(mods, obs, label = TRUE)
#'
#' # Standard ggplot2 customization.
#' gg_solar(mods, obs) +
#'   ggplot2::labs(title = "Model performance") +
#'   ggplot2::theme(legend.position = "bottom")
#' @export
gg_solar <- function(
    mods,
    obs,
    colorval = NULL,
    colorval.name = NULL,
    colour_by = c("efficiency", "model", "correlation", "r2"),
    x.axis_begin = -1,
    x.axis_end = 1,
    y.axis_end = 1.1,
    by = 0.1,
    label = FALSE,
    point_size = 7,
    label_size = 4,
    na.rm = TRUE,
    legend = TRUE,
    reference = TRUE) {

  validate_plot_sizes(label, point_size, label_size)
  check_flag(legend, "legend")
  check_flag(reference, "reference")
  colour_by <- match.arg(colour_by)

  data <- diagram_stats(mods, obs, na.rm = na.rm)

  validate_diagram_arguments(
    colorval, colorval.name, x.axis_begin, x.axis_end,
    y.axis_end, by, label, nrow(data)
  )

  # Uppercase R2 in modelskill is the model-efficiency coefficient and is
  # equivalent to NSE/MEC. Derive it from the solar coordinates so that missing
  # pairs are handled identically to diagram_stats() for every model.
  data$R2_NSE <- 1 - data$sde^2 - data$nME^2

  custom_colour <- !is.null(colorval)

  if (custom_colour && identical(colour_by, "model")) {
    stop(
      paste0(
        "`colorval` and `colour_by = \"model\"` cannot be used together. ",
        "Use either a continuous colour variable or categorical model colours."
      ),
      call. = FALSE
    )
  }

  if (custom_colour) {
    data$colvar <- colorval
    discrete_colour <- FALSE
    if (is.null(colorval.name)) colorval.name <- "Value"
  } else if (identical(colour_by, "efficiency")) {
    data$colvar <- data$R2_NSE
    discrete_colour <- FALSE
    if (is.null(colorval.name)) colorval.name <- paste0("R", intToUtf8(0x00B2))
  } else if (identical(colour_by, "correlation")) {
    data$colvar <- data$r
    discrete_colour <- FALSE
    if (is.null(colorval.name)) colorval.name <- "Correlation"
  } else if (identical(colour_by, "r2")) {
    data$colvar <- data$r^2
    discrete_colour <- FALSE
    if (is.null(colorval.name)) colorval.name <- paste0("r", intToUtf8(0x00B2))
  } else {
    data$colvar <- factor(data$model, levels = data$model)
    discrete_colour <- TRUE
    if (is.null(colorval.name)) colorval.name <- "Model"
  }

  # Retain aliases used by the original implementation.
  data$uRMSDnorm_sigmaD <- data$sde
  data$uRMSDnorm <- data$sde

  circle <- function(radius) {
    theta <- seq(0, pi, length.out = 1001)
    data.frame(
      x = radius * cos(theta),
      y = radius * sin(theta),
      label = radius
    )
  }

  circle2 <- circle(1)
  circle095 <- circle(sqrt(1 - 0.95^2)); circle095$lab <- "r>0.95"
  circle09 <- circle(sqrt(1 - 0.9^2)); circle09$lab <- "r>0.9"
  circle07 <- circle(sqrt(1 - 0.7^2)); circle07$lab <- "r>0.7"
  circle0 <- circle(1); circle0$lab <- "r>0"
  circ_pol <- rbind(circle0, circle07, circle09, circle095)

  x_ticks <- data.frame(
    ticks = seq(x.axis_begin, x.axis_end, by = by),
    zero = 0
  )
  y_ticks <- data.frame(
    ticks = seq(0, y.axis_end, by = by),
    zero = 0
  )
  x_labs <- data.frame(
    lab = c(x.axis_begin, 0, x.axis_end),
    zero = 0
  )
  y_labs <- data.frame(
    lab = y.axis_end,
    zero = 0
  )
  tick_size <- (x.axis_end - x.axis_begin) / 128

  # Do not impose clipping limits: the axis arguments define reference geometry,
  # while the reference layers themselves establish the default plotting range.
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = nME, y = uRMSDnorm_sigmaD)
  ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.06, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.08, 0.04))
    ) +
    ggplot2::coord_fixed(ratio = 1, clip = "off")

  if (isTRUE(reference)) {
    p <- p +
      ggplot2::geom_polygon(
        data = circ_pol,
        ggplot2::aes(x = x, y = y, fill = lab),
        inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "r>0" = "#FEECA4",
          "r>0.7" = "#FEF4B6",
          "r>0.9" = "#FFFCD7",
          "r>0.95" = "#FFFFE5"
        ),
        breaks = c("r>0", "r>0.7", "r>0.9", "r>0.95"),
        labels = c(
          expression(italic(r) > 0),
          expression(italic(r) > 0.7),
          expression(italic(r) > 0.9),
          expression(italic(r) > 0.95)
        ),
        name = NULL
      ) +
      ggplot2::geom_path(
        data = circle2,
        ggplot2::aes(x = x, y = y, group = label),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.8
      )
  }

  p <- p +
    ggplot2::annotate(
      "segment",
      x = 0, xend = 0,
      y = 0, yend = y.axis_end,
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "segment",
      x = x.axis_begin, xend = x.axis_end,
      y = 0, yend = 0,
      linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = x_ticks,
      ggplot2::aes(
        x = ticks, xend = ticks,
        y = zero, yend = zero + tick_size
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = y_ticks,
      ggplot2::aes(
        x = zero - tick_size, xend = zero + tick_size,
        y = ticks, yend = ticks
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      x = expression(ME^"*"),
      y = expression(SDE^"*"),
      fill = NULL
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.margin = grid::unit(c(1, 1, 1.2, 1), "cm"),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 8)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 8)
      ),
      legend.position = "right",
      legend.justification = "center",
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 8),
      legend.text = ggplot2::element_text(hjust = 0)
    ) +
    ggplot2::geom_text(
      data = x_labs,
      ggplot2::aes(x = lab, y = zero, label = lab),
      inherit.aes = FALSE,
      vjust = 1.6,
      size = 3.5
    ) +
    ggplot2::geom_text(
      data = y_labs,
      ggplot2::aes(x = zero, y = lab, label = lab),
      inherit.aes = FALSE,
      hjust = 1.5,
      size = 3.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = colvar),
      shape = 19,
      size = point_size
    )

  if (isTRUE(discrete_colour)) {
    p <- p +
      ggplot2::scale_colour_discrete(name = colorval.name) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          order = 1,
          override.aes = list(size = min(point_size, 5))
        )
      )
  } else {
    p <- p +
      viridis::scale_color_viridis(
        option = "A",
        na.value = "grey50",
        name = colorval.name
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_colourbar(order = 1)
      )
  }

  if (isTRUE(reference)) {
    p <- p +
      ggplot2::guides(
        fill = ggplot2::guide_legend(order = 2)
      )
  }

  if (!isTRUE(legend)) {
    p <- p +
      ggplot2::guides(
        colour = "none",
        fill = "none"
      )
  }

  if (isTRUE(label)) {
    p <- p +
      ggrepel::geom_label_repel(
        ggplot2::aes(label = model),
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = "grey50",
        size = label_size,
        seed = 0
      )
  }

  p
}