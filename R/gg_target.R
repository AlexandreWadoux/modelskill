#' Create a target diagram
#'
#' Creates a target diagram comparing quantitative prediction models with an
#' observation vector. The horizontal coordinate is the signed standardized
#' unbiased root mean square difference and the vertical coordinate is
#' normalized mean error. Concentric circles provide reference thresholds
#' associated with correlation levels.
#'
#' The default geometry follows Wadoux, Walvoort, and Brus (2022). By default,
#' points are coloured by the model-efficiency coefficient R-squared, equivalent
#' to NSE and MEC in modelskill.
#'
#' @inheritParams diagram_stats
#' @seealso [diagram_stats()], [model_metrics()], [gg_taylor()], [gg_solar()]
#'
#' @details
#' Missing pairs are removed separately per model when `na.rm = TRUE`.
#' Two complete pairs with non-zero observation standard deviation are required.
#'
#' The default point-colour variable is the uppercase modelskill `R2()`, which
#' is equivalent to NSE and MEC. This should not be confused with lowercase
#' `r2()`, which is squared Pearson correlation.
#'
#' The function returns an ordinary ggplot2 object. Styling that belongs to the
#' ggplot2 ecosystem can therefore be applied after the function call. Use
#' `labs()` for titles, `theme()` for typography and legend placement, and
#' `scale_fill_*()` to replace the point-colour scale.
#'
#' The axis arguments control the manually drawn target-diagram reference axes
#' rather than clipping limits. Models outside those reference axes remain
#' visible. Equal coordinate scaling is retained so that the reference circles
#' remain circular.
#'
#' @param colorval Optional finite numeric vector with one value per model,
#'   in model order. When supplied, it overrides the continuous metric selected
#'   by `colour_by`.
#' @param colorval.name Optional point-colour legend title. The title can also
#'   be replaced afterwards with `labs(fill = ...)`.
#' @param colour_by Character string defining point colouring. `"efficiency"`
#'   uses uppercase R-squared / NSE / MEC and is the default; `"model"` gives
#'   every model a categorical colour and model-name legend; `"correlation"`
#'   uses Pearson correlation; and `"r2"` uses squared Pearson correlation.
#' @param axis_begin Lower endpoint of both manually drawn reference axes.
#'   Defaults to -1.5.
#' @param axis_end Upper endpoint of both manually drawn reference axes.
#'   Defaults to 1.5.
#' @param by Spacing between manually drawn reference-axis ticks.
#' @param label Logical; draw model names beside points using `ggrepel`?
#' @param point_size Numeric point size.
#' @param label_size Numeric size of model labels.
#' @param legend Logical; show the point-colour legend?
#' @param reference Logical; draw the original RMSD reference circles and
#'   their labels?
#'
#' @section Coordinates and labels:
#' The horizontal coordinate is `signed_sde` and the vertical coordinate is
#' `nME`. To retain the presentation of the original implementation, the
#' displayed horizontal title is ME* and the displayed vertical title is
#' SDE* multiplied by the sign of the standard-deviation difference.
#'
#' @section Interpretation:
#' The target diagram combines the solar error decomposition
#' \deqn{\mathrm{RMSE}^{*2} = \mathrm{ME}^{*2} + \mathrm{SDE}^{*2}}
#' with the sign of the prediction-versus-observation standard-deviation
#' difference. The signed SDE coordinate distinguishes predictions with less
#' variation than observations from predictions with greater variation; the
#' mean-error coordinate distinguishes overprediction (negative ME*) from
#' underprediction (positive ME*) under the `obs - pred` convention. The
#' distance to the origin is standardized RMSE, so points near the origin are
#' preferred.
#'
#' The circular contours are RMSE* references. A point near the origin is both
#' close in mean and in spread/pattern; a point displaced along the mean-error
#' direction is chiefly biased; and a point displaced along the signed-SDE
#' direction chiefly differs in variability or pattern. The displayed titles
#' intentionally retain the original implementation's visual orientation; use
#' the coordinate definitions above when interpreting position.
#'
#' @section ggplot2 customization:
#' Arguments that change the statistical content or core target-diagram
#' construction are exposed directly by `gg_target()`. Ordinary appearance is
#' intentionally left to ggplot2. For example, users can add `labs()`, `theme()`,
#' a replacement `scale_fill_*()`, or a replacement `coord_fixed()`.
#'
#' @return A `ggplot2` object that can be extended with ordinary ggplot2 layers,
#'   scales, labels, themes, and coordinates.
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
#' gg_target(mods, obs)
#'
#' # Give every model a categorical colour and a model-name legend.
#' gg_target(mods, obs, colour_by = "model")
#'
#' # Write model names directly beside points.
#' gg_target(mods, obs, label = TRUE)
#'
#' # Standard ggplot2 customization.
#' gg_target(mods, obs) +
#'   ggplot2::labs(title = "Model performance") +
#'   ggplot2::theme(legend.position = "bottom")
#' @export
gg_target <- function(
    mods,
    obs,
    colorval = NULL,
    colorval.name = NULL,
    colour_by = c("efficiency", "model", "correlation", "r2"),
    axis_begin = -1.5,
    axis_end = 1.5,
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
    colorval, colorval.name, axis_begin, axis_end,
    axis_end, by, label, nrow(data)
  )

  # Uppercase R2 in modelskill is the model-efficiency coefficient and is
  # equivalent to NSE/MEC. Derive it from the normalized target coordinates so
  # that missing pairs are handled identically to diagram_stats() for each model.
  data$R2_NSE <- 1 - data$sde^2 -
    data$nME^2 * data$n / (data$n - 1)

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

  # Retain the coordinate alias used by the original implementation.
  data$uRMSDnorm_sigmaD <- data$signed_sde

  circle <- function(radius) {
    theta <- seq(0, 2 * pi, length.out = 101)
    data.frame(
      x = radius * sin(theta),
      y = radius * cos(theta),
      r = radius
    )
  }

  circle_data <- rbind(circle(0.44), circle(0.71))
  circle_reference <- circle(1)
  circle_labels <- data.frame(
    x = c(
      circle_data$x[circle_data$r == 0.44][37],
      circle_data$x[circle_data$r == 0.71][37],
      circle_reference$x[37]
    ),
    y = c(
      circle_data$y[circle_data$r == 0.44][37],
      circle_data$y[circle_data$r == 0.71][37],
      circle_reference$y[37]
    ),
    label = c(0.9, 0.7, 1)
  )

  ticks <- subset(
    data.frame(
      ticks = seq(axis_begin, axis_end, by = by),
      zero = 0
    ),
    ticks != 0
  )

  labels <- subset(
    data.frame(
      label = c(axis_begin, axis_end),
      zero = 0
    ),
    label != 0
  )

  tick_size <- (axis_end - axis_begin) / 128

  # Do not impose clipping limits: axis_begin and axis_end define the manually
  # drawn reference axes, while observations outside them remain visible.
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = uRMSDnorm_sigmaD, y = nME)
  ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.06, 0.06))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.06, 0.06))
    ) +
    ggplot2::coord_fixed(ratio = 1, clip = "off")

  if (isTRUE(reference)) {
    p <- p +
      ggplot2::geom_path(
        data = circle_data,
        ggplot2::aes(x = x, y = y, group = r),
        inherit.aes = FALSE,
        colour = "black",
        linetype = 2
      ) +
      ggplot2::geom_path(
        data = circle_reference,
        ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = 0.8
      ) +
      ggplot2::geom_text(
        data = circle_labels,
        ggplot2::aes(x = x, y = y, label = label),
        inherit.aes = FALSE,
        size = 3.5,
        colour = "black"
      )
  }

  p <- p +
    ggplot2::annotate(
      "segment",
      x = 0, xend = 0,
      y = axis_begin, yend = axis_end,
      linewidth = 0.5
    ) +
    ggplot2::annotate(
      "segment",
      x = axis_begin, xend = axis_end,
      y = 0, yend = 0,
      linewidth = 0.5
    ) +
    ggplot2::geom_segment(
      data = ticks,
      ggplot2::aes(
        x = ticks, xend = ticks,
        y = zero, yend = zero + tick_size
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = ticks,
      ggplot2::aes(
        x = zero, xend = zero + tick_size,
        y = ticks, yend = ticks
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = label, y = zero, label = label),
      inherit.aes = FALSE,
      vjust = 1.5,
      size = 3.5
    ) +
    ggplot2::geom_text(
      data = labels,
      ggplot2::aes(x = zero, y = label, label = label),
      inherit.aes = FALSE,
      hjust = 1.5,
      size = 3.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = colvar),
      shape = 21,
      colour = "black",
      stroke = 0.4,
      size = point_size
    ) +

    # Retain the original target-diagram title orientation.
    ggplot2::labs(
      x = expression(ME^"*"),
      y = expression(SDE^"*" %.% sign(sigma[d]))
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
    )

  if (isTRUE(discrete_colour)) {
    p <- p +
      ggplot2::scale_fill_discrete(name = colorval.name) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          order = 1,
          override.aes = list(size = min(point_size, 5))
        )
      )
  } else {
    p <- p +
      viridis::scale_fill_viridis(
        option = "A",
        na.value = "grey50",
        name = colorval.name,
        labels = function(x) {
          sub("\\.?0+$", "", formatC(x, format = "f", digits = 2))
        }
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_colourbar(order = 1)
      )
  }

  if (!isTRUE(legend)) {
    p <- p + ggplot2::guides(fill = "none")
  }

  if (isTRUE(label)) {
    label_data <- data[
      ,
      c("uRMSDnorm_sigmaD", "nME", "model")
    ]

    if (isTRUE(reference)) {
      label_data <- rbind(
        label_data,
        data.frame(
          uRMSDnorm_sigmaD = circle_labels$x,
          nME = circle_labels$y,
          model = ""
        )
      )
    }

    p <- p +
      ggrepel::geom_label_repel(
        data = label_data,
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
