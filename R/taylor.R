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
#' @details Missing pairs are removed separately per model when `na.rm = TRUE`.
#'   Two complete pairs with non-zero observation SD are required. All plotted
#'   statistics can be retrieved with [diagram_stats()]. Sample SD normalization
#'   and the original constant-prediction convention are documented there.
#' @param label Logical; draw model labels with `ggrepel`?
#' @param point_size Numeric size of model points.
#' @param label_size Numeric text size for model labels.
#' @param half Logical; if `TRUE`, draw only the positive-correlation half of
#'   the Taylor diagram (`r = 0` to `1`). If `FALSE`, draw the full Taylor
#'   diagram (`r = -1` to `1`).
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
#' mods <- list(
#'   perfect = obs,
#'   biased = obs + 1,
#'   smooth = c(2, 2, 3, 4, 4)
#' )
#'
#' gg_taylor(mods, obs, label = TRUE)
#' gg_taylor(mods, obs, label = TRUE, half = TRUE)
#'
#' @export
gg_taylor <- function(mods, obs, label = FALSE, point_size = 6,
                      label_size = 4, na.rm = TRUE, half = FALSE) {

  validate_plot_sizes(label, point_size, label_size)

  if (!is.logical(half) || length(half) != 1L || is.na(half)) {
    stop("`half` must be TRUE or FALSE.", call. = FALSE)
  }

  statistics <- diagram_stats(mods, obs, na.rm = na.rm)

  model_points <- data.frame(
    Cor = statistics$r,
    Std = statistics$sd_ratio,
    Model = statistics$model
  )

  # A constant model is at the origin; its angular correlation is undefined.
  geometry_cor <- ifelse(
    is.na(model_points$Cor),
    0,
    model_points$Cor
  )

  model_points$x <- model_points$Std * geometry_cor

  model_points$y <- model_points$Std *
    sqrt(
      pmax(
        0,
        1 - geometry_cor^2
      )
    )

  # In the positive-correlation layout, omit models with negative correlation.
  if (isTRUE(half)) {
    model_points <- model_points[
      is.na(model_points$Cor) | model_points$Cor >= 0,
      ,
      drop = FALSE
    ]
  }

  obs_std <- 1

  std_max <- ceiling(
    max(
      c(
        obs_std,
        model_points$Std,
        2
      ),
      na.rm = TRUE
    )
  )

  if (std_max > .Machine$double.xmax / 6) {
    stop(
      "Standard deviation ratio is too large to draw Taylor contours.",
      call. = FALSE
    )
  }

  std_major <- seq(
    0,
    std_max,
    length.out = 5
  )

  angle_max <- if (isTRUE(half)) {
    pi / 2
  } else {
    pi
  }

  # Standard-deviation arcs.
  semicircle <- do.call(
    rbind,
    lapply(
      std_major,
      function(radius) {

        angle <- seq(
          0,
          angle_max,
          length.out = 1001
        )

        data.frame(
          x = radius * cos(angle),
          y = radius * sin(angle),
          label = radius
        )
      }
    )
  )

  # Correlation rays.
  if (isTRUE(half)) {

    cor_major <- c(
      0,
      seq(0.1, 0.9, 0.1),
      0.95,
      0.99,
      1
    )

  } else {

    cor_major <- c(
      seq(-1, 1, 0.1),
      -0.95,
      0.95,
      -0.99,
      0.99
    )
  }

  cor_major <- sort(unique(cor_major))

  rays <- do.call(
    rbind,
    lapply(
      cor_major,
      function(correlation) {

        angle <- acos(correlation)

        data.frame(
          xend = std_max * cos(angle),
          yend = std_max * sin(angle),
          angle = angle,
          label = correlation
        )
      }
    )
  )

  # Correlation labels just outside the outer arc.
  correlation_label_radius <- 1.055 * std_max

  rays$label_x <- correlation_label_radius * cos(rays$angle)
  rays$label_y <- correlation_label_radius * sin(rays$angle)

  if (isTRUE(half)) {

    # Small adjustments at both ends of the correlation scale.
    rays$label_x[rays$label == 0] <- 0.025 * std_max
    rays$label_y[rays$label == 0] <- 1.065 * std_max

    rays$label_x[rays$label == 1] <- 1.055 * std_max
    rays$label_y[rays$label == 1] <- 0
  }

  # Centred RMSD contours.
  #
  # Draw contours every 0.5 in both layouts.
  rmsd_values <- seq(
    0.5,
    max(2, 1.5 * std_max),
    by = 0.5
  )

  theta_rmsd <- seq(
    0,
    pi,
    length.out = 1501
  )

  circle_data <- do.call(
    rbind,
    lapply(
      rmsd_values,
      function(radius) {

        data.frame(
          xcircle = 1 + radius * cos(theta_rmsd),
          ycircle = radius * sin(theta_rmsd),
          labelc = radius
        )
      }
    )
  )

  # Clip centred RMSD contours to the displayed Taylor domain.
  distance_origin <- sqrt(
    circle_data$xcircle^2 +
      circle_data$ycircle^2
  )

  outside <- distance_origin > std_max

  if (isTRUE(half)) {
    outside <- outside | circle_data$xcircle < 0
  }

  circle_data[
    outside,
    c("xcircle", "ycircle")
  ] <- NA_real_

  # Position RMSD labels along a straight line starting at the reference
  # point (1, 0).
  #
  # Positive-correlation layout:
  #   line points toward correlation r = 0.1 on the outer arc.
  #
  # Full layout:
  #   line points toward correlation r = -0.6 on the outer arc.
  target_cor <- if (isTRUE(half)) {
    0.1
  } else {
    -0.6
  }

  target_angle <- acos(target_cor)

  target_x <- std_max * cos(target_angle)
  target_y <- std_max * sin(target_angle)

  dx <- target_x - 1
  dy <- target_y

  direction_length <- sqrt(dx^2 + dy^2)

  ux <- dx / direction_length
  uy <- dy / direction_length

  circle_labels <- data.frame(
    xcircle = 1 + rmsd_values * ux,
    ycircle = rmsd_values * uy,
    labelc = rmsd_values
  )

  label_distance_origin <- sqrt(
    circle_labels$xcircle^2 +
      circle_labels$ycircle^2
  )

  keep_labels <- circle_labels$ycircle >= 0 &
    label_distance_origin <= std_max

  if (isTRUE(half)) {
    keep_labels <- keep_labels &
      circle_labels$xcircle >= 0
  }

  circle_labels <- circle_labels[
    keep_labels,
    ,
    drop = FALSE
  ]

  circle_labels$label_text <- format(
    circle_labels$labelc,
    trim = TRUE,
    scientific = FALSE
  )

  p <- ggplot2::ggplot(model_points) +
    ggplot2::coord_equal(clip = "off") +

    # Standard-deviation arcs.
    ggplot2::geom_line(
      data = semicircle,
      ggplot2::aes(
        x = x,
        y = y,
        group = label
      ),
      linetype = "solid",
      colour = "black",
      linewidth = 0.6
    ) +

    # Correlation rays.
    ggplot2::geom_segment(
      data = rays,
      ggplot2::aes(
        x = 0,
        y = 0,
        xend = xend,
        yend = yend
      ),
      linetype = "dashed",
      colour = "black",
      linewidth = 0.35
    ) +

    # Horizontal boundary.
    ggplot2::annotate(
      "segment",
      x = if (isTRUE(half)) 0 else -std_max,
      y = 0,
      xend = std_max,
      yend = 0,
      colour = "black",
      linewidth = 0.8
    )

  # Explicit vertical boundary for the positive-correlation layout.
  if (isTRUE(half)) {

    p <- p +
      ggplot2::annotate(
        "segment",
        x = 0,
        y = 0,
        xend = 0,
        yend = std_max,
        colour = "black",
        linewidth = 0.8
      )
  }

  p <- p +

    # Centred RMSD contours.
    ggplot2::geom_line(
      data = circle_data,
      ggplot2::aes(
        x = xcircle,
        y = ycircle,
        group = labelc
      ),
      linetype = "dashed",
      colour = "red3",
      linewidth = 0.6,
      na.rm = TRUE
    ) +

    # RMSD labels.
    ggplot2::geom_label(
      data = circle_labels,
      ggplot2::aes(
        x = xcircle,
        y = ycircle,
        label = label_text
      ),
      colour = "red3",
      fill = "white",
      linewidth = 0,
      size = 3.3,
      family = "sans",
      label.padding = grid::unit(0.045, "lines"),
      label.r = grid::unit(0, "lines"),
      hjust = 0.5,
      vjust = 0.5,
      na.rm = TRUE
    ) +

    # Correlation labels.
    ggplot2::geom_text(
      data = rays,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        label = label
      ),
      size = 4,
      colour = "black",
      family = "sans"
    ) +

    theme_taylor() +

    ggplot2::xlab(
      latex2exp::TeX(
        "Standardized standard deviation $\\sigma^*$"
      )
    )

  # Correlation title.
  if (isTRUE(half)) {

    title_angle <- pi / 4
    title_radius <- 1.27 * std_max

    p <- p +
      ggplot2::annotate(
        "text",
        x = title_radius * cos(title_angle),
        y = title_radius * sin(title_angle),
        label = latex2exp::TeX("Correlation $r$"),
        size = 4.5,
        family = "sans",
        angle = -45
      )

  } else {

    p <- p +
      ggplot2::annotate(
        "text",
        x = 0,
        y = 1.12 * std_max,
        label = latex2exp::TeX("Correlation $r$"),
        size = 4,
        family = "sans"
      )
  }

  # Model points and observation reference point.
  p <- p +
    ggplot2::geom_point(
      data = model_points,
      ggplot2::aes(
        x = x,
        y = y
      ),
      size = point_size
    ) +
    ggplot2::annotate(
      "point",
      x = 1,
      y = 0,
      size = 3,
      colour = "red3"
    )

  # Optional model labels.
  if (isTRUE(label)) {

    p <- p +
      ggrepel::geom_label_repel(
        data = model_points,
        ggplot2::aes(
          x = x,
          y = y,
          label = Model
        ),
        box.padding = 0.35,
        point.padding = 0.8,
        segment.color = "grey50",
        size = label_size,
        family = "sans",
        seed = 0
      )
  }

  # Axes and plot limits.
  if (isTRUE(half)) {

    p <- p +
      ggplot2::scale_x_continuous(
        limits = c(
          0,
          1.14 * std_max
        ),
        breaks = std_major,
        expand = ggplot2::expansion(
          mult = c(0, 0)
        )
      ) +
      ggplot2::scale_y_continuous(
        limits = c(
          0,
          1.14 * std_max
        ),
        expand = ggplot2::expansion(
          mult = c(0, 0)
        )
      )

  } else {

    p <- p +
      ggplot2::scale_x_continuous(
        limits = c(
          -1.13 * std_max,
          1.13 * std_max
        ),
        labels = abs,
        expand = ggplot2::expansion(
          mult = c(0, 0)
        )
      ) +
      ggplot2::scale_y_continuous(
        limits = c(
          0,
          1.13 * std_max
        ),
        expand = ggplot2::expansion(
          mult = c(0, 0)
        )
      )
  }

  p +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(
        size = 12,
        family = "sans"
      ),
      panel.border = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(
        t = 35,
        r = 35,
        b = 10,
        l = 10
      )
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
      text = ggplot2::element_text(
        size = 12,
        family = "sans"
      ),
      panel.border = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  )
}
