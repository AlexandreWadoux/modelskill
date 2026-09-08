#' Create a Taylor diagram
#'
#' Creates a Taylor diagram comparing one or more quantitative prediction
#' models with an observation vector. The radial coordinate is the model
#' standard deviation divided by the observation standard deviation and the
#' polar angle represents the Pearson correlation. Optional centred
#' root-mean-square distance (RMSD) contours are drawn around the observation
#' reference point.
#'
#' The geometry and default styling follow Wadoux, Walvoort, and Brus (2022)
#' and the original implementation in the accompanying repository.
#'
#' @inheritParams diagram_stats
#' @seealso [diagram_stats()], [model_metrics()], [gg_solar()], [gg_target()]
#'
#' @details
#' Missing pairs are removed separately per model when `na.rm = TRUE`.
#' Two complete pairs with non-zero observation SD are required. All plotted
#' statistics can be retrieved with [diagram_stats()].
#'
#' Model names can be displayed either directly on the diagram with
#' `label = TRUE` or through a colour legend with `legend = TRUE`.
#'
#' With `legend = TRUE`, model points use the standard ggplot2 discrete colour
#' palette. Because the returned object is a regular ggplot2 object, the model
#' colour scale, legend, theme, titles, fonts, and other graphical elements can
#' subsequently be customised with ordinary ggplot2 layers.
#'
#' RMSD contours can be removed with `rmsd = FALSE`, recoloured with
#' `rmsd_colour`, or placed at user-defined values with `rmsd_breaks`.
#'
#' @section Interpretation:
#' Let sigma-star denote the prediction standard deviation divided by the
#' observation standard deviation, and let `r` be Pearson correlation.
#' The Taylor geometry follows
#' \deqn{\mathrm{SDE}^{*} = \sqrt{1 + \sigma_{\mathrm{star}}^2 -
#' 2\sigma_{\mathrm{star}}r},}
#' where SDE* is the centred (unbiased) root mean square difference divided by
#' the observation standard deviation. Radial distance gives `sigma_star`; the
#' polar angle is `acos(r)`; and the reference point has a standard-deviation
#' ratio of one and correlation of one. Points nearer the reference point have
#' smaller unbiased error.
#'
#' A point inside the unit-radius arc has less variation than the observations
#' (a smoother prediction), while a point outside it has greater variation.
#' Points nearer the horizontal positive-correlation axis have stronger pattern
#' agreement. The diagram does not show mean error: a model can be close to the
#' reference point but systematically biased. Use [gg_solar()] or [gg_target()]
#' together with [bias()] when mean error is important.
#'
#' @param label Logical; draw model names directly on the diagram using
#'   `ggrepel`?
#' @param legend Logical; colour model points by model and display a legend?
#'   The standard ggplot2 discrete colour palette is used by default.
#' @param point_size Numeric size of model points.
#' @param label_size Numeric text size for model labels.
#' @param half Logical; if `TRUE`, draw only the positive-correlation portion
#'   of the Taylor diagram (r = 0 to r = 1). If `FALSE`, draw the full Taylor
#'   diagram (r = -1 to r = 1).
#' @param rmsd Logical; show centred RMSD contours and their labels?
#' @param rmsd_colour Character string giving the colour of the RMSD contours
#'   and labels.
#' @param rmsd_breaks Optional numeric vector giving the RMSD contour values.
#'   If `NULL`, contours are drawn every 0.5 units.
#'
#' @return A `ggplot2` plot object. It can be extended with ordinary ggplot2
#'   layers, scales, labels, and themes.
#'
#' @references
#' Wadoux, A. M. J.-C., Walvoort, D. J. J., and Brus, D. J. (2022).
#' An integrated approach for the evaluation of quantitative soil maps
#' through Taylor and solar diagrams. *Geoderma*, 405, 115332.
#' <doi:10.1016/j.geoderma.2021.115332>
#'
#' Taylor, K. E. (2001). Summarizing multiple aspects of model performance in a
#' single diagram. *Journal of Geophysical Research*, 106, 7183-7192.
#' <doi:10.1029/2000JD900719>
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#'
#' mods <- list(
#'   perfect = obs,
#'   biased = obs + 1,
#'   smooth = c(2, 2, 3, 4, 4)
#' )
#'
#' # Model names directly on the diagram
#' gg_taylor(mods, obs, label = TRUE)
#'
#' # Positive-correlation Taylor diagram
#' gg_taylor(mods, obs, label = TRUE, half = TRUE)
#'
#' # Coloured points and legend
#' gg_taylor(mods, obs, legend = TRUE, half = TRUE)
#'
#' # Remove RMSD contours completely
#' gg_taylor(mods, obs, legend = TRUE, half = TRUE, rmsd = FALSE)
#'
#' # Change RMSD colour
#' gg_taylor(
#'   mods, obs,
#'   legend = TRUE,
#'   half = TRUE,
#'   rmsd_colour = "steelblue"
#' )
#'
#' # Define custom RMSD contours
#' gg_taylor(
#'   mods, obs,
#'   half = TRUE,
#'   rmsd_breaks = c(0.25, 0.5, 1, 1.5, 2)
#' )
#'
#' # Ordinary ggplot2 customisation remains available
#' gg_taylor(mods, obs, legend = TRUE, half = TRUE) +
#'   ggplot2::scale_colour_brewer(palette = "Dark2") +
#'   ggplot2::theme(legend.position = "bottom")
#'
#' @export
gg_taylor <- function(mods, obs,
                      label = FALSE,
                      legend = FALSE,
                      point_size = 6,
                      label_size = 4,
                      na.rm = TRUE,
                      half = FALSE,
                      rmsd = TRUE,
                      rmsd_colour = "red3",
                      rmsd_breaks = NULL) {

  validate_plot_sizes(
    label,
    point_size,
    label_size
  )

  # ----------------------------------------------------------
  # Validate arguments
  # ----------------------------------------------------------

  if (!is.logical(legend) ||
      length(legend) != 1L ||
      is.na(legend)) {
    stop(
      "`legend` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(half) ||
      length(half) != 1L ||
      is.na(half)) {
    stop(
      "`half` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(rmsd) ||
      length(rmsd) != 1L ||
      is.na(rmsd)) {
    stop(
      "`rmsd` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.character(rmsd_colour) ||
      length(rmsd_colour) != 1L ||
      is.na(rmsd_colour) ||
      !nzchar(rmsd_colour)) {
    stop(
      "`rmsd_colour` must be a single colour name or colour code.",
      call. = FALSE
    )
  }

  if (!is.null(rmsd_breaks)) {

    if (!is.numeric(rmsd_breaks) ||
        length(rmsd_breaks) == 0L ||
        anyNA(rmsd_breaks) ||
        any(!is.finite(rmsd_breaks)) ||
        any(rmsd_breaks <= 0)) {
      stop(
        "`rmsd_breaks` must be NULL or a numeric vector of positive values.",
        call. = FALSE
      )
    }

    rmsd_breaks <- sort(
      unique(rmsd_breaks)
    )
  }

  if (isTRUE(label) && isTRUE(legend)) {
    stop(
      "Use either `label = TRUE` or `legend = TRUE`, not both.",
      call. = FALSE
    )
  }


  # ----------------------------------------------------------
  # Model statistics
  # ----------------------------------------------------------

  statistics <- diagram_stats(
    mods,
    obs,
    na.rm = na.rm
  )

  model_points <- data.frame(
    Cor = statistics$r,
    Std = statistics$sd_ratio,
    Model = statistics$model,
    stringsAsFactors = FALSE
  )

  # A constant model is placed at the origin because its
  # angular correlation is undefined.
  geometry_cor <- ifelse(
    is.na(model_points$Cor),
    0,
    model_points$Cor
  )

  model_points$x <-
    model_points$Std *
    geometry_cor

  model_points$y <-
    model_points$Std *
    sqrt(
      pmax(
        0,
        1 - geometry_cor^2
      )
    )


  # ----------------------------------------------------------
  # Positive-correlation layout
  # ----------------------------------------------------------

  if (isTRUE(half)) {

    model_points <- model_points[
      is.na(model_points$Cor) |
        model_points$Cor >= 0,
      ,
      drop = FALSE
    ]
  }


  # ----------------------------------------------------------
  # Diagram range
  # ----------------------------------------------------------

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


  # ----------------------------------------------------------
  # Standard-deviation arcs
  # ----------------------------------------------------------

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


  # ----------------------------------------------------------
  # Correlation grid
  # ----------------------------------------------------------

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

  cor_major <- sort(
    unique(cor_major)
  )

  rays <- do.call(
    rbind,
    lapply(
      cor_major,
      function(correlation) {

        angle <- acos(correlation)

        data.frame(
          xend =
            std_max *
            cos(angle),

          yend =
            std_max *
            sin(angle),

          angle =
            angle,

          label =
            correlation
        )
      }
    )
  )


  # ----------------------------------------------------------
  # Correlation labels
  # ----------------------------------------------------------

  # Place every correlation label at the same radial distance from the
  # outer arc. Angle-dependent justification keeps the nearest edge of
  # each label visually separated from the arc by a consistent amount.
  correlation_label_radius <-
    1.035 * std_max

  rays$label_x <-
    correlation_label_radius *
    cos(rays$angle)

  rays$label_y <-
    correlation_label_radius *
    sin(rays$angle)

  rays$label_hjust <-
    (1 - cos(rays$angle)) / 2

  rays$label_vjust <-
    (1 - sin(rays$angle)) / 2


  # ----------------------------------------------------------
  # Base Taylor diagram
  # ----------------------------------------------------------

  p <- ggplot2::ggplot(
    model_points
  ) +

    ggplot2::coord_equal(
      clip = "off"
    ) +

    # Standard-deviation arcs
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

    # Correlation rays
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

    # Horizontal boundary
    ggplot2::annotate(
      "segment",
      x = if (isTRUE(half)) {
        0
      } else {
        -std_max
      },
      y = 0,
      xend = std_max,
      yend = 0,
      colour = "black",
      linewidth = 0.8
    )


  # ----------------------------------------------------------
  # Vertical boundary for half diagram
  # ----------------------------------------------------------

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


  # ----------------------------------------------------------
  # RMSD contours
  # ----------------------------------------------------------

  if (isTRUE(rmsd)) {

    if (is.null(rmsd_breaks)) {

      rmsd_values <- seq(
        0.5,
        max(
          2,
          1.5 * std_max
        ),
        by = 0.5
      )

    } else {

      rmsd_values <- rmsd_breaks
    }


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
            xcircle =
              1 +
              radius *
              cos(theta_rmsd),

            ycircle =
              radius *
              sin(theta_rmsd),

            labelc =
              radius
          )
        }
      )
    )


    # Clip RMSD contours to the displayed Taylor domain.
    distance_origin <- sqrt(
      circle_data$xcircle^2 +
        circle_data$ycircle^2
    )

    outside <-
      distance_origin >
      std_max

    if (isTRUE(half)) {

      outside <-
        outside |
        circle_data$xcircle < 0
    }

    circle_data[
      outside,
      c(
        "xcircle",
        "ycircle"
      )
    ] <- NA_real_


    # --------------------------------------------------------
    # RMSD label alignment
    #
    # half = TRUE:
    # reference point -> correlation r = 0.1
    #
    # half = FALSE:
    # reference point -> correlation r = -0.6
    # --------------------------------------------------------

    target_cor <- if (isTRUE(half)) {
      0.1
    } else {
      -0.6
    }

    target_angle <-
      acos(target_cor)

    target_x <-
      std_max *
      cos(target_angle)

    target_y <-
      std_max *
      sin(target_angle)

    dx <- target_x - 1
    dy <- target_y

    direction_length <- sqrt(
      dx^2 +
        dy^2
    )

    ux <-
      dx /
      direction_length

    uy <-
      dy /
      direction_length


    circle_labels <- data.frame(
      xcircle =
        1 +
        rmsd_values * ux,

      ycircle =
        rmsd_values * uy,

      labelc =
        rmsd_values
    )


    label_distance_origin <- sqrt(
      circle_labels$xcircle^2 +
        circle_labels$ycircle^2
    )

    keep_labels <-
      circle_labels$ycircle >= 0 &
      label_distance_origin <= std_max

    if (isTRUE(half)) {

      keep_labels <-
        keep_labels &
        circle_labels$xcircle >= 0
    }

    circle_labels <-
      circle_labels[
        keep_labels,
        ,
        drop = FALSE
      ]


    circle_labels$label_text <-
      format(
        circle_labels$labelc,
        trim = TRUE,
        scientific = FALSE
      )


    # Add RMSD contours.
    p <- p +

      ggplot2::geom_line(
        data = circle_data,
        ggplot2::aes(
          x = xcircle,
          y = ycircle,
          group = labelc
        ),
        linetype = "dashed",
        colour = rmsd_colour,
        linewidth = 0.6,
        na.rm = TRUE
      ) +

      # RMSD labels
      ggplot2::geom_label(
        data = circle_labels,
        ggplot2::aes(
          x = xcircle,
          y = ycircle,
          label = label_text
        ),
        colour = rmsd_colour,
        fill = "white",
        linewidth = 0,
        size = 3.3,
        family = "sans",
        label.padding =
          grid::unit(
            0.045,
            "lines"
          ),
        label.r =
          grid::unit(
            0,
            "lines"
          ),
        hjust = 0.5,
        vjust = 0.5,
        na.rm = TRUE
      )
  }


  # ----------------------------------------------------------
  # Correlation labels
  # ----------------------------------------------------------

  p <- p +

    ggplot2::geom_text(
      data = rays,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        label = label,
        hjust = label_hjust,
        vjust = label_vjust
      ),
      size = 4,
      colour = "black",
      family = "sans"
    ) +

    theme_taylor() +

    ggplot2::xlab(expression("Standardized standard deviation " * sigma^"*"))


  # ----------------------------------------------------------
  # Correlation title
  # ----------------------------------------------------------

  if (isTRUE(half)) {

    title_angle <-
      pi / 4

    # Keep the correlation title close to the outer arc in the half diagram.
    title_radius <-
      1.17 *
      std_max

    p <- p +
      ggplot2::annotate(
        "text",
        x =
          title_radius *
          cos(title_angle),

        y =
          title_radius *
          sin(title_angle),

        label = expression("Correlation " * italic(r)),

        size = 4.5,
        family = "sans",
        angle = -45
      )

  } else {

    p <- p +
      ggplot2::annotate(
        "text",
        x = 0,
        y =
          1.12 *
          std_max,

        label = expression("Correlation " * italic(r)),

        size = 4,
        family = "sans"
      )
  }


  # ----------------------------------------------------------
  # Model points
  # ----------------------------------------------------------

  if (isTRUE(legend)) {

    # Model is mapped to colour so ggplot2 automatically supplies
    # its normal discrete categorical colour palette.
    p <- p +
      ggplot2::geom_point(
        data = model_points,
        ggplot2::aes(
          x = x,
          y = y,
          colour = Model
        ),
        size = point_size
      ) +

      ggplot2::labs(
        colour = "Model"
      )

  } else {

    p <- p +
      ggplot2::geom_point(
        data = model_points,
        ggplot2::aes(
          x = x,
          y = y
        ),
        colour = "black",
        size = point_size
      )
  }


  # Observation reference point.
  p <- p +
    ggplot2::annotate(
      "point",
      x = 1,
      y = 0,
      size = 3,
      colour = "red3"
    )


  # ----------------------------------------------------------
  # Direct model labels
  # ----------------------------------------------------------

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


  # ----------------------------------------------------------
  # Plot limits
  # ----------------------------------------------------------

  if (isTRUE(half)) {

    p <- p +

      ggplot2::scale_x_continuous(
        limits = c(
          0,
          1.14 * std_max
        ),
        breaks = std_major,
        expand =
          ggplot2::expansion(
            mult = c(0, 0)
          )
      ) +

      ggplot2::scale_y_continuous(
        limits = c(
          0,
          1.14 * std_max
        ),
        expand =
          ggplot2::expansion(
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
        expand =
          ggplot2::expansion(
            mult = c(0, 0)
          )
      ) +

      ggplot2::scale_y_continuous(
        limits = c(
          0,
          1.13 * std_max
        ),
        expand =
          ggplot2::expansion(
            mult = c(0, 0)
          )
      )
  }


  # ----------------------------------------------------------
  # Final theme
  # ----------------------------------------------------------

  p +
    ggplot2::theme(
      axis.ticks.y =
        ggplot2::element_blank(),

      axis.text.y =
        ggplot2::element_blank(),

      axis.title.y =
        ggplot2::element_blank(),

      panel.background =
        ggplot2::element_blank(),

      panel.grid =
        ggplot2::element_blank(),

      panel.border =
        ggplot2::element_blank(),

      axis.line.x =
        ggplot2::element_blank(),

      text =
        ggplot2::element_text(
          size = 12,
          family = "sans"
        ),

      plot.margin =
        ggplot2::margin(
          t = 35,
          r = 35,
          b = 10,
          l = 10
        )
    )
}


theme_taylor <- function(base_size = 11) {

  ggplot2::`%+replace%`(
    ggplot2::theme_bw(
      base_size = base_size
    ),
    ggplot2::theme(
      axis.ticks.y =
        ggplot2::element_blank(),

      axis.text.y =
        ggplot2::element_blank(),

      axis.title.y =
        ggplot2::element_blank(),

      panel.background =
        ggplot2::element_blank(),

      text =
        ggplot2::element_text(
          size = 12,
          family = "sans"
        ),

      panel.border =
        ggplot2::element_blank(),

      axis.line.x =
        ggplot2::element_blank(),

      panel.grid =
        ggplot2::element_blank()
    )
  )
}
