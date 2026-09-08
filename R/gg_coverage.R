#' Plot prediction-interval calibration
#'
#' Produces a prediction interval coverage probability (PICP) reliability plot,
#' also known in geostatistics as an accuracy plot. The plot compares empirical
#' prediction-interval coverage with the corresponding nominal coverage over one
#' or more central prediction-interval levels.
#'
#' For a nominal central prediction interval with coverage probability \eqn{p},
#' a calibrated predictive uncertainty model should contain approximately a
#' proportion \eqn{p} of independent validation observations. Consequently,
#' empirical PICP should satisfy
#'
#' \deqn{\mathrm{PICP}(p) \approx p,}
#'
#' and a well-calibrated model should follow the dashed 1:1 reference line.
#'
#' Points below the 1:1 line indicate **under-coverage**: fewer observations are
#' contained in the prediction intervals than expected. This generally indicates
#' prediction intervals that are too narrow and predictive uncertainty that is
#' underestimated.
#'
#' Points above the 1:1 line indicate **over-coverage**: more observations are
#' contained in the prediction intervals than expected. This generally indicates
#' prediction intervals that are wider than required and predictive uncertainty
#' that is overestimated.
#'
#' Evaluating several interval levels provides more information than evaluating
#' a single PICP value because it shows how calibration changes across the
#' predictive distribution. When `pred` and `predictive_sd` are supplied and
#' `levels` is left `NULL`, central normal prediction intervals are evaluated
#' from 1% to 99% nominal coverage.
#'
#' The accuracy-plot approach was developed for direct assessment of local
#' uncertainty in geostatistics by Deutsch (1997) and subsequently applied to
#' uncertainty evaluation in soil science by Goovaerts (2001). Similar
#' reliability diagnostics have also been used in digital soil mapping, including
#' Wadoux, Brus, and Heuvelink (2018).
#'
#' The graphical departures from the 1:1 line can be summarized numerically with
#' [accuracy_plot_metrics()], which calculates the total absolute area between
#' the empirical coverage curve and the reference line and separates this
#' departure into over-coverage and under-coverage components.
#'
#' Central PICP evaluates the **joint coverage** of lower and upper
#' prediction-interval bounds. It therefore does not identify how non-coverage is
#' distributed between the two tails. A model can have approximately correct
#' central interval coverage while having too many observations below one bound
#' and too few above the other. Use [gg_qcp()] or [gg_pit()] when tail-specific
#' or distributional calibration is also of interest.
#'
#' Calibration should also be distinguished from sharpness. Good coverage can be
#' obtained using unnecessarily wide prediction intervals, so PICP calibration
#' should generally be interpreted together with interval width or a proper
#' scoring rule such as [interval_score()].
#'
#' @param obs Numeric observation vector.
#' @param lower,upper Named lists of lower and upper prediction-interval bounds.
#'   Names must represent nominal coverage levels such as `"0.50"` or `"0.95"`.
#'   A single pair of numeric vectors is also accepted when `level` is supplied.
#' @param level Nominal central prediction-interval coverage for a single numeric
#'   `lower`/`upper` pair. It must be `NULL` when named lists are supplied.
#' @param pred,predictive_sd Optional numeric vectors of predictive means and
#'   predictive standard deviations. When supplied together, central prediction
#'   intervals are generated assuming normal predictive distributions. Do not
#'   also supply `lower` or `upper`.
#' @param levels Nominal central prediction-interval coverage probabilities used
#'   when `pred` and `predictive_sd` are supplied. Values must lie strictly
#'   between zero and one. Defaults to every percentage from 1% to 99%.
#' @param na.rm Logical; remove incomplete observation/interval combinations?
#'   With interval lists, only cases complete in `obs` and both bounds at every
#'   supplied level are used, so all levels share the same validation sample.
#'   If `FALSE`, any incomplete case makes coverage missing at every level.
#' @param point_size Positive numeric point size.
#' @param line_width Positive numeric width of the 1:1 reference line.
#'
#' @return A `ggplot2` object. Its plotting data contain:
#' \describe{
#'   \item{nominal}{Nominal prediction-interval coverage probability.}
#'   \item{picp}{Empirical prediction interval coverage probability.}
#' }
#'
#' @references
#' Deutsch, C. V. (1997). Direct assessment of local accuracy and precision.
#' In E. Y. Baafi and N. A. Schofield (Eds.), *Geostatistics Wollongong '96*,
#' pp. 115-125.
#'
#' Goovaerts, P. (2001). Geostatistical modelling of uncertainty in soil
#' science. *Geoderma*, 103, 3-26.
#' <doi:10.1016/S0016-7061(01)00067-2>
#'
#' Wadoux, A. M. J.-C., Brus, D. J. and Heuvelink, G. B. M. (2018).
#' Accounting for non-stationary variance in geostatistical mapping of soil
#' properties. *Geoderma*, 324, 138-147.
#'
#' Schmidinger, J. and Heuvelink, G. B. M. (2023). Validation of uncertainty
#' predictions in digital soil mapping. *Geoderma*, 437, 116585.
#' <doi:10.1016/j.geoderma.2023.116585>
#'
#' @seealso [picp()], [coverage_error()], [accuracy_plot_metrics()],
#'   [gg_qcp()], [gg_pit()], [interval_width()], [interval_score()]
#'
#' @examples
#' set.seed(123)
#'
#' n <- 200
#' pred <- seq(0, 10, length.out = n)
#' predictive_sd <- rep(1, n)
#' obs <- stats::rnorm(n, mean = pred, sd = predictive_sd)
#'
#' # Reliability curve generated directly from a normal predictive distribution
#' gg_coverage(
#'   obs,
#'   pred = pred,
#'   predictive_sd = predictive_sd
#' )
#'
#' # Selected prediction intervals can also be supplied directly
#' lower <- list(
#'   `0.50` = pred + stats::qnorm(0.25) * predictive_sd,
#'   `0.90` = pred + stats::qnorm(0.05) * predictive_sd
#' )
#'
#' upper <- list(
#'   `0.50` = pred + stats::qnorm(0.75) * predictive_sd,
#'   `0.90` = pred + stats::qnorm(0.95) * predictive_sd
#' )
#'
#' gg_coverage(
#'   obs,
#'   lower = lower,
#'   upper = upper
#' )
#'
#' @export
gg_coverage <- function(obs, lower = NULL, upper = NULL, level = NULL,
                        pred = NULL, predictive_sd = NULL, levels = NULL,
                        na.rm = TRUE, point_size = 3, line_width = 0.6) {

  check_flag(na.rm, "na.rm")
  check_number(point_size, "point_size", positive = TRUE)
  check_number(line_width, "line_width", positive = TRUE)

  intervals <- coverage_intervals(
    obs, lower, upper, level, na.rm,
    pred, predictive_sd, levels
  )

  p <- ggplot2::ggplot(
    intervals,
    ggplot2::aes(x = nominal, y = picp)
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey50",
      linewidth = line_width
    ) +
    ggplot2::geom_point(
      size = point_size,
      colour = "red3"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "Nominal prediction-interval coverage",
      y = "Empirical coverage (PICP)"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = 12,
        family = "sans"
      )
    )

  p
}

coverage_intervals <- function(obs, lower, upper, level, na.rm,
                               pred = NULL, predictive_sd = NULL,
                               levels = NULL) {

  check_flag(na.rm, "na.rm")
  normal_mode <- !is.null(pred) || !is.null(predictive_sd)

  if (normal_mode) {

    if (!is.null(lower) ||
        !is.null(upper) ||
        is.null(pred) ||
        is.null(predictive_sd)) {
      stop(
        "Normal-distribution mode requires `pred` and `predictive_sd` only.",
        call. = FALSE
      )
    }

    if (is.null(levels)) {
      levels <- seq(.01, .99, by = .01)
    }

    if (!is_numeric_vector(levels) ||
        !length(levels) ||
        any(!is.finite(levels)) ||
        any(levels <= 0 | levels >= 1)) {
      stop(
        "`levels` must contain finite nominal levels strictly between 0 and 1.",
        call. = FALSE
      )
    }

    x <- prepare_predictive_sd_vectors(
      obs,
      pred,
      predictive_sd,
      na.rm
    )

    if (is.null(x) || !length(x$obs)) {
      return(
        data.frame(
          nominal = sort(unique(levels)),
          picp = NA_real_
        )
      )
    }

    z <- abs(
      (x$obs - x$pred) /
        x$predictive_sd
    )

    return(
      data.frame(
        nominal = sort(unique(levels)),
        picp = vapply(
          sort(unique(levels)),
          function(level) {
            mean(
              z <= stats::qnorm((1 + level) / 2)
            )
          },
          numeric(1)
        )
      )
    )
  }

  list_mode <- is.list(lower) || is.list(upper)

  if (list_mode) {

    if (!is.list(lower) ||
        !is.list(upper) ||
        !length(lower) ||
        length(lower) != length(upper) ||
        is.null(names(lower)) ||
        is.null(names(upper)) ||
        any(!nzchar(names(lower))) ||
        anyDuplicated(names(lower)) ||
        !setequal(names(lower), names(upper))) {
      stop(
        "`lower` and `upper` must be equally sized named lists with the same nominal-level names.",
        call. = FALSE
      )
    }

    if (!is.null(level)) {
      stop(
        "`level` is only used with a single numeric interval pair.",
        call. = FALSE
      )
    }

    upper <- upper[names(lower)]

    nominal <- suppressWarnings(
      as.numeric(names(lower))
    )

    if (any(!is.finite(nominal)) ||
        any(nominal <= 0 | nominal >= 1)) {
      stop(
        "Interval-list names must be nominal levels strictly between 0 and 1, such as `0.95`.",
        call. = FALSE
      )
    }

    # Validate each interval before selecting a common validation sample.
    for (i in seq_along(lower)) {
      prepare_interval_vectors(obs, lower[[i]], upper[[i]], na.rm = TRUE)
    }
    keep <- stats::complete.cases(c(list(obs), unname(lower), unname(upper)))
    if ((!na.rm && !all(keep)) || !any(keep)) {
      picp_values <- rep(NA_real_, length(nominal))
    } else {
      picp_values <- vapply(seq_along(lower), function(i) {
        mean(obs[keep] >= lower[[i]][keep] & obs[keep] <= upper[[i]][keep])
      }, numeric(1))
    }

  } else {

    if (is.null(level)) {
      level <- 0.95
    }

    check_probability(level, "level")

    nominal <- level

    picp_values <- picp(
      obs,
      lower,
      upper,
      na.rm
    )
  }

  data.frame(
    nominal = nominal,
    picp = picp_values
  )
}
