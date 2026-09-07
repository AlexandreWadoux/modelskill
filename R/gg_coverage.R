#' Plot PICP calibration (reliability or accuracy plot)
#'
#' Compares prediction interval coverage probability (PICP) at multiple nominal
#' central prediction-interval levels. Points on the dashed 1:1 line are
#' calibrated; points below it under-cover observations. This is a PICP
#' reliability plot, also called an *accuracy plot* in geostatistical
#' uncertainty validation (Goovaerts, 2001). It is therefore an extension of a
#' single PICP to multiple interval levels. The result is an ordinary ggplot
#' object.
#'
#' Central PICP curves assess overall interval reliability, but cannot by
#' themselves identify asymmetric lower- versus upper-tail non-coverage caused
#' by a one-sided bias.
#'
#' @param obs Numeric observation vector.
#' @param lower,upper Named lists of interval-bound vectors. Names must be
#'   nominal levels such as `"0.50"` or `"0.95"`. A single pair of numeric
#'   vectors is also accepted when `level` is supplied.
#' @param level Nominal level for a single numeric lower/upper pair. It must be
#'   `NULL` when named lists are supplied.
#' @param pred,predictive_sd Optional predictive means and predictive standard
#'   deviations. When supplied together, normal central intervals are generated
#'   for `levels`; do not also supply `lower` or `upper`.
#' @param levels Nominal levels for normal predictive distributions. Defaults to
#'   every percentage from 1% to 99%.
#' @param na.rm Logical; remove incomplete interval triplets?
#' @param point_size Positive numeric point size.
#' @param line_width Positive numeric reference-line width.
#' @return A `ggplot2` object with `nominal` and `picp` data columns.
#' @references Goovaerts, P. (2001). Geostatistical modelling of uncertainty in
#'   soil science. *Geoderma*, 103, 3-26. <doi:10.1016/S0016-7061(01)00067-2>
#' @examples
#' obs <- c(1, 2, 3)
#' gg_coverage(obs, lower = list(`0.8` = c(0, 1, 2)),
#'             upper = list(`0.8` = c(2, 3, 4)))
#' @export
gg_coverage <- function(obs, lower = NULL, upper = NULL, level = NULL,
                        pred = NULL, predictive_sd = NULL, levels = NULL,
                        na.rm = TRUE, point_size = 3, line_width = 0.6) {
  check_flag(na.rm, "na.rm")
  check_number(point_size, "point_size", positive = TRUE)
  check_number(line_width, "line_width", positive = TRUE)
  intervals <- coverage_intervals(obs, lower, upper, level, na.rm,
                                  pred, predictive_sd, levels)
  p <- ggplot2::ggplot(intervals, ggplot2::aes(x = nominal, y = picp)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = "grey50", linewidth = line_width) +
    ggplot2::geom_point(size = point_size, colour = "red3") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .1),
      labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Nominal prediction-interval coverage", y = "PICP") +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 12, family = "sans"))
  p
}

coverage_intervals <- function(obs, lower, upper, level, na.rm,
                               pred = NULL, predictive_sd = NULL,
                               levels = NULL) {
  normal_mode <- !is.null(pred) || !is.null(predictive_sd)
  if (normal_mode) {
    if (!is.null(lower) || !is.null(upper) || is.null(pred) || is.null(predictive_sd)) {
      stop("Normal-distribution mode requires `pred` and `predictive_sd` only.", call. = FALSE)
    }
    if (is.null(levels)) levels <- seq(.01, .99, by = .01)
    if (!is_numeric_vector(levels) || !length(levels) || any(!is.finite(levels)) ||
        any(levels <= 0 | levels >= 1)) {
      stop("`levels` must contain finite nominal levels strictly between 0 and 1.", call. = FALSE)
    }
    x <- prepare_predictive_sd_vectors(obs, pred, predictive_sd, na.rm)
    if (is.null(x)) return(data.frame(nominal = levels, picp = NA_real_))
    z <- abs((x$obs - x$pred) / x$predictive_sd)
    return(data.frame(nominal = sort(unique(levels)), picp = vapply(sort(unique(levels)),
      function(level) mean(z <= stats::qnorm((1 + level) / 2)), numeric(1))))
  }
  list_mode <- is.list(lower) || is.list(upper)
  if (list_mode) {
    if (!is.list(lower) || !is.list(upper) || !length(lower) || length(lower) != length(upper) ||
        is.null(names(lower)) || is.null(names(upper)) || any(!nzchar(names(lower))) ||
        anyDuplicated(names(lower)) || !setequal(names(lower), names(upper))) {
      stop("`lower` and `upper` must be equally sized named lists with the same nominal-level names.", call. = FALSE)
    }
    if (!is.null(level)) stop("`level` is only used with a single numeric interval pair.", call. = FALSE)
    upper <- upper[names(lower)]
    nominal <- suppressWarnings(as.numeric(names(lower)))
    if (any(!is.finite(nominal)) || any(nominal <= 0 | nominal >= 1)) {
      stop("Interval-list names must be nominal levels strictly between 0 and 1, such as `0.95`.", call. = FALSE)
    }
    picp_values <- vapply(seq_along(lower), function(i) picp(obs, lower[[i]], upper[[i]], na.rm), numeric(1))
  } else {
    if (is.null(level)) level <- 0.95
    check_probability(level, "level")
    nominal <- level
    picp_values <- picp(obs, lower, upper, na.rm)
  }
  data.frame(nominal = nominal, picp = picp_values)
}
