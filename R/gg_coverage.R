#' Plot prediction-interval coverage calibration
#'
#' Compares empirical coverage with nominal central prediction-interval
#' coverage. Points on the dashed 1:1 line are calibrated; points below it
#' under-cover observations. The result is an ordinary ggplot object.
#'
#' @param obs Numeric observation vector.
#' @param lower,upper Named lists of interval-bound vectors. Names must be
#'   nominal levels such as `"0.50"` or `"0.95"`. A single pair of numeric
#'   vectors is also accepted when `level` is supplied.
#' @param level Nominal level for a single numeric lower/upper pair. It must be
#'   `NULL` when named lists are supplied.
#' @param na.rm Logical; remove incomplete interval triplets?
#' @param point_size Positive numeric point size.
#' @param line_width Positive numeric reference-line width.
#' @return A `ggplot2` object with `nominal` and `empirical` data columns.
#' @examples
#' obs <- c(1, 2, 3)
#' gg_coverage(obs, lower = list(`0.8` = c(0, 1, 2)),
#'             upper = list(`0.8` = c(2, 3, 4)))
#' @export
gg_coverage <- function(obs, lower, upper, level = NULL, na.rm = TRUE,
                        point_size = 3, line_width = 0.6) {
  check_flag(na.rm, "na.rm")
  check_number(point_size, "point_size", positive = TRUE)
  check_number(line_width, "line_width", positive = TRUE)
  intervals <- coverage_intervals(obs, lower, upper, level, na.rm)
  p <- ggplot2::ggplot(intervals, ggplot2::aes(x = nominal, y = empirical)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         colour = "grey50", linewidth = line_width) +
    ggplot2::geom_point(size = point_size, colour = "red3") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = intervals$nominal,
      labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Nominal coverage", y = "Empirical coverage") +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 12, family = "sans"))
  p
}

coverage_intervals <- function(obs, lower, upper, level, na.rm) {
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
    empirical <- vapply(seq_along(lower), function(i) coverage(obs, lower[[i]], upper[[i]], na.rm), numeric(1))
  } else {
    if (is.null(level)) level <- 0.95
    check_probability(level, "level")
    nominal <- level
    empirical <- coverage(obs, lower, upper, na.rm)
  }
  data.frame(nominal = nominal, empirical = empirical)
}
