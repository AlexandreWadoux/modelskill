#' Summarize predictive uncertainty validation
#'
#' Calculates prediction-interval diagnostics. Supply both `lower` and `upper`
#' for a central prediction interval. For quantile and full-distribution
#' diagnostics, see [qcp()], [pit()], [crps()], and [crps_decomposition()].
#'
#' @param obs Numeric observation vector.
#' @param lower,upper Numeric prediction-interval bounds, supplied together.
#' @param level Nominal central interval coverage in interval mode.
#' @param na.rm Logical; remove incomplete paired inputs?
#' @return A one-row base data frame. Interval mode returns `picp`,
#'   `picp_error`, `interval_width`, and `interval_score`.
#' @examples
#' uncertainty_metrics(1:3, lower = c(0, 1, 2), upper = c(2, 3, 4), level = .8)
#' @export
uncertainty_metrics <- function(obs, lower = NULL, upper = NULL,
                                level = 0.95, na.rm = TRUE) {
  if (is.null(lower) || is.null(upper)) {
    stop("Prediction-interval validation requires both `lower` and `upper`.", call. = FALSE)
  }
  check_probability(level, "level")
  data.frame(picp = picp(obs, lower, upper, na.rm),
    picp_error = coverage_error(obs, lower, upper, level, na.rm),
    interval_width = interval_width(obs, lower, upper, na.rm),
    interval_score = interval_score(obs, lower, upper, level, na.rm))
}
