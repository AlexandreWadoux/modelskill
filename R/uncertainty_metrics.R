#' Summarize predictive uncertainty validation
#'
#' Calculates uncertainty diagnostics in one unambiguous mode. Supply either
#' `lower` and `upper` for a central prediction interval, or `pred` and
#' `predictive_sd` for predictive means and predictive standard deviations.
#' Interval mode assesses calibration and sharpness; predictive-SD mode assesses
#' standardized-error calibration.
#'
#' @param obs Numeric observation vector.
#' @param lower,upper Numeric prediction-interval bounds, supplied together.
#' @param pred Numeric predictive-mean vector.
#' @param predictive_sd Numeric predictive standard deviation vector.
#' @param level Nominal central interval coverage in interval mode.
#' @param na.rm Logical; remove incomplete paired inputs?
#' @return A one-row base data frame. Interval mode returns `picp`,
#'   `picp_error`, `interval_width`, and `interval_score`. Predictive-SD
#'   mode returns `standardized_error_mean`, `standardized_error_sd`,
#'   `within_1sd`, and `within_1.96sd`.
#' @examples
#' uncertainty_metrics(1:3, lower = c(0, 1, 2), upper = c(2, 3, 4), level = .8)
#' uncertainty_metrics(1:3, pred = c(1, 2, 4), predictive_sd = c(1, 1, 2))
#' @export
uncertainty_metrics <- function(obs, lower = NULL, upper = NULL, pred = NULL,
                                predictive_sd = NULL, level = 0.95,
                                na.rm = TRUE) {
  interval_mode <- !is.null(lower) || !is.null(upper)
  sd_mode <- !is.null(pred) || !is.null(predictive_sd)
  if (interval_mode && sd_mode) {
    stop("Supply either interval inputs (`lower` and `upper`) or predictive-SD inputs (`pred` and `predictive_sd`), not both.", call. = FALSE)
  }
  if (interval_mode) {
    if (is.null(lower) || is.null(upper)) {
      stop("Interval mode requires both `lower` and `upper`.", call. = FALSE)
    }
    check_probability(level, "level")
    return(data.frame(picp = picp(obs, lower, upper, na.rm),
      picp_error = coverage_error(obs, lower, upper, level, na.rm),
      interval_width = interval_width(obs, lower, upper, na.rm),
      interval_score = interval_score(obs, lower, upper, level, na.rm)))
  }
  if (sd_mode) {
    if (is.null(pred) || is.null(predictive_sd)) {
      stop("Predictive-SD mode requires both `pred` and `predictive_sd`.", call. = FALSE)
    }
    return(data.frame(standardized_error_mean = standardized_error_mean(obs, pred, predictive_sd, na.rm),
      standardized_error_sd = standardized_error_sd(obs, pred, predictive_sd, na.rm),
      within_1sd = within_sd(obs, pred, predictive_sd, k = 1, na.rm = na.rm),
      within_1.96sd = within_sd(obs, pred, predictive_sd, k = 1.96, na.rm = na.rm)))
  }
  stop("Supply either `lower` and `upper`, or `pred` and `predictive_sd`.", call. = FALSE)
}
