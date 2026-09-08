#' Summarize prediction-interval validation
#'
#' Calculates a set of complementary diagnostics for evaluating central
#' prediction intervals. The function combines empirical coverage, departure
#' from nominal coverage, interval width, and the interval score in a single
#' one-row summary.
#'
#' Both `lower` and `upper` prediction-interval bounds must be supplied. For a
#' nominal interval level \eqn{p}, a well-calibrated uncertainty model should
#' have empirical prediction interval coverage probability (PICP) close to
#' \eqn{p}.
#'
#' The returned diagnostics describe complementary aspects of prediction-
#' interval performance:
#'
#' \itemize{
#'   \item `picp` is the proportion of observations contained within the
#'   prediction intervals;
#'   \item `picp_error` is empirical minus nominal coverage. Values below zero
#'   indicate under-coverage, whereas values above zero indicate over-coverage;
#'   \item `interval_width` is the mean width of the prediction intervals and
#'   summarizes their sharpness. Smaller values indicate narrower intervals,
#'   but are desirable only when calibration remains adequate;
#'   \item `interval_score` is a proper scoring rule that rewards narrow
#'   intervals while penalizing observations falling below or above them.
#'   Smaller values indicate better performance when comparing predictions on
#'   the same response scale and at the same nominal interval level.
#' }
#'
#' These quantities should be interpreted together. Coverage alone does not
#' reward sharp predictions, while interval width alone does not assess whether
#' the intervals contain observations at the stated frequency. The interval
#' score combines both aspects in a single statistic.
#'
#' This function is intentionally restricted to prediction intervals. For
#' calibration across multiple interval levels, use [gg_coverage()] and
#' [accuracy_plot_metrics()]. For predictive quantiles, use [qcp()] and
#' [gg_qcp()]. For complete predictive distributions, use [pit()], [gg_pit()],
#' [crps()], or [crps_decomposition()].
#'
#' All four returned statistics use the same complete observation/lower/upper
#' triplets. This differs intentionally from standalone [interval_width()],
#' which is observation-independent and can use interval bounds where `obs` is
#' missing.
#'
#' @param obs Numeric observation vector.
#' @param lower,upper Numeric vectors containing the lower and upper
#'   prediction-interval bounds. Both must be supplied and have the same length
#'   as `obs`.
#' @param level Nominal central prediction-interval coverage probability,
#'   strictly between zero and one. The default is `0.95`.
#' @param na.rm Logical; remove incomplete observation/interval combinations?
#'   The default is `TRUE`.
#'
#' @return A one-row base data frame containing:
#' \describe{
#'   \item{picp}{Empirical prediction interval coverage probability.}
#'   \item{picp_error}{Difference between empirical and nominal coverage,
#'   calculated as `picp - level`.}
#'   \item{interval_width}{Mean prediction-interval width.}
#'   \item{interval_score}{Mean interval score at the specified nominal
#'   coverage level.}
#' }
#'
#' @references
#' Gneiting, T. and Raftery, A. E. (2007). Strictly proper scoring rules,
#' prediction, and estimation. *Journal of the American Statistical
#' Association*, 102, 359-378.
#' <doi:10.1198/016214506000001437>
#'
#' Schmidinger, J. and Heuvelink, G. B. M. (2023). Validation of uncertainty
#' predictions in digital soil mapping. *Geoderma*, 437, 116585.
#' <doi:10.1016/j.geoderma.2023.116585>
#'
#' @seealso [picp()], [coverage_error()], [interval_width()],
#'   [interval_score()], [gg_coverage()], [accuracy_plot_metrics()],
#'   [qcp()], [pit()], [crps()]
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#' lower <- c(0, 1, 2, 3, 4)
#' upper <- c(2, 3, 4, 5, 6)
#'
#' uncertainty_metrics(
#'   obs,
#'   lower = lower,
#'   upper = upper,
#'   level = 0.80
#' )
#'
#' @export
uncertainty_metrics <- function(obs,
                                lower = NULL,
                                upper = NULL,
                                level = 0.95,
                                na.rm = TRUE) {

  check_flag(na.rm, "na.rm")

  if (is.null(lower) || is.null(upper)) {
    stop(
      "Prediction-interval validation requires both `lower` and `upper`.",
      call. = FALSE
    )
  }

  check_probability(level, "level")

  x <- prepare_interval_vectors(obs, lower, upper, na.rm)

  if (is.null(x) || !length(x$obs)) {
    return(data.frame(
      picp = NA_real_,
      picp_error = NA_real_,
      interval_width = NA_real_,
      interval_score = NA_real_
    ))
  }

  data.frame(
    picp = picp(
      x$obs,
      x$lower,
      x$upper,
      na.rm = FALSE
    ),
    picp_error = coverage_error(
      x$obs,
      x$lower,
      x$upper,
      level,
      na.rm = FALSE
    ),
    interval_width = interval_width(
      x$obs,
      x$lower,
      x$upper,
      na.rm = FALSE
    ),
    interval_score = interval_score(
      x$obs,
      x$lower,
      x$upper,
      level,
      na.rm = FALSE
    )
  )
}
