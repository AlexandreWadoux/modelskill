#' Prediction interval coverage probability (PICP)
#'
#' PICP is the empirical proportion of observations satisfying
#' `lower <= obs <= upper`; endpoints are included. It is returned on the
#' probability scale from zero to one, so multiply by 100 to report a percent.
#' For a well-calibrated central interval, `picp()` should be close to its
#' nominal level. Missing triplets are removed when `na.rm = TRUE`.
#'
#' A single PICP does not reveal whether non-coverage is balanced between the
#' lower and upper tails. Use [gg_coverage()] to inspect PICP across interval
#' levels; assess tail-specific calibration separately when directional bias is
#' scientifically important.
#' @param obs Numeric observation vector.
#' @param lower,upper Numeric lower and upper prediction-interval bounds.
#' @param na.rm Logical; remove incomplete triplets?
#' @return One numeric value.
#' @references Goovaerts, P. (2001). Geostatistical modelling of uncertainty in
#'   soil science. *Geoderma*, 103, 3-26. <doi:10.1016/S0016-7061(01)00067-2>
#' @examples picp(1:3, c(0, 1, 2), c(2, 3, 4))
#' @export
picp <- function(obs, lower, upper, na.rm = TRUE) {
  x <- prepare_interval_vectors(obs, lower, upper, na.rm)
  if (is.null(x) || !length(x$obs)) return(NA_real_)
  mean(x$obs >= x$lower & x$obs <= x$upper)
}

#' Empirical prediction-interval coverage
#'
#' Backward-compatible alias for [picp()]. New code should prefer `picp()`, the
#' conventional abbreviation for prediction interval coverage probability.
#' @inheritParams picp
#' @return One numeric value on the probability scale from zero to one.
#' @examples coverage(1:3, c(0, 1, 2), c(2, 3, 4))
#' @export
coverage <- function(obs, lower, upper, na.rm = TRUE) {
  picp(obs, lower, upper, na.rm = na.rm)
}

#' Prediction-interval coverage error
#'
#' Empirical [picp()] minus nominal coverage `level`. Positive values mean
#' over-coverage and negative values mean under-coverage. The result is on the
#' probability scale; multiply by 100 for percentage points.
#' @inheritParams picp
#' @param level Nominal central interval coverage, strictly between zero and one.
#' @return One numeric value.
#' @examples coverage_error(1:3, c(0, 1, 2), c(2, 3, 4), level = .8)
#' @export
coverage_error <- function(obs, lower, upper, level = 0.95, na.rm = TRUE) {
  check_probability(level, "level")
  picp(obs, lower, upper, na.rm = na.rm) - level
}

#' Average prediction-interval width
#'
#' Arithmetic mean of `upper - lower`, i.e. PIW(tau) = sum(upper - lower) / n
#' for a tau-level prediction interval. Smaller widths are sharper, but should
#' always be interpreted jointly with empirical coverage. PIW is independent of
#' observed values: `obs` is retained only to check input length compatibility.
#' Central intervals have lower and upper predictive quantiles at
#' `(1 - tau) / 2` and `(1 + tau) / 2`, respectively.
#' @inheritParams coverage
#' @return One numeric value.
#' @examples interval_width(1:3, c(0, 1, 2), c(2, 3, 4))
#' @export
interval_width <- function(obs, lower, upper, na.rm = TRUE) {
  # Keep `obs` in the API while ensuring PIW itself is independent of test data.
  prepare_metric_vectors(obs = obs, lower = lower, upper = upper, na.rm = FALSE)
  x <- prepare_interval_bounds(lower, upper, na.rm)
  if (is.null(x) || !length(x$lower)) return(NA_real_)
  mean(x$upper - x$lower)
}

#' Central prediction-interval score
#'
#' The interval score is `upper - lower + 2 / alpha * (lower - obs)` below the
#' interval and `upper - lower + 2 / alpha * (obs - upper)` above it, where
#' `alpha = 1 - level`; there is no penalty inside the interval. Lower scores
#' indicate sharper, well-calibrated intervals.
#' @inheritParams coverage
#' @param level Nominal central interval coverage, strictly between zero and one.
#' @return One numeric value.
#' @references Gneiting, T. and Raftery, A. E. (2007). Strictly proper scoring
#'   rules, prediction, and estimation. *Journal of the American Statistical
#'   Association*, 102, 359-378.
#' @examples interval_score(1:3, c(0, 1, 2), c(2, 3, 4), level = .8)
#' @export
interval_score <- function(obs, lower, upper, level = 0.95, na.rm = TRUE) {
  check_probability(level, "level")
  x <- prepare_interval_vectors(obs, lower, upper, na.rm)
  if (is.null(x) || !length(x$obs)) return(NA_real_)
  alpha <- 1 - level
  width <- x$upper - x$lower
  mean(width + 2 / alpha * pmax(x$lower - x$obs, 0) +
         2 / alpha * pmax(x$obs - x$upper, 0))
}

#' Standardized prediction errors
#'
#' Returns `(obs - pred) / predictive_sd` for each complete triplet. The term
#' *predictive standard deviation* refers to the uncertainty supplied for each
#' prediction, not the sample standard deviation of the prediction vector.
#' @param obs Numeric observation vector.
#' @param pred Numeric predictive-mean vector.
#' @param predictive_sd Numeric predictive standard deviation vector; values
#'   must be finite and strictly positive.
#' @param na.rm Logical; remove incomplete triplets? If `FALSE` and a triplet
#'   is incomplete, a vector of `NA` values is returned.
#' @return Numeric vector of standardized errors.
#' @examples standardized_error(1:3, c(1, 2, 4), c(1, 1, 2))
#' @export
standardized_error <- function(obs, pred, predictive_sd, na.rm = TRUE) {
  x <- prepare_predictive_sd_vectors(obs, pred, predictive_sd, na.rm)
  if (is.null(x)) return(rep(NA_real_, length(obs)))
  (x$obs - x$pred) / x$predictive_sd
}

#' Mean standardized prediction error
#'
#' Mean of [standardized_error()]. An ideally calibrated unbiased predictive
#' distribution has a value near zero.
#' @inheritParams standardized_error
#' @return One numeric value.
#' @examples standardized_error_mean(1:3, c(1, 2, 4), c(1, 1, 2))
#' @export
standardized_error_mean <- function(obs, pred, predictive_sd, na.rm = TRUE) {
  z <- standardized_error(obs, pred, predictive_sd, na.rm)
  if (!length(z) || anyNA(z)) return(NA_real_)
  mean(z)
}

#' Standard deviation of standardized prediction errors
#'
#' Sample standard deviation of [standardized_error()]. One is ideal. Values
#' above one indicate predictive uncertainty is generally too small; values
#' below one indicate it is generally too large.
#' @inheritParams standardized_error
#' @return One numeric value.
#' @examples standardized_error_sd(1:3, c(1, 2, 4), c(1, 1, 2))
#' @export
standardized_error_sd <- function(obs, pred, predictive_sd, na.rm = TRUE) {
  z <- standardized_error(obs, pred, predictive_sd, na.rm)
  if (length(z) < 2L || anyNA(z)) return(NA_real_)
  stats::sd(z)
}

#' Proportion within predictive standard deviations
#'
#' Proportion satisfying `abs(obs - pred) <= k * predictive_sd`. For a normal,
#' calibrated predictive distribution, approximately 68% and 95% fall within
#' one and 1.96 predictive standard deviations, respectively.
#' @inheritParams standardized_error
#' @param k Positive number of predictive standard deviations.
#' @return One numeric value.
#' @examples within_sd(1:3, c(1, 2, 4), c(1, 1, 2), k = 1)
#' @export
within_sd <- function(obs, pred, predictive_sd, k = 1, na.rm = TRUE) {
  check_number(k, "k", positive = TRUE)
  z <- standardized_error(obs, pred, predictive_sd, na.rm)
  if (!length(z) || anyNA(z)) return(NA_real_)
  mean(abs(z) <= k)
}
