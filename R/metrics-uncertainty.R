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

#' Quantile coverage probability
#'
#' Calculates QCP at each supplied quantile level: the empirical proportion of
#' observations less than or equal to the predicted quantile. A calibrated
#' predictive distribution has QCP close to the nominal quantile level. Unlike
#' PICP, QCP can reveal one-sided quantile bias.
#' @param obs Numeric observation vector.
#' @param quantiles Numeric matrix or data frame with observations in rows and
#'   predicted quantiles in columns.
#' @param levels Strictly increasing quantile probabilities, one per column.
#' @param na.rm Logical; remove incomplete observation/quantile rows?
#' @return Named numeric vector of QCP values on the probability scale.
#' @references Schmidinger, J. and Heuvelink, G. B. M. (2023). Validation of
#'   uncertainty predictions in digital soil mapping. *Geoderma*, 437, 116585.
#'   <doi:10.1016/j.geoderma.2023.116585>
#' @examples
#' qcp(1:3, cbind(c(0, 1, 2), c(1, 2, 3)), c(.25, .75))
#' @export
qcp <- function(obs, quantiles, levels, na.rm = TRUE) {
  x <- prepare_quantiles(obs, quantiles, levels, na.rm)
  out <- rep(NA_real_, length(levels))
  if (!is.null(x) && length(x$obs)) {
    out <- colMeans(sweep(x$distribution, 1, x$obs, FUN = ">="))
  }
  stats::setNames(out, as.character(levels))
}

#' Probability integral transform values
#'
#' Validates already evaluated predictive-CDF values, `cdf_at_obs = F_i(y_i)`.
#' For calibrated continuous predictive distributions, PIT values are uniform
#' on zero to one. Plot them with [gg_pit()].
#' @param cdf_at_obs Numeric vector of predictive CDF values evaluated at the
#'   corresponding observations.
#' @param na.rm Logical; remove missing values?
#' @return Numeric PIT values.
#' @references Gneiting, T., Balabdaoui, F. and Raftery, A. E. (2007).
#'   Probabilistic forecasts, calibration and sharpness. *JRSS B*, 69, 243-268.
#'   <doi:10.1111/j.1467-9868.2007.00587.x>
#' @examples pit(stats::pnorm(c(-1, 0, 1)))
#' @export
pit <- function(cdf_at_obs, na.rm = TRUE) {
  if (!is_numeric_vector(cdf_at_obs) || any(!is.na(cdf_at_obs) &
      (!is.finite(cdf_at_obs) | cdf_at_obs < 0 | cdf_at_obs > 1))) {
    stop("`cdf_at_obs` must be a numeric vector with values between zero and one.", call. = FALSE)
  }
  if (!na.rm && anyNA(cdf_at_obs)) return(rep(NA_real_, length(cdf_at_obs)))
  cdf_at_obs[!is.na(cdf_at_obs)]
}

crps_casewise <- function(obs, distribution = NULL, pred = NULL,
                          predictive_sd = NULL, na.rm = TRUE) {
  has_distribution <- !is.null(distribution)
  has_normal <- !is.null(pred) || !is.null(predictive_sd)
  if (has_distribution == has_normal) {
    stop("Supply either `distribution`, or both `pred` and `predictive_sd`.", call. = FALSE)
  }
  if (has_normal) {
    if (is.null(pred) || is.null(predictive_sd)) {
      stop("Normal-distribution mode requires both `pred` and `predictive_sd`.", call. = FALSE)
    }
    x <- prepare_predictive_sd_vectors(obs, pred, predictive_sd, na.rm)
    if (is.null(x)) return(numeric())
    z <- (x$obs - x$pred) / x$predictive_sd
    return(x$predictive_sd * (z * (2 * stats::pnorm(z) - 1) +
      2 * stats::dnorm(z) - 1 / sqrt(pi)))
  }
  x <- prepare_distribution_matrix(obs, distribution, na.rm)
  if (is.null(x)) return(numeric())
  vapply(seq_len(nrow(x$distribution)), function(i) {
    draws <- sort(x$distribution[i, ])
    members <- length(draws)
    first_term <- mean(abs(draws - x$obs[i]))
    second_term <- sum((2 * seq_len(members) - members - 1) * draws) / members^2
    first_term - second_term
  }, numeric(1))
}

#' CRPS reliability decomposition
#'
#' Decomposes mean ensemble CRPS into a reliability component and potential
#' CRPS following Hersbach (2000). Predictive-distribution columns are treated
#' as equally likely ensemble members; lower `reliability` is better.
#' @inheritParams crps
#' @return One-row data frame with `crps`, `reliability`, and `potential_crps`.
#' @references Hersbach, H. (2000). Decomposition of the continuous ranked
#'   probability score for ensemble prediction systems. *Weather and
#'   Forecasting*, 15, 559-570. <doi:10.1175/1520-0434(2000)015%3C0559:DOTCRP%3E2.0.CO;2>
#' @examples crps_decomposition(c(0, 1), matrix(c(-1, 1, 0, 2), nrow = 2))
#' @export
crps_decomposition <- function(obs, distribution, na.rm = TRUE) {
  x <- prepare_distribution_matrix(obs, distribution, na.rm, min_columns = 2L)
  if (is.null(x) || !length(x$obs)) {
    return(data.frame(crps = NA_real_, reliability = NA_real_, potential_crps = NA_real_))
  }
  ensemble <- t(apply(x$distribution, 1, sort))
  members <- ncol(ensemble)
  alpha <- beta <- matrix(0, nrow(ensemble), members + 1L)
  h0 <- as.numeric(x$obs <= ensemble[, 1])
  hn <- as.numeric(x$obs <= ensemble[, members])
  below <- x$obs < ensemble[, 1]
  above <- x$obs > ensemble[, members]
  beta[below, 1] <- ensemble[below, 1] - x$obs[below]
  alpha[above, members + 1L] <- x$obs[above] - ensemble[above, members]
  for (j in seq_len(members - 1L)) {
    width <- ensemble[, j + 1L] - ensemble[, j]
    alpha[x$obs > ensemble[, j + 1L], j + 1L] <- width[x$obs > ensemble[, j + 1L]]
    beta[x$obs < ensemble[, j], j + 1L] <- width[x$obs < ensemble[, j]]
    inside <- x$obs > ensemble[, j] & x$obs < ensemble[, j + 1L]
    alpha[inside, j + 1L] <- x$obs[inside] - ensemble[inside, j]
    beta[inside, j + 1L] <- ensemble[inside, j + 1L] - x$obs[inside]
  }
  reliability <- potential <- 0
  for (j in 0:members) {
    index <- j + 1L
    if (j == 0L) {
      oi <- mean(h0); gi <- if (oi == 0) 0 else mean(beta[, index]) / oi
    } else if (j == members) {
      oi <- mean(hn); gi <- if (oi == 1) 0 else mean(alpha[, index]) / (1 - oi)
    } else {
      a <- mean(alpha[, index]); b <- mean(beta[, index])
      gi <- a + b
      oi <- if (gi == 0) j / members else b / gi
    }
    probability <- j / members
    reliability <- reliability + gi * (oi - probability)^2
    potential <- potential + gi * oi * (1 - oi)
  }
  data.frame(crps = reliability + potential, reliability = reliability,
             potential_crps = potential)
}

#' Continuous ranked probability score
#'
#' CRPS compares a predictive distribution with an observation; lower values
#' are better. Supply either equally weighted predictive samples in
#' `distribution`, or a normal predictive distribution through `pred` and
#' `predictive_sd`.
#' @param obs Numeric observation vector.
#' @param distribution Numeric matrix/data frame of equally weighted predictive
#'   samples, one row per observation.
#' @param pred,predictive_sd Mean and strictly positive predictive SD for normal
#'   predictive distributions.
#' @param na.rm Logical; remove incomplete observation/distribution rows?
#' @return One numeric mean CRPS value.
#' @references Hersbach, H. (2000). Decomposition of the continuous ranked
#'   probability score for ensemble prediction systems. *Weather and
#'   Forecasting*, 15, 559-570. <doi:10.1175/1520-0434(2000)015%3C0559:DOTCRP%3E2.0.CO;2>
#' @examples crps(0, distribution = matrix(c(-1, 1), nrow = 1))
#' @export
crps <- function(obs, distribution = NULL, pred = NULL, predictive_sd = NULL,
                 na.rm = TRUE) {
  values <- crps_casewise(obs, distribution, pred, predictive_sd, na.rm)
  if (!length(values)) return(NA_real_)
  mean(values)
}

#' Median continuous ranked probability score
#'
#' Median of case-wise CRPS values. Lower values are better. It is a robust
#' descriptive summary when a few large errors dominate mean CRPS.
#' @inheritParams crps
#' @return One numeric median CRPS value.
#' @examples median_crps(0, distribution = matrix(c(-1, 1), nrow = 1))
#' @export
median_crps <- function(obs, distribution = NULL, pred = NULL,
                        predictive_sd = NULL, na.rm = TRUE) {
  values <- crps_casewise(obs, distribution, pred, predictive_sd, na.rm)
  if (!length(values)) return(NA_real_)
  stats::median(values)
}

#' Logarithmic (ignorance) score
#'
#' Returns the mean negative log predictive density at the observations. Lower
#' values are better. This score requires positive predictive densities and is
#' particularly sensitive to observations assigned very low density.
#' @param obs Numeric observation vector, retained for length checking.
#' @param density_at_obs Numeric vector of strictly positive predictive-density
#'   values evaluated at each corresponding observation.
#' @param na.rm Logical; remove incomplete pairs?
#' @return One numeric score.
#' @references Gneiting, T. and Raftery, A. E. (2007). Strictly proper scoring
#'   rules, prediction, and estimation. *JASA*, 102, 359-378.
#'   <doi:10.1198/016214506000001437>
#' @examples log_score(0, stats::dnorm(0))
#' @export
log_score <- function(obs, density_at_obs, na.rm = TRUE) {
  x <- prepare_metric_vectors(obs = obs, density_at_obs = density_at_obs, na.rm = na.rm)
  if (is.null(x) || !length(x$obs)) return(NA_real_)
  if (any(x$density_at_obs <= 0)) {
    stop("`density_at_obs` must be strictly positive for every complete pair.", call. = FALSE)
  }
  mean(-log(x$density_at_obs))
}
