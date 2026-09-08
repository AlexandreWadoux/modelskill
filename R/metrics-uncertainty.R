#' Prediction interval coverage probability (PICP)
#'
#' PICP is the empirical proportion of observations satisfying
#' `lower <= obs <= upper`; endpoints are included. It is returned on the
#' probability scale from zero to one, so multiply by 100 to report a percent.
#'
#' \deqn{\mathrm{PICP}(\tau) = \frac{1}{n}\sum_{i=1}^{n}
#' I(lower_i \leq obs_i \leq upper_i)}
#'
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
#'
#'   Shrestha, D. L. and Solomatine, D. P. (2008). Data-driven approaches for
#'   estimating uncertainty in rainfall-runoff modelling. *International Journal
#'   of River Basin Management*, 6, 109-122.
#'   <doi:10.1080/15715124.2008.9635341>
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
#' conventional abbreviation for prediction interval coverage probability. Its
#' equation, interpretation, and reference are given in [picp()].
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
#'
#' \deqn{\mathrm{PICP\ error}(\tau) = \mathrm{PICP}(\tau) - \tau}
#'
#' Zero is ideal. Positive values mean intervals cover too often (are too wide
#' or over-pessimistic); negative values mean intervals cover too rarely.
#' @inheritParams picp
#' @param level Nominal central interval coverage, strictly between zero and one.
#' @return One numeric value.
#' @references Schmidinger, J. and Heuvelink, G. B. M. (2023). Validation of
#'   uncertainty predictions in digital soil mapping. *Geoderma*, 437, 116585.
#'   <doi:10.1016/j.geoderma.2023.116585>
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
#'
#' \deqn{\mathrm{PIW}(\tau) = \frac{1}{n}\sum_{i=1}^{n}(upper_i-lower_i)}
#'
#' PIW has response units. Smaller values indicate sharper predictions, but are
#' desirable only when calibration is adequate; assess it alongside [picp()].
#' @inheritParams coverage
#' @return One numeric value.
#' @references Schmidinger, J. and Heuvelink, G. B. M. (2023). Validation of
#'   uncertainty predictions in digital soil mapping. *Geoderma*, 437, 116585.
#'   <doi:10.1016/j.geoderma.2023.116585>
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
#'
#' For the usual proper-scoring interpretation, `lower` and `upper` must be the
#' central (equal-tailed) predictive quantiles corresponding to `level`: the
#' predictive quantiles at probabilities `(1 - level) / 2` and
#' `(1 + level) / 2`, respectively. Supplying arbitrary interval bounds together
#' with a nominal `level` still evaluates the formula below, but does not in
#' general give the same proper interval-score interpretation.
#'
#' \deqn{\mathrm{IS}_\tau = \frac{1}{n}\sum_{i=1}^{n}[upper_i-lower_i+
#' \frac{2}{1-\tau}\max(lower_i-obs_i,0)+
#' \frac{2}{1-\tau}\max(obs_i-upper_i,0)]}
#'
#' The score has response units and lower values are better. It rewards narrow
#' intervals but penalizes observations outside them by their distance from the
#' nearest bound.
#' @param obs Numeric observation vector.
#' @param lower,upper Numeric bounds of the central prediction interval. For the
#'   usual proper-score interpretation, these must be the equal-tailed
#'   predictive quantiles corresponding to `level`.
#' @param level Nominal central interval coverage, strictly between zero and one;
#'   determines the required predictive quantiles for `lower` and `upper`.
#' @param na.rm Logical; remove incomplete triplets?
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
#' Quantile coverage probability (QCP) evaluates the calibration of individual
#' predictive quantiles. For a predicted quantile at nominal probability `p`,
#' QCP is the empirical proportion of observations that are less than or equal
#' to that predicted quantile.
#'
#' \deqn{
#' \mathrm{QCP}(p) =
#' \frac{1}{n}
#' \sum_{i=1}^{n}
#' I(obs_i \leq q_{i,p})
#' }
#'
#' For a calibrated predictive distribution, QCP should be close to the nominal
#' quantile probability `p`. For example, approximately 5% of observations
#' should fall below predicted 0.05 quantiles, approximately 50% below predicted
#' medians, and approximately 95% below predicted 0.95 quantiles.
#'
#' Values above `p` indicate that observations fall below the predicted quantile
#' more frequently than expected, whereas values below `p` indicate that they do
#' so less frequently than expected.
#'
#' QCP differs from prediction interval coverage probability ([picp()]).
#' PICP evaluates the joint coverage of a lower and upper prediction-interval
#' bound, whereas QCP evaluates each predictive quantile separately. QCP can
#' therefore reveal asymmetric or one-sided miscalibration that may be hidden by
#' apparently satisfactory central prediction-interval coverage.
#'
#' Quantile calibration is generally most informative when evaluated across
#' several probability levels rather than at a single quantile. A dense sequence
#' of quantiles provides a more complete view of distributional calibration,
#' although selected levels remain useful when only specific predictive
#' quantiles are available.
#'
#' @param obs Numeric observation vector.
#' @param quantiles Numeric matrix or data frame with observations in rows and
#'   predicted quantiles in columns.
#' @param levels Strictly increasing quantile probabilities, one per column of
#'   `quantiles`. Values must lie strictly between zero and one.
#' @param na.rm Logical; remove incomplete observation/quantile rows?
#'
#' @return Named numeric vector containing one QCP value for each supplied
#'   quantile level, on the probability scale from zero to one.
#'
#' @references
#' Schmidinger, J. and Heuvelink, G. B. M. (2023). Validation of uncertainty
#' predictions in digital soil mapping. *Geoderma*, 437, 116585.
#' <doi:10.1016/j.geoderma.2023.116585>
#'
#' @seealso [gg_qcp()], [picp()], [gg_coverage()], [pit()], [gg_pit()]
#'
#' @examples
#' set.seed(123)
#'
#' n <- 500
#' pred <- seq(0, 10, length.out = n)
#' predictive_sd <- rep(1, n)
#' obs <- stats::rnorm(n, mean = pred, sd = predictive_sd)
#'
#' levels <- seq(0.05, 0.95, by = 0.05)
#'
#' quantiles <- vapply(
#'   levels,
#'   function(p) pred + stats::qnorm(p) * predictive_sd,
#'   numeric(n)
#' )
#'
#' qcp(
#'   obs,
#'   quantiles = quantiles,
#'   levels = levels
#' )
#'
#' @export
qcp <- function(obs, quantiles, levels, na.rm = TRUE) {
  x <- prepare_quantiles(obs, quantiles, levels, na.rm)

  out <- rep(NA_real_, length(levels))

  if (!is.null(x) && length(x$obs)) {
    out <- colMeans(
      sweep(
        x$distribution,
        1,
        x$obs,
        FUN = ">="
      )
    )
  }

  stats::setNames(out, as.character(levels))
}

#' Probability integral transform
#'
#' Calculates probability integral transform (PIT) values for continuous
#' predictive distributions. PIT values can either be supplied directly as
#' predictive cumulative distribution function (CDF) values evaluated at the
#' observations, or calculated from predictive means and standard deviations
#' under a normal predictive-distribution assumption.
#'
#' For observation \eqn{obs_i} and predictive cumulative distribution function
#' \eqn{F_i}, the PIT value is
#'
#' \deqn{
#' u_i = F_i(obs_i).
#' }
#'
#' For calibrated continuous predictive distributions, PIT values evaluated
#' over independent validation observations should be approximately uniformly
#' distributed between zero and one. Individual PIT values do not have a
#' preferred value; calibration is assessed from their distribution across
#' observations, for example with [gg_pit()].
#'
#' When `cdf_at_obs` is supplied, the values are returned after validation.
#' This mode can be used with any continuous predictive distribution provided
#' its CDF has already been evaluated at each corresponding observation.
#'
#' Alternatively, `obs`, `pred`, and `predictive_sd` can be supplied together.
#' In this case, normal predictive distributions are assumed and PIT values are
#' calculated as
#'
#' \deqn{
#' u_i =
#' \Phi\left(
#' \frac{obs_i-pred_i}{\sigma_i}
#' \right),
#' }
#'
#' where \eqn{\Phi} is the standard normal CDF and \eqn{\sigma_i} is the
#' predictive standard deviation.
#'
#' A uniform PIT distribution is consistent with probabilistic calibration.
#' Systematic departures from uniformity can indicate misspecification of the
#' predictive distributions. For example, U-shaped PIT histograms are commonly
#' associated with underdispersed predictive distributions, hump-shaped
#' histograms with overdispersed distributions, and asymmetric PIT histograms
#' with systematic bias. These patterns are diagnostic rather than unique and
#' should be interpreted together with other validation measures.
#'
#' PIT uniformity assesses the predictive distributions collectively and does
#' not by itself establish that every aspect of conditional calibration is
#' correct. Calibration should therefore generally be evaluated using
#' complementary diagnostics such as [gg_qcp()], [gg_coverage()], and proper
#' scoring rules.
#'
#' The usual uniformity interpretation applies directly to continuous
#' predictive distributions. For discrete distributions or finite predictive
#' ensembles, ordinary PIT values are discrete; randomized PIT or rank-based
#' diagnostics are more appropriate.
#'
#' @param cdf_at_obs Optional numeric vector containing predictive CDF values
#'   evaluated at the corresponding observations. Values must lie between zero
#'   and one. Do not supply this together with `obs`, `pred`, or
#'   `predictive_sd`.
#' @param obs Optional numeric vector of observations. Must be supplied together
#'   with `pred` and `predictive_sd`.
#' @param pred Optional numeric vector of predictive means. Used with `obs` and
#'   `predictive_sd` to calculate PIT values assuming normal predictive
#'   distributions.
#' @param predictive_sd Optional numeric vector of predictive standard
#'   deviations. Values must be finite and strictly positive.
#' @param na.rm Logical; remove incomplete values or observation-prediction
#'   combinations? The default is `TRUE`.
#'
#' @return Numeric vector of PIT values between zero and one.
#'
#' @references
#' Gneiting, T., Balabdaoui, F. and Raftery, A. E. (2007). Probabilistic
#' forecasts, calibration and sharpness. *Journal of the Royal Statistical
#' Society: Series B*, 69, 243-268.
#' <doi:10.1111/j.1467-9868.2007.00587.x>
#'
#' Schmidinger, J. and Heuvelink, G. B. M. (2023). Validation of uncertainty
#' predictions in digital soil mapping. *Geoderma*, 437, 116585.
#' <doi:10.1016/j.geoderma.2023.116585>
#'
#' @seealso [gg_pit()], [qcp()], [gg_qcp()], [picp()], [gg_coverage()],
#'   [crps()]
#'
#' @examples
#' # PIT values from CDF values calculated elsewhere
#' cdf_values <- stats::pnorm(c(-1, 0, 1))
#' pit(cdf_values)
#'
#' # Calculate PIT directly for normal predictive distributions
#' set.seed(123)
#'
#' n <- 500
#' pred <- seq(0, 10, length.out = n)
#' predictive_sd <- rep(1, n)
#' obs <- stats::rnorm(n, mean = pred, sd = predictive_sd)
#'
#' pit_values <- pit(
#'   obs = obs,
#'   pred = pred,
#'   predictive_sd = predictive_sd
#' )
#'
#' gg_pit(pit_values)
#'
#' @export
pit <- function(cdf_at_obs = NULL,
                obs = NULL,
                pred = NULL,
                predictive_sd = NULL,
                na.rm = TRUE) {

  check_flag(na.rm, "na.rm")

  cdf_mode <- !is.null(cdf_at_obs)
  normal_mode <- !is.null(obs) ||
    !is.null(pred) ||
    !is.null(predictive_sd)

  if (cdf_mode && normal_mode) {
    stop(
      paste0(
        "Supply either `cdf_at_obs`, or `obs`, `pred`, and ",
        "`predictive_sd`, not both."
      ),
      call. = FALSE
    )
  }

  if (!cdf_mode && !normal_mode) {
    stop(
      paste0(
        "Supply either `cdf_at_obs`, or `obs`, `pred`, and ",
        "`predictive_sd`."
      ),
      call. = FALSE
    )
  }

  # Already evaluated predictive CDF values
  if (cdf_mode) {

    if (!is_numeric_vector(cdf_at_obs) || !length(cdf_at_obs)) {
      stop(
        "`cdf_at_obs` must be a non-empty numeric vector.",
        call. = FALSE
      )
    }

    invalid <- !is.na(cdf_at_obs) &
      (
        !is.finite(cdf_at_obs) |
          cdf_at_obs < 0 |
          cdf_at_obs > 1
      )

    if (any(invalid)) {
      stop(
        "`cdf_at_obs` values must be finite and between zero and one.",
        call. = FALSE
      )
    }

    if (na.rm) {
      return(cdf_at_obs[!is.na(cdf_at_obs)])
    }

    return(cdf_at_obs)
  }

  # Normal predictive-distribution mode
  if (is.null(obs) ||
      is.null(pred) ||
      is.null(predictive_sd)) {
    stop(
      paste0(
        "Normal-distribution mode requires `obs`, `pred`, and ",
        "`predictive_sd`."
      ),
      call. = FALSE
    )
  }

  x <- prepare_predictive_sd_vectors(
    obs,
    pred,
    predictive_sd,
    na.rm = na.rm
  )

  if (is.null(x)) {
    return(rep(NA_real_, length(obs)))
  }

  if (!length(x$obs)) {
    return(numeric())
  }

  z <- (x$obs - x$pred) / x$predictive_sd

  stats::pnorm(z)
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
#' as equally likely ensemble members.
#'
#' \deqn{\mathrm{CRPS} = \mathrm{RELI} + \mathrm{potential\ CRPS}}
#'
#' `reliability` (RELI) is non-negative and zero is ideal; smaller values
#' indicate better distributional calibration. `potential_crps` is the
#' remainder after removing reliability error. The decomposition is applicable
#' here only to equally weighted predictive samples.
#'
#' For each retained case \eqn{i=1,\ldots,n}, let
#' \eqn{x_{i,1}\leq\cdots\leq x_{i,m}} be the sorted ensemble members and
#' \eqn{p_j=j/m}. For interior bins \eqn{j=1,\ldots,m-1}, define
#' \deqn{\alpha_{i,j}=\max\{0,\min(obs_i,x_{i,j+1})-x_{i,j}\},
#' \qquad \beta_{i,j}=\max\{0,x_{i,j+1}-\max(obs_i,x_{i,j})\}.}
#' These are the portions of a bin below and above the observation. Equality
#' at a bin edge assigns the full width to the appropriate portion; tied
#' ensemble members have zero width. With bars denoting means over cases,
#' \deqn{g_j=\bar{\alpha}_j+\bar{\beta}_j,\qquad
#' o_j=\frac{\bar{\beta}_j}{g_j}.}
#' If \eqn{g_j=0}, set \eqn{o_j=p_j}; this bin contributes zero.
#' The two exterior bins use
#' \deqn{o_0=\frac{1}{n}\sum_{i=1}^n I(obs_i\leq x_{i,1}),\qquad
#' o_m=\frac{1}{n}\sum_{i=1}^n I(obs_i\leq x_{i,m}),}
#' \deqn{g_0=\frac{\frac{1}{n}\sum_{i=1}^n\max(x_{i,1}-obs_i,0)}{o_0},
#' \qquad g_m=\frac{\frac{1}{n}\sum_{i=1}^n\max(obs_i-x_{i,m},0)}{1-o_m}.}
#' Set \eqn{g_0=0} when \eqn{o_0=0} and \eqn{g_m=0} when \eqn{o_m=1}.
#' The components, in response units, are then
#' \deqn{\mathrm{RELI}=\sum_{j=0}^m g_j(o_j-p_j)^2,\qquad
#' \mathrm{potential\ CRPS}=\sum_{j=0}^m g_j o_j(1-o_j).}
#' @inheritParams crps
#' @return One-row data frame with `crps`, `reliability`, and `potential_crps`.
#' @references Hersbach, H. (2000). Decomposition of the continuous ranked
#'   probability score for ensemble prediction systems. *Weather and
#'   Forecasting*, 15, 559-570. <doi:10.1175/1520-0434(2000)015%3C0559:DOTCRP%3E2.0.CO;2>
#' @examples crps_decomposition(c(0, 1), matrix(c(-1, 1, 0, 2), nrow = 2))
#' @export
crps_decomposition <- function(obs, distribution, na.rm = TRUE) {
  check_flag(na.rm, "na.rm")
  x <- prepare_distribution_matrix(obs, distribution, na.rm, min_columns = 2L)
  if (is.null(x) || !length(x$obs)) {
    return(data.frame(crps = NA_real_, reliability = NA_real_, potential_crps = NA_real_))
  }
  ordered <- x$distribution
  for (row in seq_len(nrow(ordered))) ordered[row, ] <- sort(ordered[row, ])
  size <- ncol(ordered)
  left <- ordered[, -size, drop = FALSE]
  right <- ordered[, -1L, drop = FALSE]

  # Integrate the two constant indicator values within each CDF step.
  # Clipping the observation to the bin handles edge ties without case branches.
  mean_below <- colMeans(pmax(pmin(right, x$obs) - left, 0))
  mean_above <- colMeans(pmax(right - pmax(left, x$obs), 0))
  mean_width <- mean_below + mean_above
  probability <- seq_len(size - 1L) / size
  active <- mean_width > 0
  frequency <- mean_above[active] / mean_width[active]

  # The exterior terms simplify algebraically, avoiding division by a tail
  # frequency that may be zero. Retain the documented inclusive CDF convention.
  tail_distance <- c(mean(pmax(ordered[, 1L] - x$obs, 0)),
                     mean(pmax(x$obs - ordered[, size], 0)))
  tail_frequency <- c(mean(x$obs <= ordered[, 1L]),
                      mean(x$obs > ordered[, size]))
  reliability <- sum(mean_width[active] * (frequency - probability[active])^2) +
    sum(tail_distance * tail_frequency)
  potential <- sum(mean_below[active] * frequency) +
    sum(tail_distance * (1 - tail_frequency))

  # Compute the score independently from its decomposition identity.
  score <- sum(mean_below * probability^2 + mean_above * (1 - probability)^2) +
    sum(tail_distance)
  data.frame(crps = score, reliability = reliability, potential_crps = potential)
}

#' Continuous ranked probability score
#'
#' CRPS compares a predictive distribution with an observation; lower values
#' are better. Supply either equally weighted predictive samples in
#' `distribution`, or a normal predictive distribution through `pred` and
#' `predictive_sd`.
#'
#' \deqn{\mathrm{CRPS}(F, obs) = \int_{-\infty}^{\infty}
#' [F(z)-I(z\geq obs)]^2\,dz}
#'
#' CRPS has response units and lower values are better; zero is ideal. It is a
#' proper scoring rule that jointly rewards calibrated and sharp distributions.
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
#'
#' \deqn{\mathrm{median\ CRPS} = \mathrm{median}(\mathrm{CRPS}_i)}
#'
#' It has response units; lower values are better. Unlike mean CRPS, it
#' describes a typical case and is less sensitive to a small number of very poor
#' predictive distributions. Median aggregation is a robust descriptive summary
#' but should not replace mean CRPS for formal comparisons based on proper
#' scoring rules.
#' @inheritParams crps
#' @return One numeric median CRPS value.
#' @references Hersbach, H. (2000). Decomposition of the continuous ranked
#'   probability score for ensemble prediction systems. *Weather and
#'   Forecasting*, 15, 559-570.
#'   <doi:10.1175/1520-0434(2000)015%3C0559:DOTCRP%3E2.0.CO;2>
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
#'
#' \deqn{\mathrm{Log\ score} = -\frac{1}{n}\sum_{i=1}^{n}\log f_i(obs_i)}
#'
#' Lower values are better. The score strongly penalizes assigning near-zero
#' density to observations, so it is useful for comparing full predictive
#' distributions but can be dominated by tail failures.
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
