#' Bias of quantitative predictions
#'
#' Mean signed error, calculated as observation minus prediction. Negative
#' values indicate overprediction and zero is ideal. Missing pairs are removed
#' when `na.rm = TRUE`; otherwise the result is `NA` when any pair is missing.
#'
#' @param obs Numeric observation vector.
#' @param pred Numeric prediction vector paired with `obs`.
#' @param na.rm Logical; remove incomplete pairs?
#' @return One numeric value.
#' @examples bias(c(1, 2, 3), c(1, 3, 2))
#' @export
bias <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "bias")

#' Mean absolute error
#'
#' Mean absolute observation-minus-prediction error. It has the input units;
#' zero is ideal. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @examples mae(1:3, c(1, 3, 2))
#' @export
mae <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "mae")

#' Mean squared error
#'
#' Mean squared observation-minus-prediction error. Lower values are better.
#' @inheritParams bias
#' @return One numeric value.
#' @examples mse(1:3, c(1, 3, 2))
#' @export
mse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "mse")

#' Root mean squared error
#'
#' Square root of [mse()], in the input units. Lower values are better.
#' @inheritParams bias
#' @return One numeric value.
#' @examples rmse(1:3, c(1, 3, 2))
#' @export
rmse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "rmse")

#' Normalized root mean squared error
#'
#' RMSE divided by the sample standard deviation of observations. This is the
#' normalization used by the package's diagram statistics; lower values are
#' better. It is undefined for constant observations.
#' @inheritParams bias
#' @return One numeric value.
#' @examples nrmse(1:3, c(1, 3, 2))
#' @export
nrmse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "nrmse")

#' Centered root mean squared error
#'
#' Root mean square error after subtracting the mean error. It measures pattern
#' and scale disagreement independently of constant bias; lower is better.
#' @inheritParams bias
#' @return One numeric value.
#' @examples crmse(1:3, c(1, 3, 2))
#' @export
crmse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "crmse")

#' Pearson correlation
#'
#' Pearson product-moment correlation between observations and predictions.
#' It ranges from -1 to 1 and is undefined for a constant vector.
#' @inheritParams bias
#' @return One numeric value.
#' @examples correlation(1:3, c(1, 3, 2))
#' @export
correlation <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "correlation")

#' Squared Pearson correlation
#'
#' Square of [correlation()]. It measures linear association, not agreement in
#' level or scale, and is undefined when correlation is undefined.
#' @inheritParams bias
#' @return One numeric value.
#' @examples r2(1:3, c(1, 3, 2))
#' @export
r2 <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "r2")

#' Nash-Sutcliffe efficiency
#'
#' One minus the ratio of prediction squared error to the observation sum of
#' squares about its mean. One is ideal, zero equals predicting the observation
#' mean, and negative values are worse. It is undefined for constant observations.
#' @inheritParams bias
#' @return One numeric value.
#' @examples nse(1:3, c(1, 3, 2))
#' @export
nse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "nse")

#' Prediction-to-observation standard deviation ratio
#'
#' Sample standard deviation of predictions divided by sample standard deviation
#' of observations. One indicates equal spread; it is undefined when observation
#' spread is zero.
#' @inheritParams bias
#' @return One numeric value.
#' @examples sd_ratio(1:3, c(1, 3, 2))
#' @export
sd_ratio <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "sd_ratio")

#' Lin's concordance correlation coefficient
#'
#' Lin's CCC combines Pearson correlation with agreement in location and scale.
#' It ranges from -1 to 1 and equals one for perfect agreement. Population
#' variances (divisor n) are used, matching the existing package convention.
#' @inheritParams bias
#' @return One numeric value.
#' @examples ccc(1:3, c(1, 3, 2))
#' @export
ccc <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "ccc")

metric_value <- function(obs, pred, na.rm, name) {
  values <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(values)) return(NA_real_)
  metric_components(values$obs, values$pred)[[name]]
}

# Shared calculations for individual metrics and model_metrics().
metric_components <- function(obs, pred) {
  empty <- stats::setNames(rep(NA_real_, 13),
    c("bias", "mae", "mse", "rmse", "nrmse", "crmse", "correlation",
      "r2", "nse", "sd_ratio", "ccc", "cb", "n"))
  n <- length(obs)
  if (!n) return(empty)
  m <- pair_moments(pred, obs)
  error <- m$o - m$p
  result <- empty
  result["n"] <- n
  result["bias"] <- mean(error) * m$scale
  result["mae"] <- mean(abs(error)) * m$scale
  result["rmse"] <- root_mean_square(error) * m$scale
  result["mse"] <- result["rmse"]^2
  result["crmse"] <- root_mean_square(error - mean(error)) * m$scale
  if (n < 2L || m$so == 0) return(result)
  sample_obs_sd <- m$so * m$scale * sqrt(n / (n - 1))
  result["nrmse"] <- result["rmse"] / sample_obs_sd
  result["sd_ratio"] <- m$sp / m$so
  result["nse"] <- 1 - (root_mean_square(error) / m$so)^2
  if (m$sp > 0) {
    result["correlation"] <- m$r
    result["r2"] <- m$r^2
  }
  shift <- mean(m$p) - mean(m$o)
  denom_scale <- max(m$sp, m$so, abs(shift))
  sp <- m$sp / denom_scale
  so <- m$so / denom_scale
  result["cb"] <- 2 * sp * so / (sp^2 + so^2 + (shift / denom_scale)^2)
  result["ccc"] <- if (m$sp > 0) m$r * result["cb"] else 0
  result
}
