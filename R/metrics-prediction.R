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

#' Model efficiency coefficient
#'
#' Alias for [nse()]. MEC, NSE, and the uppercase R-squared efficiency `R2()`
#' are the same statistic. They must not be confused with lowercase `r2()`,
#' the squared Pearson correlation.
#' @inheritParams bias
#' @return One numeric value.
#' @examples mec(1:3, c(1, 3, 2))
#' @export
mec <- function(obs, pred, na.rm = TRUE) nse(obs, pred, na.rm)

#' Coefficient of determination / efficiency R-squared
#'
#' Alias for [nse()] and `mec()`. This uppercase `R2()` is the model-efficiency
#' coefficient; lowercase `r2()` remains squared Pearson correlation.
#' @inheritParams bias
#' @return One numeric value.
#' @examples R2(1:3, c(1, 3, 2))
#' @export
R2 <- function(obs, pred, na.rm = TRUE) nse(obs, pred, na.rm)

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

extended_components <- function(obs, pred, na.rm = TRUE) {
  x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(x) || !length(x$obs)) return(stats::setNames(rep(NA_real_, 10), c("mdae", "rpd", "rpiq", "sep", "rer", "mape", "smape", "msle", "rmsle", "rae")))
  error <- x$obs - x$pred; root <- sqrt(mean(error^2)); n <- length(error)
  out <- c(mdae = stats::median(abs(error)),
    rpd = if (n < 2 || root == 0) NA_real_ else stats::sd(x$obs) / root,
    rpiq = if (root == 0) NA_real_ else stats::IQR(x$obs) / root,
    sep = if (n < 2) NA_real_ else sqrt(sum((error - mean(error))^2) / (n - 1)),
    rer = if (root == 0) NA_real_ else diff(range(x$obs)) / root,
    mape = if (any(x$obs == 0)) NA_real_ else mean(abs(error / x$obs)),
    smape = mean(ifelse(x$obs == 0 & x$pred == 0, 0, 2 * abs(error) / (abs(x$obs) + abs(x$pred))), na.rm = TRUE),
    msle = if (any(x$obs < 0 | x$pred < 0)) NA_real_ else mean((log1p(x$obs) - log1p(x$pred))^2),
    rmsle = NA_real_,
    rae = if (sum(abs(x$obs - mean(x$obs))) == 0) NA_real_ else sum(abs(error)) / sum(abs(x$obs - mean(x$obs))))
  out["rmsle"] <- sqrt(out["msle"])
  out
}

#' Extended continuous-prediction metrics
#'
#' These metrics complement the core error and agreement statistics. MAPE is
#' undefined for zero observations; log metrics require non-negative values.
#' @inheritParams bias
#' @return One numeric value.
#' @name extended_prediction_metrics
NULL

#' @rdname extended_prediction_metrics
#' @export
mdae <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["mdae"])
#' @rdname extended_prediction_metrics
#' @export
rpd <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rpd"])
#' @rdname extended_prediction_metrics
#' @export
rpiq <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rpiq"])
#' @rdname extended_prediction_metrics
#' @export
sep <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["sep"])
#' @rdname extended_prediction_metrics
#' @export
rer <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rer"])
#' @rdname extended_prediction_metrics
#' @export
mape <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["mape"])
#' @rdname extended_prediction_metrics
#' @export
smape <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["smape"])
#' @rdname extended_prediction_metrics
#' @export
msle <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["msle"])
#' @rdname extended_prediction_metrics
#' @export
rmsle <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rmsle"])
#' @rdname extended_prediction_metrics
#' @export
rae <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rae"])

#' Quantile (pinball) loss
#' @inheritParams bias
#' @param level Quantile level strictly between zero and one.
#' @return One numeric loss; lower is better.
#' @export
pinball_loss <- function(obs, pred, level = .5, na.rm = TRUE) {
  check_probability(level, "level"); x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(x)) return(NA_real_); error <- x$obs - x$pred
  mean(ifelse(error >= 0, level * error, (level - 1) * error))
}

#' Kling-Gupta efficiency
#' @inheritParams bias
#' @return One numeric value; one is ideal.
#' @export
kge <- function(obs, pred, na.rm = TRUE) {
  x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(x) || length(x$obs) < 2 || mean(x$obs) == 0 || stats::sd(x$obs) == 0 || stats::sd(x$pred) == 0) return(NA_real_)
  1 - sqrt((stats::cor(x$obs, x$pred) - 1)^2 + (stats::sd(x$pred) / stats::sd(x$obs) - 1)^2 + (mean(x$pred) / mean(x$obs) - 1)^2)
}

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
