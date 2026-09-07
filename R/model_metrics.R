#' Calculate prediction-validation metrics for one or more models
#'
#' Applies the individual prediction metrics to one or more prediction vectors.
#' Canonical lower-case columns are returned together with legacy upper-case
#' columns retained for compatibility with earlier versions of `modelskill`.
#'
#' Errors are observation minus prediction. See [bias()], [crmse()], [nse()],
#' and [ccc()] for definitions and interpretation.
#'
#' @param mods Numeric vector, list of numeric vectors, or numeric matrix/data
#'   frame with one model per column. Rows match `obs` in order. Supplied model
#'   names must be unique; missing names are generated.
#' @param obs A numeric observation vector.
#' @param na.rm Logical; whether incomplete observation-prediction pairs should
#'   be removed. The default is `TRUE`. If `FALSE`, incomplete pairs result in
#'   missing statistics rather than being silently removed.
#' @param extended Logical; include extended error metrics used in spectroscopy
#'   and quantile-regression workflows?
#' @param digits Integer or `NULL`. If supplied, round numeric results to this
#'   many digits (0 to 22). `NULL` retains full numerical precision.
#'
#' @details Errors are observation minus prediction: negative ME indicates
#' overprediction. ME, MAE and RMSE have the input units. NSE is one minus the
#' ratio of squared error to the observation sum of squares about its mean.
#' Zero is the observation-mean benchmark and negative values are worse.
#' Concordance uses population variances (divisor n); rhoC = r * Cb.
#' Missing pairs are removed separately for each model, so comparisons may use
#' different subsets. NA and NaN are missing; infinite values are rejected.
#' Repeated observations are retained with equal weight.
#' With fewer than two pairs, correlation, efficiency and concordance are NA.
#' Constant inputs return NA for r and r2 because Pearson correlation is
#' undefined. Constant observations give NA efficiency
#' and concordance. Constant predictions with varying observations give zero
#' Cb and rhoC (the continuous limiting value). All-missing models return NA.
#' @seealso [diagram_stats()], [gg_taylor()], [gg_solar()], [gg_target()]
#' @return A base data frame with one row per model and canonical columns
#'   `model`, `bias`, `mae`, `mse`, `rmse`, `nrmse`, `crmse`, `correlation`,
#'   `r2`, `nse`, `sd_ratio`, and `ccc`. Legacy columns `ME`, `MAE`, `RMSE`,
#'   `r`, `NSE`, `rhoC`, and `Cb` are also retained.
#'
#' @references
#' Wadoux, A. M. J.-C., Walvoort, D. J. J., and Brus, D. J. (2022). An
#' integrated approach for the evaluation of quantitative soil maps through
#' Taylor and solar diagrams. *Geoderma*, 405, 115332.
#' <doi:10.1016/j.geoderma.2021.115332>
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#' preds <- list(model_a = c(1, 2, 3, 4, 5),
#'               model_b = c(2, 2, 3, 4, 4))
#' model_metrics(preds, obs)
#' @export
model_metrics <- function(mods, obs, na.rm = TRUE, extended = FALSE, digits = NULL) {
  mods <- prepare_models(mods, obs, na.rm)
  check_flag(extended, "extended")
  if (!is.null(digits)) {
    check_number(digits, "digits")
    if (digits < 0 || digits != floor(digits) || digits > 22) {
      stop("`digits` must be NULL or one non-negative integer from 0 to 22.", call. = FALSE)
    }
  }
  rows <- lapply(mods, function(pred) {
    values <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
    if (is.null(values)) {
      stats::setNames(rep(NA_real_, 12), c("bias", "mae", "mse", "rmse",
        "nrmse", "crmse", "correlation", "r2", "nse", "sd_ratio", "ccc", "cb"))
    } else {
      metric_components(values$obs, values$pred)[c("bias", "mae", "mse", "rmse",
        "nrmse", "crmse", "correlation", "r2", "nse", "sd_ratio", "ccc", "cb")]
    }
  })
  canonical <- as.data.frame(do.call(rbind, rows))
  result <- data.frame(model = names(mods), canonical[, setdiff(names(canonical), "cb"), drop = FALSE],
    ME = canonical$bias, MAE = canonical$mae, RMSE = canonical$rmse,
    r = canonical$correlation, NSE = canonical$nse, rhoC = canonical$ccc,
    Cb = canonical$cb, check.names = FALSE)
  result$MEC <- result$nse
  result$R2 <- result$nse
  if (extended) {
    extra <- do.call(rbind, lapply(mods, function(pred) extended_components(obs, pred, na.rm)))
    result <- cbind(result, as.data.frame(extra))
  }
  rownames(result) <- names(mods)
  numeric <- vapply(result, is.numeric, logical(1))
  if (any(is.infinite(as.matrix(result[, numeric, drop = FALSE])))) {
    warning("Some metrics exceed numeric precision; consider rescaling the input units.", call. = FALSE)
  }
  if (!is.null(digits)) result[numeric] <- lapply(result[numeric], round, digits = digits)
  result
}
