#' Calculate quantitative model-skill indices
#'
#' Calculates error, correlation, efficiency, and concordance statistics for
#' one or more prediction vectors against a common observation vector. Missing
#' values are removed by complete pairs, so every statistic is calculated from
#' the same locations.
#'
#' The definitions follow Wadoux, Walvoort, and Brus (2022), including mean
#' error (ME), mean absolute error (MAE), root mean square error (RMSE),
#' Pearson correlation (`r`), modelling efficiency coefficient (`NSE`), and
#' Lin's concordance correlation components (`rhoC` and `Cb`).
#'
#' @param mods Numeric vector, list of numeric vectors, or numeric matrix/data
#'   frame with one model per column. Rows match `obs` in order. Supplied model
#'   names must be unique; missing names are generated.
#' @param obs A numeric observation vector.
#' @param na.rm Logical; whether incomplete observation-prediction pairs should
#'   be removed. The default is `TRUE`. If `FALSE`, incomplete pairs result in
#'   missing statistics rather than being silently removed.
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
#' @return A data frame with one row per model and columns `ME`, `MAE`, `RMSE`,
#'   `r`, `r2`, `NSE`, `rhoC`, and `Cb`.
#'
#' @references
#' Wadoux, A. M. J.-C., Walvoort, D. J. J., and Brus, D. J. (2022). An
#' integrated approach for the evaluation of quantitative soil maps through
#' Taylor and solar diagrams. *Geoderma*, 405, 115332.
#' <https://doi.org/10.1016/j.geoderma.2021.115332>
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#' preds <- list(model_a = c(1, 2, 3, 4, 5),
#'               model_b = c(2, 2, 3, 4, 4))
#' model_metrics(preds, obs)
#' @export
model_metrics <- function(mods, obs, na.rm = TRUE, digits = NULL) {
  mods <- prepare_models(mods, obs, na.rm)
  if (!is.null(digits)) {
    check_number(digits, "digits")
    if (digits < 0 || digits != floor(digits) || digits > 22) {
      stop("`digits` must be NULL or one non-negative integer from 0 to 22.", call. = FALSE)
    }
  }
  result <- do.call(rbind, lapply(mods, metric_row, obs = obs, na.rm = na.rm))
  rownames(result) <- names(mods)
  if (any(is.infinite(as.matrix(result)))) {
    warning("Some metrics exceed numeric precision; consider rescaling the input units.", call. = FALSE)
  }
  if (!is.null(digits)) result[] <- lapply(result, round, digits = digits)
  result
}

metric_row <- function(pred, obs, na.rm) {
  pair <- paired_values(pred, obs, na.rm)
  pred <- pair$pred
  obs <- pair$obs
  result <- as.data.frame(as.list(stats::setNames(rep(NA_real_, 8),
    c("ME", "MAE", "RMSE", "r", "r2", "NSE", "rhoC", "Cb"))))
  if (!length(pred) || anyNA(c(pred, obs))) return(result)
  m <- pair_moments(pred, obs)
  error <- m$o - m$p
  result$ME <- mean(error) * m$scale
  result$MAE <- mean(abs(error)) * m$scale
  result$RMSE <- root_mean_square(error) * m$scale
  if (length(pred) < 2L) return(result)
  if (m$sp > 0 && m$so > 0) {
    result$r <- m$r
    result$r2 <- m$r^2
  }
  if (m$so > 0) {
    result$NSE <- 1 - (root_mean_square(error) / m$so)^2
    # Lin's expression, including its continuous limit for constant predictions.
    shift <- mean(m$p) - mean(m$o)
    denom_scale <- max(m$sp, m$so, abs(shift))
    sp <- m$sp / denom_scale
    so <- m$so / denom_scale
    result$Cb <- 2 * sp * so / (sp^2 + so^2 + (shift / denom_scale)^2)
    result$rhoC <- m$r * result$Cb
  }
  result
}
