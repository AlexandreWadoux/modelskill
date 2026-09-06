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
#' @param mods A numeric prediction vector, or a named list of numeric
#'   prediction vectors.
#' @param obs A numeric observation vector.
#' @param na.rm Logical; whether incomplete observation-prediction pairs should
#'   be removed. The default is `TRUE`. If `FALSE`, incomplete pairs result in
#'   missing statistics rather than being silently removed.
#' @param digits Integer or `NULL`. If supplied, round numeric results to this
#'   many digits. `NULL` (the default) retains full numerical precision.
#'
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
  validate_metrics_inputs(mods, obs, na.rm, digits)
  if (is.numeric(mods)) {
    mods <- list(Model = mods)
  }
  if (is.null(names(mods)) || any(names(mods) == "")) {
    names(mods) <- paste0("Model ", seq_along(mods))
  }

  result <- do.call(rbind, lapply(mods, metric_row, obs = obs, na.rm = na.rm))
  rownames(result) <- names(mods)
  if (!is.null(digits)) result[] <- lapply(result, round, digits = digits)
  result
}

metric_row <- function(pred, obs, na.rm) {
  if (na.rm) {
    keep <- stats::complete.cases(pred, obs)
    pred <- pred[keep]
    obs <- obs[keep]
  }
  if (!na.rm && anyNA(c(pred, obs))) {
    return(as.data.frame(as.list(setNames(rep(NA_real_, 8),
                                          c("ME", "MAE", "RMSE", "r", "r2",
                                            "NSE", "rhoC", "Cb")))))
  }
  if (!length(pred)) {
    return(as.data.frame(as.list(setNames(rep(NA_real_, 8),
                                          c("ME", "MAE", "RMSE", "r", "r2",
                                            "NSE", "rhoC", "Cb")))))
  }

  error <- obs - pred
  r <- suppressWarnings(stats::cor(pred, obs, method = "pearson"))
  if (is.na(r)) r <- 0
  sdx <- stats::sd(pred)
  sdy <- stats::sd(obs)
  sse <- sum(error^2)
  sst <- sum((obs - mean(obs))^2)

  if (isTRUE(sdy > 0)) {
    sx2 <- stats::var(pred) * (length(pred) - 1) / length(pred)
    sy2 <- stats::var(obs) * (length(obs) - 1) / length(obs)
    v <- sdx / sdy
    u <- (mean(pred) - mean(obs)) / ((sx2 * sy2)^0.25)
    cb <- ((v + 1 / v + u^2) / 2)^-1
    rho_c <- r * cb
    nse <- 1 - sse / sst
  } else {
    cb <- rho_c <- nse <- NA_real_
  }

  data.frame(
    ME = mean(error), MAE = mean(abs(error)), RMSE = sqrt(mean(error^2)),
    r = r, r2 = r^2, NSE = nse, rhoC = rho_c, Cb = cb,
    check.names = FALSE
  )
}

validate_metrics_inputs <- function(mods, obs, na.rm, digits) {
  if (!is.numeric(obs)) stop("`obs` must be a numeric vector.", call. = FALSE)
  if (!(is.numeric(mods) || is.list(mods))) {
    stop("`mods` must be a numeric vector or a list of numeric vectors.", call. = FALSE)
  }
  if (is.list(mods) && !all(vapply(mods, is.numeric, logical(1)))) {
    stop("Every element of `mods` must be numeric.", call. = FALSE)
  }
  if (is.numeric(mods) && length(mods) != length(obs)) {
    stop("A prediction vector and `obs` must have the same length.", call. = FALSE)
  }
  if (is.list(mods) && any(vapply(mods, length, integer(1)) != length(obs))) {
    stop("Every prediction vector in `mods` must have the same length as `obs`.", call. = FALSE)
  }
  if (length(na.rm) != 1L || is.na(na.rm)) stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(digits) && (length(digits) != 1L || is.na(digits) || digits < 0 || digits != as.integer(digits))) {
    stop("`digits` must be NULL or one non-negative integer.", call. = FALSE)
  }
  invisible(NULL)
}
