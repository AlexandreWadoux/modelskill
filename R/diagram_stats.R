#' Calculate the statistics underlying model-skill diagrams
#'
#' Computes a shared numerical representation for Taylor, solar and target
#' diagrams without constructing a plot.
#' @inheritParams model_metrics
#' @param na.rm Logical; remove incomplete pairs separately for each model?
#'   If FALSE, missing pairs cause an error because coordinates cannot be
#'   plotted. The default is TRUE.
#' @return A data frame with one row per model: `model` (identifier), `n`
#'   (complete pair count), `r` (correlation), `sd_ratio` (prediction SD
#'   divided by observation SD), `mean_error` (observation minus prediction,
#'   in original units), `nME` (mean error divided by sample observation SD),
#'   `sde` (sample error SD divided by sample observation SD), and
#'   `signed_sde` (sde times the sign of the SD difference).
#' @details At least two complete pairs and non-zero observation standard
#' deviation are required for every model. Inputs are paired by position.
#' Constant predictions have r = NA. Their SD ratio is zero and sde is one.
#' Plot functions can therefore draw them without reporting a defined
#' correlation. Correlation-coloured points are grey when r is NA.
#'
#' Let `s_p` and `s_o` denote sample standard deviations of predictions and
#' observations, respectively. The standard-deviation ratio is
#' \deqn{\sigma^* = \frac{s_p}{s_o}.}
#' Mean error is calculated with divisor `n`, and its diagram coordinate is
#' \deqn{\mathrm{nME} = \frac{\overline{obs-pred}}{s_o}
#' \sqrt{\frac{n-1}{n}}.}
#' The standardized centred error is evaluated directly from centred errors to
#' avoid cancellation near perfect agreement:
#' \deqn{\mathrm{SDE}^* = \frac{\sqrt{n^{-1}\sum_{i=1}^n
#' [(obs_i-pred_i)-\overline{obs-pred}]^2}}{s_o}.}
#' Thus, with sample SDs (divisor `n - 1`),
#' \deqn{\mathrm{SDE}^* = \sqrt{\frac{n-1}{n}
#' [1 + \sigma^{*2} - 2\sigma^*r]}.}
#' This finite-sample convention preserves the original research code. It
#' differs by the factor `sqrt((n - 1) / n)` from the population-moment form
#' often shown for Taylor diagrams. Consequently `nME^2 + sde^2` is not
#' exactly `RMSE^2 / sd(obs)^2` for finite samples.
#'
#' Equal prediction and observation SDs receive a positive sign in signed_sde,
#' as in the original code. Positive nME means underprediction.
#' @seealso [model_metrics()], [gg_taylor()], [gg_solar()], [gg_target()]
#' @references Wadoux, A. M. J.-C., Walvoort, D. J. J., and Brus, D. J.
#'   (2022). An integrated approach for the evaluation of quantitative soil
#'   maps through Taylor and solar diagrams. Geoderma, 405, 115332.
#'   <doi:10.1016/j.geoderma.2021.115332>
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#' diagram_stats(list(accurate = obs, biased = obs + 1), obs)
#' @export
diagram_stats <- function(mods, obs, na.rm = TRUE) {
  mods <- prepare_models(mods, obs, na.rm)
  rows <- lapply(seq_along(mods), function(i) {
    pair <- paired_values(mods[[i]], obs, na.rm)
    name <- names(mods)[i]
    if (anyNA(c(pair$pred, pair$obs))) {
      stop(sprintf("Model '%s' has missing pairs; use na.rm = TRUE.", name), call. = FALSE)
    }
    n <- length(pair$pred)
    if (n < 2L) {
      stop(sprintf("Model '%s' needs at least two complete pairs.", name), call. = FALSE)
    }
    m <- pair_moments(pair$pred, pair$obs)
    if (m$so == 0) {
      stop(sprintf("Model '%s': paired obs must have non-zero standard deviation.", name), call. = FALSE)
    }
    difference <- m$o - m$p
    centred <- difference - mean(difference)
    sde <- root_mean_square(centred) / m$so
    sign_sd <- if (m$sp < m$so) -1 else 1
    data.frame(model = name, n = n, r = if (m$sp == 0) NA_real_ else m$r,
      sd_ratio = m$sp / m$so, mean_error = mean(difference) * m$scale,
      nME = mean(difference) / m$so * sqrt((n - 1) / n),
      sde = sde, signed_sde = sign_sd * sde)
  })
  result <- do.call(rbind, rows)
  coordinates <- result[, !names(result) %in% c("model", "r")]
  if (any(!is.finite(as.matrix(coordinates)))) {
    stop("Diagram statistics exceed numeric precision; rescale the input units.", call. = FALSE)
  }
  result
}
