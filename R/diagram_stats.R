#' Statistics underlying summary diagrams
#'
#' Computes the statistics used as coordinates in Taylor, solar, and target
#' diagrams without constructing a plot. This is useful for inspecting the
#' numerical quantities represented by the diagrams or for constructing custom
#' visualizations.
#'
#' @inheritParams model_metrics
#' @param na.rm Logical; remove incomplete observation-prediction pairs
#'   separately for each model? If `FALSE`, missing pairs cause an error because
#'   diagram coordinates cannot be calculated. The default is `TRUE`.
#'
#' @return A data frame with one row per model and the following columns:
#' \describe{
#'   \item{model}{Model identifier.}
#'   \item{n}{Number of complete observation-prediction pairs.}
#'   \item{r}{Pearson correlation coefficient.}
#'   \item{sd_ratio}{Ratio of prediction to observation standard deviation.}
#'   \item{mean_error}{Mean error, calculated as observation minus prediction,
#'   in the original response units.}
#'   \item{nME}{Normalized mean error (ME*), obtained by dividing mean error by
#'   the population-moment standard deviation of the observations.}
#'   \item{sde}{Standardized standard deviation of the error (SDE*), obtained by
#'   dividing the population-moment standard deviation of the errors by the
#'   population-moment standard deviation of the observations.}
#'   \item{signed_sde}{SDE* multiplied by the sign of the difference between
#'   prediction and observation standard deviations.}
#' }
#'
#' @details
#' Inputs are paired by position. At least two complete pairs and non-zero
#' observation standard deviation are required for every model.
#'
#' For the diagram geometry, standard deviations are calculated as population
#' moments, using divisor \eqn{n}, rather than the \eqn{n - 1} sample standard
#' deviation returned by `stats::sd()`. This convention makes the normalized
#' error decomposition exact for finite samples.
#'
#' Let
#'
#' \deqn{e_i = obs_i - pred_i}
#'
#' denote the prediction error, and let \eqn{\sigma_p} and \eqn{\sigma_o}
#' denote the population-moment standard deviations of predictions and
#' observations, respectively.
#'
#' The standard-deviation ratio is
#'
#' \deqn{
#' \sigma^* = \frac{\sigma_p}{\sigma_o}.
#' }
#'
#' The normalized mean error is
#'
#' \deqn{
#' \mathrm{ME}^* =
#' \frac{\bar{e}}{\sigma_o}.
#' }
#'
#' Positive ME* indicates underprediction and negative ME* indicates
#' overprediction under the package convention `obs - pred`.
#'
#' Let \eqn{\sigma_e} denote the population-moment standard deviation of the
#' errors. The standardized error standard deviation is
#'
#' \deqn{
#' \mathrm{SDE}^* =
#' \frac{\sigma_e}{\sigma_o}.
#' }
#'
#' Using the relationship between the variances of observations, predictions,
#' and their errors, this is equivalently
#'
#' \deqn{
#' \mathrm{SDE}^* =
#' \sqrt{
#' 1 + \sigma^{*2} - 2\sigma^*r
#' }.
#' }
#'
#' The sign used for `signed_sde` indicates whether the prediction standard
#' deviation is smaller or larger than the observation standard deviation:
#' negative when \eqn{\sigma_p < \sigma_o} and positive when
#' \eqn{\sigma_p \geq \sigma_o}. Equal standard deviations therefore receive a
#' positive sign, following the original diagram implementation.
#'
#' Constant predictions have undefined Pearson correlation and are returned
#' with Pearson correlation set to `NA`, an SD ratio of zero, and SDE equal to
#' one. Their diagram geometry remains
#' defined even though their correlation is not.
#'
#' With this common population-moment normalization, the exact finite-sample
#' relationship is
#'
#' \deqn{
#' \frac{\mathrm{RMSE}^2}{\sigma_o^2}
#' =
#' \mathrm{ME}^{*2}
#' +
#' \mathrm{SDE}^{*2}.
#' }
#'
#' Thus Euclidean distance from the origin in the solar and target diagrams is
#' exactly RMSE normalized by the population-moment observation standard
#' deviation.
#'
#' @seealso [model_metrics()], [gg_taylor()], [gg_solar()], [gg_target()]
#'
#' @references
#' Wadoux, A. M. J.-C., Walvoort, D. J. J., and Brus, D. J. (2022).
#' An integrated approach for the evaluation of quantitative soil maps through
#' Taylor and solar diagrams. *Geoderma*, 405, 115332.
#' <doi:10.1016/j.geoderma.2021.115332>
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#'
#' mods <- list(
#'   perfect = obs,
#'   biased = obs + 1,
#'   noisy = c(1, 3, 2, 5, 4)
#' )
#'
#' diagram_stats(mods, obs)
#'
#' @export
diagram_stats <- function(mods, obs, na.rm = TRUE) {

  mods <- prepare_models(mods, obs, na.rm)

  rows <- lapply(seq_along(mods), function(i) {

    pair <- paired_values(
      mods[[i]],
      obs,
      na.rm
    )

    model_name <- names(mods)[i]

    if (anyNA(pair$pred) || anyNA(pair$obs)) {
      stop(
        sprintf(
          "Model '%s' has missing pairs; use na.rm = TRUE.",
          model_name
        ),
        call. = FALSE
      )
    }

    n <- length(pair$pred)

    if (n < 2L) {
      stop(
        sprintf(
          "Model '%s' needs at least two complete pairs.",
          model_name
        ),
        call. = FALSE
      )
    }

    m <- pair_moments(
      pair$pred,
      pair$obs
    )

    if (m$so == 0) {
      stop(
        sprintf(
          "Model '%s': paired observations must have non-zero standard deviation.",
          model_name
        ),
        call. = FALSE
      )
    }

    error <- m$o - m$p
    mean_error_scaled <- mean(error)

    centred_error <- error - mean_error_scaled

    sde <- root_mean_square(centred_error) / m$so

    sign_sd <- if (m$sp < m$so) -1 else 1

    data.frame(
      model = model_name,
      n = n,
      r = if (m$sp == 0) NA_real_ else m$r,
      sd_ratio = m$sp / m$so,
      mean_error = mean_error_scaled * m$scale,
      nME = mean_error_scaled / m$so,
      sde = sde,
      signed_sde = sign_sd * sde
    )
  })

  result <- do.call(
    rbind,
    rows
  )

  finite_values <- result[
    ,
    !names(result) %in% c("model", "r")
  ]

  if (any(!is.finite(as.matrix(finite_values)))) {
    stop(
      "Diagram statistics exceed numeric precision; rescale the input units.",
      call. = FALSE
    )
  }

  rownames(result) <- NULL

  result
}