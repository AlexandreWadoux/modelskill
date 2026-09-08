#' Summarize calibration from an accuracy plot
#'
#' Computes numerical summaries of the departures from the 1:1 line in a
#' prediction-interval reliability plot, also known in geostatistics as an
#' accuracy plot. The approach evaluates prediction interval coverage
#' probability (PICP) over multiple nominal interval levels.
#'
#' For a well-calibrated uncertainty model, the empirical coverage
#' \eqn{\mathrm{PICP}(p)} should be close to the nominal coverage probability
#' \eqn{p} over the range of evaluated prediction intervals. Perfect
#' calibration therefore corresponds to the 1:1 line.
#'
#' The overall departure from this line is summarized by the absolute area
#'
#' \deqn{
#' A = \int_0^1 |\mathrm{PICP}(p)-p|\,dp.
#' }
#'
#' `absolute_deviation` is a numerical approximation of this area using
#' trapezoidal integration. A value of zero indicates perfect calibration;
#' larger values indicate greater overall disagreement between nominal and
#' empirical coverage.
#'
#' The total deviation is further separated according to whether the empirical
#' coverage curve lies above or below the 1:1 line:
#'
#' \deqn{
#' A_{\mathrm{over}} =
#' \int_0^1 \max\{\mathrm{PICP}(p)-p,0\}\,dp,
#' }
#'
#' \deqn{
#' A_{\mathrm{under}} =
#' \int_0^1 \max\{p-\mathrm{PICP}(p),0\}\,dp.
#' }
#'
#' `over_uncertainty` is the area above the 1:1 line, where empirical coverage
#' exceeds nominal coverage. This corresponds to over-coverage and is generally
#' associated with prediction intervals that are too wide, or predictive
#' uncertainty that is overestimated.
#'
#' `under_uncertainty` is the area below the 1:1 line, where empirical coverage
#' is smaller than nominal coverage. This corresponds to under-coverage and is
#' generally associated with prediction intervals that are too narrow, or
#' predictive uncertainty that is underestimated.
#'
#' `over_percent` and `under_percent` give the relative contributions of these
#' two components to the total absolute deviation. They describe percentages of
#' the total area of miscalibration, not percentages of observations. When
#' `absolute_deviation` is greater than zero, the two percentages sum to 100.
#'
#' The integration is performed over the supplied nominal levels after adding
#' the endpoints `(0, 0)` and `(1, 1)` to the reliability curve. For a normal
#' predictive distribution, supplying `pred` and `predictive_sd` without
#' `levels` evaluates the default sequence of nominal interval levels from 1%
#' to 99%.
#'
#' These summaries describe calibration rather than sharpness. They should
#' therefore be interpreted together with measures such as prediction interval
#' width or a proper scoring rule when comparing predictive uncertainty.
#'
#' Accuracy plots were proposed for the direct assessment of local uncertainty
#' by Deutsch (1997) and subsequently applied to geostatistical uncertainty
#' evaluation in soil science by Goovaerts (2001). Related numerical summaries
#' of departure from the accuracy-plot reference line were used by Wadoux,
#' Brus, and Heuvelink (2018).
#'
#' @inheritParams gg_coverage
#'
#' @return A one-row data frame containing:
#' \describe{
#'   \item{absolute_deviation}{Total area between the empirical coverage curve
#'   and the 1:1 line. Zero is ideal.}
#'   \item{over_uncertainty}{Area above the 1:1 line, corresponding to
#'   over-coverage.}
#'   \item{under_uncertainty}{Area below the 1:1 line, corresponding to
#'   under-coverage.}
#'   \item{over_percent}{Percentage of total absolute deviation occurring above
#'   the 1:1 line.}
#'   \item{under_percent}{Percentage of total absolute deviation occurring below
#'   the 1:1 line.}
#' }
#'
#' @references
#' Deutsch, C. V. (1997). Direct assessment of local accuracy and precision.
#' In E. Y. Baafi and N. A. Schofield (Eds.), *Geostatistics Wollongong '96*,
#' pp. 115-125.
#'
#' Goovaerts, P. (2001). Geostatistical modelling of uncertainty in soil
#' science. *Geoderma*, 103, 3-26.
#' <doi:10.1016/S0016-7061(01)00067-2>
#'
#' Wadoux, A. M. J.-C., Brus, D. J. and Heuvelink, G. B. M. (2018).
#' Accounting for non-stationary variance in geostatistical mapping of soil
#' properties. *Geoderma*, 324, 138-147.
#'
#' @seealso [gg_coverage()], [picp()], [coverage_error()],
#'   [interval_width()], [interval_score()]
#'
#' @examples
#' set.seed(123)
#' n <- 200
#' pred <- seq(0, 10, length.out = n)
#' predictive_sd <- rep(1, n)
#' obs <- stats::rnorm(n, mean = pred, sd = predictive_sd)
#'
#' accuracy_plot_metrics(
#'   obs,
#'   pred = pred,
#'   predictive_sd = predictive_sd
#' )
#'
#' @export
accuracy_plot_metrics <- function(obs, lower = NULL, upper = NULL, level = NULL,
                                  pred = NULL, predictive_sd = NULL, levels = NULL,
                                  na.rm = TRUE) {
  curve <- coverage_intervals(
    obs, lower, upper, level, na.rm,
    pred, predictive_sd, levels
  )

  curve <- curve[order(curve$nominal), ]

  x <- c(0, curve$nominal, 1)
  d <- c(0, curve$picp - curve$nominal, 0)

  area <- function(y) {
    sum(
      diff(x) *
        (utils::head(y, -1) + utils::tail(y, -1)) / 2
    )
  }

  absolute <- area(abs(d))
  over <- area(pmax(d, 0))
  under <- area(pmax(-d, 0))

  data.frame(
    absolute_deviation = absolute,
    over_uncertainty = over,
    under_uncertainty = under,
    over_percent = if (absolute == 0) {
      NA_real_
    } else {
      100 * over / absolute
    },
    under_percent = if (absolute == 0) {
      NA_real_
    } else {
      100 * under / absolute
    }
  )
}
