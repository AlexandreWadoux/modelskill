#' Plot a probability integral transform histogram
#'
#' Produces a probability integral transform (PIT) histogram for assessing the
#' calibration of continuous predictive distributions.
#'
#' For observation \eqn{y_i} with predictive cumulative distribution function
#' \eqn{F_i}, the PIT value is
#'
#' \deqn{
#' u_i = F_i(y_i).
#' }
#'
#' If the predictive distributions are calibrated and continuous, the PIT values
#' should be approximately uniformly distributed between zero and one. The
#' dashed horizontal line shows the density expected under a uniform
#' distribution.
#'
#' Departures from uniformity can indicate systematic miscalibration. Common
#' patterns include:
#'
#' \itemize{
#'   \item a U-shaped histogram, with excess values near zero and one, which is
#'   commonly associated with predictive distributions that are too narrow
#'   (underdispersed);
#'   \item a hump-shaped histogram, with excess values near 0.5, which is
#'   commonly associated with predictive distributions that are too wide
#'   (overdispersed);
#'   \item an excess of PIT values near zero, which can occur when predictions
#'   are systematically too high relative to the observations;
#'   \item an excess of PIT values near one, which can occur when predictions
#'   are systematically too low relative to the observations.
#' }
#'
#' These patterns are diagnostic rather than unique: different forms of
#' misspecification can produce similar PIT histograms. The PIT should therefore
#' be interpreted together with other calibration and performance diagnostics.
#'
#' PIT histograms assess the calibration of the complete predictive
#' distribution. This differs from [gg_coverage()], which evaluates central
#' prediction-interval coverage, and [gg_qcp()], which evaluates calibration of
#' individual predictive quantiles.
#'
#' The number of histogram bins affects the appearance of the diagnostic.
#' Too few bins may conceal departures from uniformity, whereas too many bins
#' can make sampling variability appear as structure, particularly for small
#' validation datasets.
#'
#' @param pit_values Numeric vector of PIT values between zero and one,
#'   typically calculated with [pit()].
#' @param bins Positive integer number of equal-width histogram bins.
#' @param na.rm Logical; remove missing PIT values? The default is `TRUE`.
#'
#' @return A `ggplot2` object showing the empirical PIT density. Under uniform
#'   calibration the expected density is one.
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
#' @seealso [pit()], [gg_qcp()], [gg_coverage()]
#'
#' @examples
#' set.seed(123)
#'
#' # Approximately calibrated PIT values
#' values <- stats::runif(500)
#'
#' gg_pit(values)
#'
#' # Use more bins
#' gg_pit(values, bins = 20)
#'
#' @export
gg_pit <- function(pit_values, bins = 10, na.rm = TRUE) {

  check_number(bins, "bins", positive = TRUE)
  check_flag(na.rm, "na.rm")

  if (bins != as.integer(bins)) {
    stop(
      "`bins` must be a positive integer.",
      call. = FALSE
    )
  }

  values <- pit(
    pit_values,
    na.rm = na.rm
  )

  if (!length(values) || all(is.na(values))) {
    stop(
      "No valid PIT values are available to plot.",
      call. = FALSE
    )
  }

  data <- data.frame(
    pit = values
  )

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = pit)
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      binwidth = 1 / bins,
      boundary = 0,
      fill = "grey75",
      colour = "grey20"
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      colour = "red3"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      labels = function(x) paste0(round(100 * x), "%"),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "PIT",
      y = "Density"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = 12,
        family = "sans"
      )
    )
}
