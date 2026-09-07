#' Summarize accuracy-plot calibration
#'
#' Derives calibration summaries from a prediction-interval coverage plot, also
#' called an accuracy plot. `absolute_deviation` is the trapezoidal integral of
#' the absolute vertical distance from the 1:1 line. `over_uncertainty` and
#' `under_uncertainty` are the corresponding areas above and below the line;
#' their percentages partition absolute deviation when it is non-zero. For a
#' normal predictive distribution, supply `pred` and `predictive_sd` to assess
#' the default 1%--99% curve, as in Goovaerts (2001).
#'
#' @inheritParams gg_coverage
#' @return One-row data frame with absolute and directional calibration areas.
#' @references Goovaerts, P. (2001). Geostatistical modelling of uncertainty in
#'   soil science. *Geoderma*, 103, 3-26. <doi:10.1016/S0016-7061(01)00067-2>
#' @examples
#' accuracy_plot_metrics(1:3, pred = c(1, 2, 4), predictive_sd = rep(1, 3))
#' @export
accuracy_plot_metrics <- function(obs, lower = NULL, upper = NULL, level = NULL,
                                  pred = NULL, predictive_sd = NULL, levels = NULL,
                                  na.rm = TRUE) {
  curve <- coverage_intervals(obs, lower, upper, level, na.rm,
                              pred, predictive_sd, levels)
  curve <- curve[order(curve$nominal), ]
  x <- c(0, curve$nominal, 1)
  d <- c(0, curve$empirical - curve$nominal, 0)
  area <- function(y) sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  absolute <- area(abs(d))
  over <- area(pmax(d, 0))
  under <- area(pmax(-d, 0))
  data.frame(absolute_deviation = absolute, over_uncertainty = over,
    under_uncertainty = under,
    over_percent = if (absolute == 0) NA_real_ else 100 * over / absolute,
    under_percent = if (absolute == 0) NA_real_ else 100 * under / absolute)
}
