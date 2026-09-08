#' Quantitative prediction-model evaluation and uncertainty diagnostics
#'
#' `modelskill` provides tools for evaluating continuous predictions and their
#' quantified predictive uncertainty. Point-prediction performance can be
#' summarized across one or more models with [model_metrics()] or evaluated
#' using individual statistics such as [bias()], [rmse()], [correlation()],
#' [r2()], [R2()], and [ccc()].
#'
#' Predictive uncertainty can be evaluated from prediction intervals,
#' predictive quantiles, predictive means and standard deviations, or complete
#' predictive distributions. Interval-based diagnostics include [picp()],
#' [coverage_error()], [interval_width()], [interval_score()], and
#' [uncertainty_metrics()]. Calibration across multiple interval levels can be
#' examined with [gg_coverage()] and summarized numerically with
#' [accuracy_plot_metrics()].
#'
#' Distributional calibration can be assessed using quantile coverage
#' probability with [qcp()] and [gg_qcp()], or the probability integral
#' transform with [pit()] and [gg_pit()]. [pit()] can use predictive CDF values
#' evaluated at the observations or calculate PIT values directly from
#' predictive means and standard deviations under a normal-distribution
#' assumption. Proper scoring rules and distributional summaries include
#' [crps()], [median_crps()], [crps_decomposition()], and [log_score()].
#'
#' Complementary graphical summaries of point-prediction performance are
#' provided by [gg_taylor()], [gg_solar()], and [gg_target()], with their
#' underlying statistics available through [diagram_stats()].
#'
#' All plotting functions return ordinary `ggplot2` objects and can therefore
#' be extended using standard `ggplot2` layers, scales, labels, and themes.
#' The package operates on ordinary numeric vectors, matrices, data frames, and
#' lists; no spatial data class is required.
#'
#' @keywords internal
"_PACKAGE"
