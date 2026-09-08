#' Quantile calibration plot
#'
#' Plots empirical quantile coverage probability (QCP) against the corresponding
#' nominal predictive quantile levels. For a calibrated predictive distribution,
#' approximately a proportion `p` of observations should fall below the predicted
#' `p`-quantile. Points should therefore follow the dashed 1:1 line.
#'
#' Unlike [gg_coverage()], which evaluates the joint coverage of central
#' prediction intervals, `gg_qcp()` evaluates individual predictive quantiles.
#' It can therefore reveal asymmetric or one-sided miscalibration that may be
#' hidden when lower and upper prediction-interval bounds are assessed together.
#'
#' Quantiles can be supplied directly through `quantiles` and `levels`. When the
#' predictive distribution is assumed to be normal, they can instead be generated
#' automatically from predictive means (`pred`) and predictive standard deviations
#' (`predictive_sd`). In this mode, the default is to evaluate quantiles from 0.05
#' to 0.95 in increments of 0.05.
#'
#' A calibration curve is generally most informative when quantiles are available
#' over a reasonably dense range of probability levels. A smaller number of levels
#' remains valid when only selected predictive quantiles are available.
#'
#' @inheritParams qcp
#' @param quantiles Numeric matrix or data frame containing predicted quantiles
#'   in columns. Required unless `pred` and `predictive_sd` are supplied.
#' @param levels Numeric vector of nominal quantile probabilities corresponding
#'   to the columns of `quantiles`. When `pred` and `predictive_sd` are supplied,
#'   defaults to `seq(0.05, 0.95, by = 0.05)`.
#' @param pred Optional numeric vector of predictive means. Must be supplied
#'   together with `predictive_sd`; in this mode predictive quantiles are
#'   generated assuming normal predictive distributions.
#' @param predictive_sd Optional numeric vector of predictive standard deviations.
#'   Must be supplied together with `pred`.
#' @param point_size Positive numeric point size.
#'
#' @return A `ggplot2` object with `nominal` and `qcp` data columns.
#'
#' @seealso [qcp()], [gg_coverage()], [gg_pit()]
#'
#' @examples
#' set.seed(123)
#'
#' n <- 500
#' pred <- seq(0, 10, length.out = n)
#' predictive_sd <- rep(1, n)
#' obs <- stats::rnorm(n, mean = pred, sd = predictive_sd)
#'
#' # Generate a quantile-calibration curve directly from a normal
#' # predictive distribution
#' gg_qcp(
#'   obs,
#'   pred = pred,
#'   predictive_sd = predictive_sd
#' )
#'
#' # Predicted quantiles can also be supplied directly
#' levels <- seq(0.05, 0.95, by = 0.05)
#'
#' quantiles <- vapply(
#'   levels,
#'   function(p) pred + stats::qnorm(p) * predictive_sd,
#'   numeric(n)
#' )
#'
#' gg_qcp(
#'   obs,
#'   quantiles = quantiles,
#'   levels = levels
#' )
#'
#' @export
gg_qcp <- function(obs,
                   quantiles = NULL,
                   levels = NULL,
                   pred = NULL,
                   predictive_sd = NULL,
                   na.rm = TRUE,
                   point_size = 3) {

  check_number(point_size, "point_size", positive = TRUE)

  normal_mode <- !is.null(pred) || !is.null(predictive_sd)

  if (normal_mode) {

    if (!is.null(quantiles)) {
      stop(
        "Supply either `quantiles`, or `pred` and `predictive_sd`, not both.",
        call. = FALSE
      )
    }

    if (is.null(pred) || is.null(predictive_sd)) {
      stop(
        "Normal-distribution mode requires both `pred` and `predictive_sd`.",
        call. = FALSE
      )
    }

    if (is.null(levels)) {
      levels <- seq(0.05, 0.95, by = 0.05)
    }

    if (!is_numeric_vector(levels) ||
        !length(levels) ||
        any(!is.finite(levels)) ||
        any(levels <= 0 | levels >= 1)) {
      stop(
        "`levels` must contain finite probabilities strictly between 0 and 1.",
        call. = FALSE
      )
    }

    levels <- sort(unique(levels))

    x <- prepare_predictive_sd_vectors(
      obs,
      pred,
      predictive_sd,
      na.rm
    )

    if (is.null(x) || !length(x$obs)) {
      data <- data.frame(
        nominal = levels,
        qcp = NA_real_
      )
    } else {

      quantiles <- vapply(
        levels,
        function(p) {
          x$pred + stats::qnorm(p) * x$predictive_sd
        },
        numeric(length(x$obs))
      )
      dim(quantiles) <- c(length(x$obs), length(levels))

      values <- qcp(
        x$obs,
        quantiles,
        levels,
        na.rm = FALSE
      )

      data <- data.frame(
        nominal = levels,
        qcp = unname(values)
      )
    }

  } else {

    if (is.null(quantiles)) {
      stop(
        "Supply `quantiles` and `levels`, or `pred` and `predictive_sd`.",
        call. = FALSE
      )
    }

    if (is.null(levels)) {
      stop(
        "`levels` must be supplied with `quantiles`.",
        call. = FALSE
      )
    }

    values <- qcp(
      obs,
      quantiles,
      levels,
      na.rm
    )

    data <- data.frame(
      nominal = levels,
      qcp = unname(values)
    )
  }

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = nominal, y = qcp)
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      colour = "grey50"
    ) +
    ggplot2::geom_point(
      size = point_size,
      colour = "red3"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "Nominal quantile level",
      y = "Empirical quantile coverage"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = 12,
        family = "sans"
      )
    )
}
