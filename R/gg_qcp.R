#' Plot quantile coverage probability calibration
#'
#' Plots QCP against nominal quantile level. The dashed 1:1 line indicates
#' calibrated quantiles. Unlike central PICP plots, this display can reveal
#' one-sided bias.
#' @inheritParams qcp
#' @param point_size Positive numeric point size.
#' @return A `ggplot2` object with `nominal` and `qcp` data columns.
#' @examples
#' gg_qcp(1:3, cbind(c(0, 1, 2), c(1, 2, 3)), c(.25, .75))
#' @export
gg_qcp <- function(obs, quantiles, levels, na.rm = TRUE, point_size = 3) {
  check_number(point_size, "point_size", positive = TRUE)
  values <- qcp(obs, quantiles, levels, na.rm)
  data <- data.frame(nominal = levels, qcp = unname(values))
  ggplot2::ggplot(data, ggplot2::aes(x = nominal, y = qcp)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(size = point_size, colour = "red3") +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(100 * x, "%")) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(100 * x, "%")) +
    ggplot2::coord_equal() + ggplot2::labs(x = "Nominal quantile level", y = "QCP") +
    ggplot2::theme_classic() + ggplot2::theme(text = ggplot2::element_text(size = 12, family = "sans"))
}
