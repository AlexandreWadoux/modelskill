#' Plot a probability integral transform histogram
#'
#' Displays PIT values. For calibrated continuous predictive distributions the
#' histogram should be approximately uniform; the dashed line is the expected
#' bin proportion under uniformity.
#' @param pit_values Numeric PIT values, typically calculated by [pit()].
#' @param bins Positive integer number of histogram bins.
#' @return A `ggplot2` object.
#' @examples gg_pit(stats::runif(100))
#' @export
gg_pit <- function(pit_values, bins = 10) {
  check_number(bins, "bins", positive = TRUE)
  if (bins != as.integer(bins)) stop("`bins` must be an integer.", call. = FALSE)
  values <- pit(pit_values)
  ggplot2::ggplot(data.frame(pit = values), ggplot2::aes(x = pit)) +
    ggplot2::geom_histogram(binwidth = 1 / bins, boundary = 0, fill = "grey75", colour = "grey20") +
    ggplot2::geom_hline(yintercept = length(values) / bins, linetype = "dashed", colour = "red3") +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = function(x) paste0(100 * x, "%")) +
    ggplot2::labs(x = "PIT", y = "Frequency") + ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 12, family = "sans"))
}
