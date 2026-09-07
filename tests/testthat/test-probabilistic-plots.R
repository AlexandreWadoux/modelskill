test_that("QCP and PIT plots are ordinary ggplot objects", {
  qcp_plot <- gg_qcp(1:3, cbind(c(0, 1, 2), c(1, 2, 3)), c(.25, .75))
  pit_plot <- gg_pit(c(.1, .5, .9), bins = 3)
  expect_s3_class(qcp_plot, "ggplot")
  expect_s3_class(pit_plot, "ggplot")
  expect_true(inherits(qcp_plot + ggplot2::labs(title = "QCP"), "ggplot"))
  expect_true(inherits(pit_plot + ggplot2::theme_minimal(), "ggplot"))
  expect_error(gg_pit(c(.1, .5), bins = 2.5), "integer")
})
