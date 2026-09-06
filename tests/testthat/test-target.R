test_that("gg_target returns a composable ggplot object", {
  skip_if_not_installed("ggplot2")
  obs <- 1:5
  p <- gg_target(list(perfect = obs, biased = obs + 1), obs)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p + ggplot2::labs(title = "Test"), "ggplot"))
})

test_that("gg_target validates colour and axis inputs", {
  expect_error(gg_target(list(a = 1:3, b = 1:3), 1:3, colorval = 1), "one value per model")
  expect_error(gg_target(1:3, 1:3, axis_begin = 1, axis_end = 1), "axis_begin")
})
