test_that("gg_solar returns a composable ggplot object", {
  skip_if_not_installed("ggplot2")
  obs <- 1:5
  p <- gg_solar(list(perfect = obs, biased = obs + 1), obs)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p + ggplot2::labs(title = "Test"), "ggplot"))
})

test_that("gg_solar validates colour and axis inputs", {
  expect_error(gg_solar(list(a = 1:3, b = 1:3), 1:3, colorval = 1), "one value per model")
  expect_error(gg_solar(1:3, 1:3, x.axis_begin = 1, x.axis_end = 1), "x.axis_begin")
  expect_error(gg_solar(1:3, 1:3, by = 0), "positive")
})
