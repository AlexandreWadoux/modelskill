test_that("gg_taylor returns a composable ggplot object", {
  skip_if_not_installed("ggplot2")
  obs <- 1:5
  p <- gg_taylor(list(perfect = obs, biased = obs + 1), obs)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p + ggplot2::labs(title = "Test"), "ggplot"))
})

test_that("gg_taylor supports labels and a single unnamed vector", {
  skip_if_not_installed("ggrepel")
  p <- gg_taylor(1:5, 1:5, label = TRUE)
  expect_s3_class(p, "ggplot")
  expect_error(gg_taylor(c(1, 2), 1:3), "same length")
})
