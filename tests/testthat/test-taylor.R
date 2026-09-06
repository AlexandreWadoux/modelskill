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

test_that("gg_taylor builds without labels and supports point size", {
  obs <- 1:10
  p <- gg_taylor(list(a = obs, b = obs + 1), obs,
                 label = FALSE, point_size = 3)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_true(any(vapply(p$layers, function(x) inherits(x$geom, "GeomPoint"), logical(1))))
})

test_that("gg_taylor rejects constant observations", {
  expect_error(gg_taylor(1:3, c(2, 2, 2)), "non-zero standard deviation")
})
