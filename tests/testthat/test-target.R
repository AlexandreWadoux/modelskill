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

test_that("gg_target builds with labels off and custom continuous colours", {
  obs <- 1:10
  p <- gg_target(list(a = obs, b = obs + 1), obs,
                 colorval = c(-1, 1), label = FALSE)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_true(any(vapply(p$layers, function(x) inherits(x$geom, "GeomPoint"), logical(1))))
})

test_that("gg_target supports legend and reference controls", {
  p <- gg_target(1:5, 1:5, legend = FALSE, reference = FALSE)
  has_reference_data <- any(vapply(p$layers, function(layer) {
    is.data.frame(layer$data) && "r" %in% names(layer$data) &&
      "x" %in% names(layer$data)
  }, logical(1)))

  expect_false(has_reference_data)
  expect_warning(ggplot2::ggplotGrob(p), NA)
  expect_error(gg_target(1:5, 1:5, legend = NA), "legend")
  expect_error(gg_target(1:5, 1:5, reference = "yes"), "reference")
})

test_that("gg_target retains the authoritative axis mapping and titles", {
  p <- gg_target(1:5, 1:5)
  expect_equal(all.vars(p$mapping$x), "uRMSDnorm_sigmaD")
  expect_equal(all.vars(p$mapping$y), "nME")
  expect_identical(p$labels$x, expression(ME^"*"))
  expect_identical(p$labels$y, expression(SDE^"*" %.% sign(sigma[d])))
})

test_that("gg_target rejects constant observations", {
  expect_error(gg_target(1:3, c(2, 2, 2)), "non-zero standard deviation")
})
