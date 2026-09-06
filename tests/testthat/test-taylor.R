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

test_that("gg_taylor uses portable plotmath annotations", {
  p <- gg_taylor(1:5, 1:5)
  expect_true(is.language(p$labels$x))
  expect_false(inherits(p$labels$x, "latexexpression"))
  expect_warning(ggplot2::ggplotGrob(p), NA)
})

test_that("gg_taylor supports half-diagram and legend modes", {
  obs <- 1:5
  models <- list(positive = obs, negative = rev(obs))
  full <- gg_taylor(models, obs)
  half <- gg_taylor(models, obs, half = TRUE)
  keyed <- gg_taylor(models, obs, legend = TRUE)

  expect_equal(full$data$Model, c("positive", "negative"))
  expect_equal(half$data$Model, "positive")
  expect_equal(keyed$labels$colour, "Model")
  expect_true(any(vapply(keyed$layers, function(layer) {
    "colour" %in% names(layer$mapping)
  }, logical(1))))
  expect_warning(ggplot2::ggplotGrob(keyed), NA)
})

test_that("gg_taylor supports RMSD controls", {
  obs <- 1:5
  no_contours <- gg_taylor(obs, obs, rmsd = FALSE)
  custom <- gg_taylor(obs, obs, rmsd_colour = "steelblue",
                      rmsd_breaks = c(0.25, 1, 2))
  has_circle_data <- function(plot) {
    any(vapply(plot$layers, function(layer) {
      is.data.frame(layer$data) && "xcircle" %in% names(layer$data)
    }, logical(1)))
  }

  expect_false(has_circle_data(no_contours))
  expect_true(has_circle_data(custom))
  expect_true(any(vapply(custom$layers, function(layer) {
    identical(layer$aes_params$colour, "steelblue")
  }, logical(1))))
  expect_warning(ggplot2::ggplotGrob(custom), NA)
})

test_that("gg_taylor validates its additional controls", {
  expect_error(gg_taylor(1:5, 1:5, legend = NA), "legend")
  expect_error(gg_taylor(1:5, 1:5, half = 1), "half")
  expect_error(gg_taylor(1:5, 1:5, rmsd = "yes"), "rmsd")
  expect_error(gg_taylor(1:5, 1:5, rmsd_colour = ""), "rmsd_colour")
  expect_error(gg_taylor(1:5, 1:5, rmsd_breaks = c(0, 1)), "positive")
  expect_error(gg_taylor(1:5, 1:5, label = TRUE, legend = TRUE),
               "either")
})

test_that("gg_taylor rejects constant observations", {
  expect_error(gg_taylor(1:3, c(2, 2, 2)), "non-zero standard deviation")
})
