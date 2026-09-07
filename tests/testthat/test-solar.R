test_that("gg_solar returns a composable ggplot object", {
  skip_if_not_installed("ggplot2")
  obs <- 1:5
  p <- gg_solar(list(perfect = obs, biased = obs + 1), obs)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p + ggplot2::labs(title = "Test"), "ggplot"))
})

test_that("gg_solar uses the documented defaults", {
  expect_identical(formals(gg_solar)$y.axis_end, 1.1)
  expect_identical(
    eval(formals(gg_solar)$colour_by),
    c("efficiency", "model", "correlation", "r2")
  )
})

test_that("gg_solar colours by R2/NSE efficiency by default", {
  obs <- 1:10
  mods <- list(
    perfect = obs,
    biased = obs + 1,
    noisy = obs + c(rep(-1, 5), rep(1, 5))
  )
  p <- gg_solar(mods, obs)

  expected <- vapply(
    mods,
    function(pred) nse(obs, pred),
    numeric(1)
  )

  expect_equal(p$data$colvar, unname(expected), tolerance = 1e-12)
})

test_that("gg_solar supports categorical model colours", {
  obs <- 1:5
  mods <- list(alpha = obs, beta = obs + 0.5)
  p <- gg_solar(mods, obs, colour_by = "model")

  expect_true(is.factor(p$data$colvar))
  expect_identical(levels(p$data$colvar), names(mods))
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_error(
    gg_solar(mods, obs, colour_by = "model", colorval = c(1, 2)),
    "cannot be used together"
  )
})

test_that("gg_solar validates colour and axis inputs", {
  expect_error(
    gg_solar(list(a = 1:3, b = 1:3), 1:3, colorval = 1),
    "one value per model"
  )
  expect_error(
    gg_solar(1:3, 1:3, x.axis_begin = 1, x.axis_end = 1),
    "x.axis_begin"
  )
  expect_error(gg_solar(1:3, 1:3, by = 0), "positive")
  expect_error(gg_solar(1:3, 1:3, colour_by = "unknown"), "arg")
})

test_that("gg_solar builds with labels off and custom continuous colours", {
  obs <- 1:10
  p <- gg_solar(
    list(a = obs, b = obs + 1),
    obs,
    colorval = c(-1, 1),
    label = FALSE
  )
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  expect_true(any(vapply(
    p$layers,
    function(x) inherits(x$geom, "GeomPoint"),
    logical(1)
  )))
})

test_that("gg_solar supports legend and reference controls", {
  p <- gg_solar(1:5, 1:5, legend = FALSE, reference = FALSE)
  has_reference_data <- any(vapply(p$layers, function(layer) {
    is.data.frame(layer$data) &&
      "label" %in% names(layer$data) &&
      "x" %in% names(layer$data)
  }, logical(1)))

  expect_false(has_reference_data)
  expect_warning(ggplot2::ggplotGrob(p), NA)
  expect_error(gg_solar(1:5, 1:5, legend = NA), "legend")
  expect_error(gg_solar(1:5, 1:5, reference = "yes"), "reference")
})

test_that("gg_solar retains the coordinate mapping and correct titles", {
  p <- gg_solar(1:5, 1:5)
  expect_equal(all.vars(p$mapping$x), "nME")
  expect_equal(all.vars(p$mapping$y), "uRMSDnorm_sigmaD")
  expect_identical(p$labels$x, expression(ME^"*"))
  expect_identical(p$labels$y, expression(SDE^"*"))
})

test_that("gg_solar remains customizable with ggplot2", {
  obs <- 1:5
  p <- gg_solar(list(a = obs, b = obs + 0.5), obs)

  p_theme <- p + ggplot2::theme(legend.position = "bottom")
  expect_identical(p_theme$theme$legend.position, "bottom")

  p_labels <- p + ggplot2::labs(colour = "Skill", title = "Solar")
  expect_identical(p_labels$labels$colour, "Skill")
  expect_identical(p_labels$labels$title, "Solar")
})

test_that("gg_solar rejects constant observations", {
  expect_error(
    gg_solar(1:3, c(2, 2, 2)),
    "non-zero standard deviation"
  )
})
