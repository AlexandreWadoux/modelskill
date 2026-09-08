test_that("QCP plots are ordinary ggplot objects", {
  obs <- 1:3
  quantiles <- cbind(
    c(0, 1, 2),
    c(1, 2, 3)
  )

  p <- gg_qcp(
    obs,
    quantiles = quantiles,
    levels = c(0.25, 0.75)
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$data$nominal, c(0.25, 0.75))
  expect_equal(p$data$qcp, c(0, 1))
  expect_equal(p$labels$x, "Nominal quantile level")
  expect_equal(p$labels$y, "Empirical quantile coverage")

  expect_true(
    inherits(
      p + ggplot2::labs(title = "QCP"),
      "ggplot"
    )
  )
})


test_that("QCP plots support normal predictive distributions", {
  set.seed(123)

  n <- 100
  pred <- seq(0, 10, length.out = n)
  predictive_sd <- rep(1, n)
  obs <- stats::rnorm(n, mean = pred, sd = predictive_sd)

  p <- gg_qcp(
    obs,
    pred = pred,
    predictive_sd = predictive_sd
  )

  expect_s3_class(p, "ggplot")
  expect_equal(
    p$data$nominal,
    seq(0.05, 0.95, by = 0.05)
  )
  expect_length(p$data$qcp, 19)
  expect_true(all(p$data$qcp >= 0 & p$data$qcp <= 1))

  expect_error(
    gg_qcp(
      obs,
      pred = pred
    ),
    "requires both"
  )

  expect_error(
    gg_qcp(
      obs,
      quantiles = cbind(pred, pred + 1),
      levels = c(0.25, 0.75),
      pred = pred,
      predictive_sd = predictive_sd
    ),
    "not both"
  )
})


test_that("PIT plots are ordinary ggplot objects on a density scale", {
  values <- c(0.05, 0.15, 0.35, 0.55, 0.75, 0.95)

  p <- gg_pit(
    values,
    bins = 5
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$data$pit, values)
  expect_equal(p$labels$x, "PIT")
  expect_equal(p$labels$y, "Density")

  built <- ggplot2::ggplot_build(p)
  hline <- which(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomHline"),
    logical(1)
  ))

  expect_length(hline, 1)
  expect_equal(
    unique(built$data[[hline]]$yintercept),
    1
  )

  expect_true(
    inherits(
      p + ggplot2::theme_minimal(),
      "ggplot"
    )
  )
})


test_that("PIT plots handle missing values and validate arguments", {
  p <- gg_pit(
    c(0.1, NA, 0.5, 0.9),
    bins = 4
  )

  expect_equal(
    p$data$pit,
    c(0.1, 0.5, 0.9)
  )

  expect_error(
    gg_pit(c(0.1, 0.5), bins = 2.5),
    "integer"
  )

  expect_error(
    gg_pit(c(0.1, 0.5), bins = 0),
    "positive"
  )

  expect_error(
    gg_pit(c(-0.1, 0.5)),
    "between zero and one"
  )

  expect_error(
    gg_pit(rep(NA_real_, 3)),
    "No valid PIT values"
  )

  expect_error(
    gg_pit(c(0.1, 0.5), na.rm = 1),
    "TRUE or FALSE"
  )
})
