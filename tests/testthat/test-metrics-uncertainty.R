test_that("interval metrics have known calibration, width and score", {
  obs <- c(0, 1, 3)
  lower <- c(-1, 0, 0)
  upper <- c(1, 2, 2)

  expect_equal(
    picp(obs, lower, upper),
    2 / 3
  )

  expect_equal(
    coverage(obs, lower, upper),
    2 / 3
  )

  expect_equal(
    coverage_error(obs, lower, upper, level = 0.8),
    2 / 3 - 0.8
  )

  expect_equal(
    interval_width(obs, lower, upper),
    2
  )

  # The third observation misses by one; alpha = 0.2 gives a penalty of 10.
  expect_equal(
    interval_score(obs, lower, upper, level = 0.8),
    16 / 3
  )
})


test_that("interval metrics validate bounds, levels and missing pairs", {
  expect_error(
    coverage(
      1:3,
      c(0, 3, 2),
      c(2, 2, 4)
    ),
    "lower"
  )

  expect_error(
    coverage_error(
      1:3,
      0:2,
      2:4,
      level = 1
    ),
    "between"
  )

  expect_equal(
    coverage(
      c(1, NA, 3),
      c(0, 0, 2),
      c(2, 2, 4)
    ),
    1
  )

  expect_true(
    is.na(
      coverage(
        c(1, NA, 3),
        c(0, 0, 2),
        c(2, 2, 4),
        na.rm = FALSE
      )
    )
  )

  expect_equal(
    interval_width(
      c(1, NA, 3),
      c(0, 0, 2),
      c(2, 2, 4)
    ),
    2
  )
})


test_that("QCP has known values and validates quantiles", {
  quantiles <- cbind(
    c(0, 1, 2),
    c(1, 2, 3)
  )

  expect_equal(
    qcp(
      1:3,
      quantiles,
      c(0.25, 0.75)
    ),
    c(
      `0.25` = 0,
      `0.75` = 1
    )
  )

  expect_error(
    qcp(
      1:3,
      cbind(1:3, 0:2),
      c(0.25, 0.75)
    ),
    "non-decreasing"
  )
})


test_that("PIT accepts already evaluated predictive CDF values", {
  values <- c(0, 0.25, 0.5, 0.75, 1)

  expect_equal(
    pit(values),
    values
  )

  expect_equal(
    pit(cdf_at_obs = values),
    values
  )
})


test_that("PIT calculates values for normal predictive distributions", {
  obs <- c(-1, 0, 1)
  pred <- c(0, 0, 0)
  predictive_sd <- c(1, 1, 1)

  expected <- stats::pnorm(
    (obs - pred) / predictive_sd
  )

  expect_equal(
    pit(
      obs = obs,
      pred = pred,
      predictive_sd = predictive_sd
    ),
    expected
  )
})


test_that("PIT normal mode agrees with directly evaluated CDF values", {
  set.seed(123)

  n <- 100

  pred <- seq(0, 10, length.out = n)
  predictive_sd <- seq(0.5, 1.5, length.out = n)

  obs <- stats::rnorm(
    n,
    mean = pred,
    sd = predictive_sd
  )

  cdf_at_obs <- stats::pnorm(
    obs,
    mean = pred,
    sd = predictive_sd
  )

  expect_equal(
    pit(
      obs = obs,
      pred = pred,
      predictive_sd = predictive_sd
    ),
    pit(cdf_at_obs)
  )
})


test_that("PIT handles missing values", {
  values <- c(0.1, NA, 0.5, NA, 0.9)

  expect_equal(
    pit(values),
    c(0.1, 0.5, 0.9)
  )

  expect_equal(
    pit(values, na.rm = FALSE),
    values
  )

  obs <- c(-1, NA, 1)
  pred <- c(0, 0, 0)
  predictive_sd <- c(1, 1, 1)

  expect_equal(
    pit(
      obs = obs,
      pred = pred,
      predictive_sd = predictive_sd
    ),
    stats::pnorm(c(-1, 1))
  )

  expect_true(
    all(
      is.na(
        pit(
          obs = obs,
          pred = pred,
          predictive_sd = predictive_sd,
          na.rm = FALSE
        )
      )
    )
  )
})


test_that("PIT validates its inputs", {
  expect_error(
    pit(c(-0.1, 0.5)),
    "between zero and one"
  )

  expect_error(
    pit(c(0.5, 1.1)),
    "between zero and one"
  )

  expect_error(
    pit(c(0.5, Inf)),
    "between zero and one"
  )

  expect_error(
    pit(character()),
    "non-empty numeric vector"
  )

  expect_error(
    pit(),
    "Supply either"
  )

  expect_error(
    pit(
      obs = 1:3,
      pred = 1:3
    ),
    "requires"
  )

  expect_error(
    pit(
      obs = 1:3,
      predictive_sd = rep(1, 3)
    ),
    "requires"
  )

  expect_error(
    pit(
      pred = 1:3,
      predictive_sd = rep(1, 3)
    ),
    "requires"
  )

  expect_error(
    pit(
      cdf_at_obs = c(0.2, 0.5, 0.8),
      obs = 1:3,
      pred = 1:3,
      predictive_sd = rep(1, 3)
    ),
    "not both"
  )

  expect_error(
    pit(
      obs = 1:3,
      pred = 1:3,
      predictive_sd = c(1, 0, 1)
    ),
    "strictly positive"
  )

  expect_error(
    pit(
      obs = 1:3,
      pred = 1:3,
      predictive_sd = c(1, -1, 1)
    ),
    "strictly positive"
  )

  expect_error(
    pit(
      obs = 1:3,
      pred = 1:2,
      predictive_sd = rep(1, 3)
    ),
    "same length"
  )

  expect_error(
    pit(
      cdf_at_obs = c(0.2, 0.8),
      na.rm = 1
    ),
    "TRUE or FALSE"
  )
})


test_that("log score has known values and validates densities", {
  expect_equal(
    log_score(
      1:2,
      c(0.5, 0.5)
    ),
    log(2)
  )

  expect_error(
    log_score(
      1:2,
      c(1, 0)
    ),
    "positive"
  )
})


test_that("CRPS supports ensembles and normal distributions", {
  draws <- matrix(
    c(-1, 1, 0, 2),
    nrow = 2
  )

  expect_equal(
    crps(
      c(0, 1),
      distribution = draws
    ),
    0.25
  )

  expect_equal(
    median_crps(
      c(0, 1),
      distribution = draws
    ),
    0.25
  )

  expect_equal(
    crps(
      0,
      pred = 0,
      predictive_sd = 1
    ),
    2 / sqrt(2 * pi) - 1 / sqrt(pi)
  )

  decomposition <- crps_decomposition(
    c(0.2, 1.2),
    draws
  )

  expect_equal(
    decomposition$crps,
    crps(
      c(0.2, 1.2),
      distribution = draws
    )
  )

  expect_equal(
    decomposition$crps,
    decomposition$reliability +
      decomposition$potential_crps
  )
})


test_that("uncertainty wrapper is interval-only", {
  interval <- uncertainty_metrics(
    1:3,
    lower = 0:2,
    upper = 2:4,
    level = 0.8
  )

  expect_equal(
    names(interval),
    c(
      "picp",
      "picp_error",
      "interval_width",
      "interval_score"
    )
  )

  expect_error(
    uncertainty_metrics(
      1:3,
      lower = 0:2
    ),
    "both"
  )
})

test_that("uncertainty wrapper uses one complete-triplet subset", {
  obs <- c(1, NA_real_, 3)
  lower <- c(0, 10, 2)
  upper <- c(2, 30, 4)

  summary <- uncertainty_metrics(obs, lower, upper, level = 0.8)

  expect_equal(summary$picp, 1)
  expect_equal(summary$picp_error, 0.2)
  expect_equal(summary$interval_width, 2)
  expect_equal(summary$interval_score, 2)
  expect_equal(interval_width(obs, lower, upper), 8)

  incomplete <- uncertainty_metrics(obs, lower, upper, level = 0.8, na.rm = FALSE)
  expect_true(all(is.na(incomplete)))
})
