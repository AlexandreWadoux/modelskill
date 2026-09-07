test_that("interval metrics have known calibration, width and score", {
  obs <- c(0, 1, 3)
  lower <- c(-1, 0, 0)
  upper <- c(1, 2, 2)
  expect_equal(picp(obs, lower, upper), 2 / 3)
  expect_equal(coverage(obs, lower, upper), 2 / 3)
  expect_equal(coverage_error(obs, lower, upper, level = .8), 2 / 3 - .8)
  expect_equal(interval_width(obs, lower, upper), 2)
  # The third observation misses by one; alpha = .2 gives a penalty of 10.
  expect_equal(interval_score(obs, lower, upper, level = .8), 16 / 3)
})

test_that("interval metrics validate bounds, levels and missing pairs", {
  expect_error(coverage(1:3, c(0, 3, 2), c(2, 2, 4)), "lower")
  expect_error(coverage_error(1:3, 0:2, 2:4, level = 1), "between")
  expect_equal(coverage(c(1, NA, 3), c(0, 0, 2), c(2, 2, 4)), 1)
  expect_true(is.na(coverage(c(1, NA, 3), c(0, 0, 2), c(2, 2, 4), na.rm = FALSE)))
  expect_equal(interval_width(c(1, NA, 3), c(0, 0, 2), c(2, 2, 4)), 2)
})

test_that("QCP, PIT and log score have known values", {
  expect_equal(qcp(1:3, cbind(c(0, 1, 2), c(1, 2, 3)), c(.25, .75)), c(`0.25` = 0, `0.75` = 1))
  expect_equal(pit(c(0, .5, 1)), c(0, .5, 1))
  expect_equal(log_score(1:2, c(.5, .5)), log(2))
  expect_error(qcp(1:3, cbind(1:3, 0:2), c(.25, .75)), "non-decreasing")
  expect_error(pit(c(-.1, .5)), "between")
  expect_error(log_score(1:2, c(1, 0)), "positive")
})

test_that("CRPS supports ensembles and normal distributions", {
  draws <- matrix(c(-1, 1, 0, 2), nrow = 2)
  expect_equal(crps(c(0, 1), distribution = draws), .25)
  expect_equal(median_crps(c(0, 1), distribution = draws), .25)
  expect_equal(crps(0, pred = 0, predictive_sd = 1), 2 / sqrt(2 * pi) - 1 / sqrt(pi))
  decomposition <- crps_decomposition(c(.2, 1.2), draws)
  expect_equal(decomposition$crps, crps(c(.2, 1.2), distribution = draws))
  expect_equal(decomposition$crps, decomposition$reliability + decomposition$potential_crps)
})

test_that("uncertainty wrapper is interval-only", {
  interval <- uncertainty_metrics(1:3, lower = 0:2, upper = 2:4, level = .8)
  expect_equal(names(interval), c("picp", "picp_error", "interval_width", "interval_score"))
  expect_error(uncertainty_metrics(1:3, lower = 0:2), "both")
})
