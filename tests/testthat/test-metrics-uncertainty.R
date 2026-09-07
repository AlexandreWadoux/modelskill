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

test_that("predictive-SD metrics have known values", {
  obs <- c(-1, 0, 1)
  pred <- c(0, 0, 0)
  predictive_sd <- c(1, 1, 1)
  expect_equal(standardized_error(obs, pred, predictive_sd), c(-1, 0, 1))
  expect_equal(standardized_error_mean(obs, pred, predictive_sd), 0)
  expect_equal(standardized_error_sd(obs, pred, predictive_sd), 1)
  expect_equal(within_sd(obs, pred, predictive_sd, k = 1), 1)
  expect_equal(within_sd(obs, pred, predictive_sd, k = .5), 1 / 3)
})

test_that("uncertainty wrappers choose one clear mode", {
  interval <- uncertainty_metrics(1:3, lower = 0:2, upper = 2:4, level = .8)
  expect_equal(names(interval), c("picp", "picp_error", "interval_width", "interval_score"))
  sd_mode <- uncertainty_metrics(1:3, pred = 1:3, predictive_sd = rep(1, 3))
  expect_equal(names(sd_mode), c("standardized_error_mean", "standardized_error_sd", "within_1sd", "within_1.96sd"))
  expect_error(uncertainty_metrics(1:3, lower = 0:2), "both")
  expect_error(uncertainty_metrics(1:3, pred = 1:3), "both")
  expect_error(uncertainty_metrics(1:3, lower = 0:2, upper = 2:4,
                                   pred = 1:3, predictive_sd = 1), "not both")
  expect_error(within_sd(1:3, 1:3, c(1, 0, 1)), "positive")
})
