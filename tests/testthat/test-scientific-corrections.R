test_that("CRPS decomposition includes edge ties and agrees with pairwise CRPS", {
  draws <- matrix(c(-1, 1, 0, 2), nrow = 2)
  got <- crps_decomposition(c(0, 1), draws)
  expect_equal(got, data.frame(crps = 0.25, reliability = 0,
                               potential_crps = 0.25))

  # Independent empirical-distribution identity, not the decomposition formula.
  pairwise_crps <- function(y, x) {
    mean(abs(x - y)) - mean(abs(outer(x, x, "-"))) / 2
  }
  ensembles <- list(c(-1, 1), c(0, 0, 2), c(1, 1, 1), c(-2, 0, 0, 3))
  for (x in ensembles) {
    obs <- sort(unique(c(x, x - 0.5, x + 0.5)))
    distribution <- matrix(rep(x, each = length(obs)), nrow = length(obs))
    expected <- mean(vapply(obs, pairwise_crps, numeric(1), x = x))
    got <- crps_decomposition(obs, distribution)
    expect_equal(got$crps, expected, tolerance = 1e-12)
    expect_equal(got$crps, crps(obs, distribution), tolerance = 1e-12)
    expect_gte(got$reliability, 0)
    expect_gte(got$potential_crps, 0)
    for (y in obs) {
      one <- crps_decomposition(y, matrix(x, nrow = 1))
      expect_equal(one$crps, pairwise_crps(y, x), tolerance = 1e-12)
    }
  }
  # Ties, unequal row distributions, both exterior bins, and missing rows.
  set.seed(260908)
  for (i in seq_len(20)) {
    x <- matrix(sample(-3:3, 48, replace = TRUE), nrow = 8)
    obs <- sample(-4:4, 8, replace = TRUE)
    expected <- mean(vapply(seq_along(obs), function(j) {
      pairwise_crps(obs[j], x[j, ])
    }, numeric(1)))
    expect_equal(crps_decomposition(obs, x)$crps, expected, tolerance = 1e-12)
  }
  expect_equal(crps_decomposition(c(NA, 0, 1), rbind(NA, draws)),
               crps_decomposition(c(0, 1), draws))
})

test_that("accuracy areas split at crossings of the identity line", {
  bounds <- list(`0.25` = c(-1, -1), `0.75` = c(-1, -1))
  upper <- lapply(bounds, function(x) x + 2)
  expect_equal(accuracy_plot_metrics(c(0, 2), bounds, upper),
               data.frame(absolute_deviation = 0.125, over_uncertainty = 0.0625,
                          under_uncertainty = 0.0625, over_percent = 50,
                          under_percent = 50))
  # Unequal triangle areas on the two sides of an off-centre crossing.
  lower <- list(`0.2` = rep(-1, 4), `0.8` = rep(-1, 4))
  upper <- list(`0.2` = rep(0, 4), `0.8` = rep(1, 4))
  got <- accuracy_plot_metrics(0:3, lower, upper)
  crossing <- 0.2 + 0.6 * 0.05 / 0.35
  over <- 0.2 * 0.05 / 2 + (crossing - 0.2) * 0.05 / 2
  under <- (0.8 - crossing) * 0.3 / 2 + 0.2 * 0.3 / 2
  expect_equal(got$absolute_deviation, over + under)
  expect_equal(got$over_uncertainty, over)
  expect_equal(got$under_uncertainty, under)
  expect_equal(got$over_percent, 100 * over / (over + under))
  expect_equal(got$over_percent + got$under_percent, 100)
  expect_silent(ideal <- accuracy_plot_metrics(c(0, 2), c(-1, -1), c(1, 1),
                                               level = 0.5))
  expect_equal(unname(unlist(ideal[1:3])), c(0, 0, 0))
  expect_true(all(is.na(ideal[4:5])))
})

test_that("coverage curves share complete cases across levels", {
  obs <- c(0, 1, 2, 3)
  lower <- list(`0.5` = c(-1, -1, NA, -1), `0.9` = c(-1, NA, -1, -1))
  upper <- list(`0.5` = c(1, 1, 1, 1), `0.9` = c(3, 3, 3, 3))
  expect_silent(p <- gg_coverage(obs, lower, upper))
  expect_equal(p$data$picp, c(0.5, 1))
  keep <- c(1, 4)
  expect_equal(p$data, gg_coverage(obs[keep], lapply(lower, `[`, keep),
                                   lapply(upper, `[`, keep))$data)
  expect_equal(accuracy_plot_metrics(obs, lower, upper),
               accuracy_plot_metrics(obs[keep], lapply(lower, `[`, keep),
                                     lapply(upper, `[`, keep)))
  expect_true(all(is.na(gg_coverage(obs, lower, upper, na.rm = FALSE)$data$picp)))
  expect_warning(missing <- accuracy_plot_metrics(obs, lower, upper, na.rm = FALSE),
                 "inputs contain missing values")
  expect_true(all(is.na(missing)))
  lower$`0.5`[] <- NA_real_
  expect_warning(empty <- accuracy_plot_metrics(obs, lower, upper),
                 "no complete validation cases remain")
  expect_true(all(is.na(empty)))
  expect_named(empty, c("absolute_deviation", "over_uncertainty", "under_uncertainty",
                        "over_percent", "under_percent"))
  expect_warning(normal_empty <- accuracy_plot_metrics(c(NA_real_, NA_real_),
                 pred = c(0, 0), predictive_sd = c(1, 1)), "no complete validation cases")
  expect_true(all(is.na(normal_empty)))
  expect_error(gg_coverage(obs, list(`0.5` = -1), list(`0.5` = 1)), "length")
  expect_error(gg_coverage(obs, list(`0.5` = rep(2, 4)),
                           list(`0.5` = rep(1, 4))), "lower")
})

test_that("CCC follows its covariance definition for constant inputs", {
  for (x in list(0:2, rep(2, 3))) {
    expect_silent(expect_equal(ccc(rep(1, 3), x), 0))
    expect_silent(expect_equal(ccc(x, rep(1, 3)), 0))
    expect_equal(model_metrics(x, rep(1, 3))$ccc, 0)
    expect_equal(model_metrics(x, rep(1, 3))$Cb, 0)
  }
  expect_silent(expect_equal(ccc(1, 2), 0))
  for (x in list(1, rep(1, 3))) {
    expect_warning(value <- ccc(x, x), "CCC.*denominator is zero")
    expect_identical(value, NA_real_)
    expect_true(is.na(model_metrics(x, x)$ccc))
    expect_true(is.na(model_metrics(x, x)$Cb))
  }
  expect_silent(expect_equal(ccc(c(1, NA, 1), c(0, 9, 2)), 0))
  obs <- c(1, 2, 5, 8)
  pred <- c(2, 2, 4, 7)
  expected <- 2 * mean((obs - mean(obs)) * (pred - mean(pred))) /
    (mean((obs - mean(obs))^2) + mean((pred - mean(pred))^2) +
       (mean(obs) - mean(pred))^2)
  expect_equal(ccc(obs, pred), expected, tolerance = 1e-12)
  expect_equal(ccc(pred, obs), expected, tolerance = 1e-12)
})

test_that("normal QCP keeps matrix shape for one retained validation case", {
  for (levels in list(0.5, c(0.25, 0.5, 0.75))) {
    expected <- as.numeric(levels >= 0.5)
    expect_silent(p <- gg_qcp(0, pred = 0, predictive_sd = 1, levels = levels))
    expect_s3_class(p, "ggplot")
    expect_equal(p$data$qcp, expected)
    expect_equal(gg_qcp(c(0, NA), pred = c(0, 0), predictive_sd = c(1, 1),
                        levels = levels)$data, p$data)
    expect_silent(ggplot2::ggplot_build(p + ggplot2::theme_minimal() +
                  ggplot2::labs(title = "One case")))
  }
})

test_that("scaled metric calculations avoid spurious overflow and underflow", {
  obs <- c(1, 2, 3)
  pred <- c(1, 2, 4)
  dimensionless <- list(rpd, rpiq, rer, mape, mpe, smape, rae, rrmse,
                        willmott_d, kge, nrmse, ccc)
  for (scale in c(1e160, 1e-170, -1e160, -1e-170)) {
    for (metric in dimensionless) {
      expect_silent(got <- metric(obs * scale, pred * scale))
      expect_equal(got, metric(obs, pred), tolerance = 1e-12)
    }
    for (metric in list(mdae, sep)) {
      expect_equal(metric(obs * scale, pred * scale) / abs(scale),
                   metric(obs, pred), tolerance = 1e-12)
    }
  }
  expect_equal(rpd(obs * 1e160, pred * 1e160), sqrt(3))
  expect_equal(rrmse(obs * 1e-170, pred * 1e-170), 100 / (2 * sqrt(3)))
  expect_equal(willmott_d(obs * 1e160, pred * 1e160), 12 / 13)
  for (metric in list(rpd, rpiq, rer)) {
    expect_silent(expect_identical(metric(obs * 1e-170, obs * 1e-170), Inf))
  }
  # Wrapper uses the same stable extended components; keep MSE representable.
  original <- model_metrics(pred, obs, extended = TRUE)
  for (scale in c(1e150, 1e-150)) {
    got <- model_metrics(pred * scale, obs * scale, extended = TRUE)
    columns <- c("rpd", "rpiq", "rer", "mape", "mpe", "smape", "rae", "rrmse",
                 "willmott_d", "kge", "nrmse", "ccc", "Cb")
    expect_equal(got[columns], original[columns], tolerance = 1e-12)
  }
})
