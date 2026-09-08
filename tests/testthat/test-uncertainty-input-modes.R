interval_functions <- list(picp, coverage, coverage_error, interval_width,
                           interval_score, uncertainty_metrics)

test_that("normal interval inputs reproduce explicit central bounds", {
  obs <- c(-3, 0, 2, 6)
  pred <- c(-1, 0, 1, 2)
  sd <- c(.5, 1, 2, 3)
  for (level in c(.5, .8, .95)) {
    lower <- pred + qnorm((1 - level) / 2) * sd
    upper <- pred + qnorm((1 + level) / 2) * sd
    for (f in interval_functions) {
      expect_no_warning(actual <- f(obs, pred = pred, predictive_sd = sd, level = level))
      expect_equal(actual, f(obs, lower, upper, level = level), tolerance = 1e-12)
    }
  }
  expect_named(uncertainty_metrics(obs, pred = pred, predictive_sd = sd),
               c("picp", "picp_error", "interval_width", "interval_score"))
  # Existing positional na.rm arguments retain their meaning.
  expect_identical(picp(obs, pred - 1, pred + 1, FALSE), .5)
  expect_identical(coverage(obs, pred - 1, pred + 1, FALSE), .5)
  expect_identical(interval_width(obs, pred - 1, pred + 1, FALSE), 2)
})

test_that("sample intervals use equal-tailed type-7 quantiles", {
  obs <- c(0, 6, 12)
  draws <- rbind(c(0, 1, 2, 3, 4), c(2, 4, 6, 8, 10), c(5, 6, 7, 8, 9))
  lower <- c(1, 4, 6)
  upper <- c(3, 8, 8)
  for (f in interval_functions) {
    expect_no_warning(value <- f(obs, distribution = draws, level = .5))
    expect_equal(value, f(obs, lower, upper, level = .5))
    expect_equal(value, f(obs, distribution = as.data.frame(draws), level = .5))
  }
  result <- uncertainty_metrics(obs, distribution = draws, level = .5)
  expect_equal(unlist(result, use.names = FALSE), c(1 / 3, -1 / 6, 8 / 3, 28 / 3))
  # One observation, one draw, ties, and zero-width intervals are well-defined.
  expect_equal(uncertainty_metrics(2, distribution = matrix(2)),
               uncertainty_metrics(2, 2, 2))
  expect_equal(picp(3, distribution = matrix(2)), 0)
  expect_equal(interval_score(3, distribution = matrix(2), level = .5), 4)
})

test_that("quantile and reliability curves agree with explicit sample quantiles", {
  obs <- c(0, 6, 12)
  draws <- rbind(0:4, seq(2, 10, 2), 5:9)
  levels <- c(.25, .5, .75)
  quantiles <- t(apply(draws, 1, quantile, probs = levels, names = FALSE))
  expect_equal(qcp(obs, distribution = draws, levels = levels),
               qcp(obs, quantiles, levels))
  expect_equal(qcp(obs, distribution = draws, levels = levels),
               c(`0.25` = 1/3, `0.5` = 2/3, `0.75` = 2/3))
  expect_length(qcp(obs, distribution = draws), 19)
  expect_identical(names(qcp(obs, distribution = draws)),
                   as.character(seq(.05, .95, .05)))
  expect_equal(qcp(2, distribution = matrix(2), levels = .5), c(`0.5` = 1))
  expect_equal(qcp(obs, distribution = draws, levels = c(.75, .25, .25)),
               qcp(obs, distribution = draws, levels = c(.25, .75)))

  lower <- lapply(levels, function(p) apply(draws, 1, quantile, probs = (1-p)/2))
  upper <- lapply(levels, function(p) apply(draws, 1, quantile, probs = (1+p)/2))
  names(lower) <- names(upper) <- as.character(levels)
  expect_equal(gg_coverage(obs, distribution = draws, levels = levels)$data,
               gg_coverage(obs, lower, upper)$data)
  expect_equal(accuracy_plot_metrics(obs, distribution = draws, levels = levels),
               accuracy_plot_metrics(obs, lower, upper))
  expect_equal(nrow(gg_coverage(obs, distribution = draws)$data), 99L)
})

test_that("normal quantile calculation and plot share defaults and values", {
  obs <- c(-2, 0, 3)
  pred <- c(-1, 1, 2)
  sd <- c(.5, 1, 2)
  levels <- c(.05, .5, .95)
  quantiles <- vapply(levels, function(p) pred + qnorm(p) * sd, numeric(3))
  expect_equal(qcp(obs, pred = pred, predictive_sd = sd, levels = levels),
               qcp(obs, quantiles, levels))
  expected <- qcp(obs, pred = pred, predictive_sd = sd)
  expect_length(expected, 19L)
  expect_equal(gg_qcp(obs, pred = pred, predictive_sd = sd)$data$qcp,
               unname(expected))
  expect_equal(qcp(0, pred = 0, predictive_sd = 1, levels = .5), c(`0.5` = 1))
})

test_that("sample PIT is the inclusive empirical CDF, including ties", {
  draws <- rbind(c(0, 0, 2, 2), 1:4, 1:4, 1:4)
  obs <- c(0, -1, 4, 2)
  expect_identical(pit(obs = obs, distribution = draws), c(.5, 0, 1, .5))
  expect_equal(pit(obs = obs, distribution = as.data.frame(draws)),
               pit(c(.5, 0, 1, .5)))
  expect_equal(pit(obs = 2, distribution = matrix(2)), 1)
})

test_that("missing cases use complete rows and PIW remains observation independent", {
  obs <- c(1, NA, 3, 4)
  draws <- rbind(0:4, seq(0, 40, 10), c(1, 2, NA, 4, 5), 2:6)
  for (f in interval_functions) {
    keep <- if (identical(f, interval_width)) c(1, 2, 4) else c(1, 4)
    expect_no_warning(actual <- f(obs, distribution = draws))
    expect_equal(actual, f(obs[keep], distribution = draws[keep, , drop = FALSE]))
    expect_true(all(is.na(f(obs, distribution = draws, na.rm = FALSE))))
  }
  expect_equal(interval_width(c(1, NA), pred = c(1, 1), predictive_sd = c(1, 10)),
               mean(2 * qnorm(.975) * c(1, 10)))
  expect_equal(uncertainty_metrics(c(1, NA), pred = c(1, 1),
                                 predictive_sd = c(1, 10))$interval_width,
               2 * qnorm(.975))
  expect_no_warning(expect_equal(interval_width(NA_real_, distribution = matrix(0:4, 1),
                                                na.rm = FALSE), 3.8))
  expect_equal(qcp(obs, distribution = draws), qcp(obs[c(1, 4)], distribution = draws[c(1, 4), ]))
  expect_equal(pit(obs = obs, distribution = draws), c(.4, .6))
  expect_identical(pit(obs = obs, distribution = draws, na.rm = FALSE), rep(NA_real_, 4))
  for (f in list(gg_coverage, gg_qcp)) {
    expect_equal(f(obs, distribution = draws)$data,
                 f(obs[c(1, 4)], distribution = draws[c(1, 4), ])$data)
    expect_true(all(is.na(f(obs, distribution = draws, na.rm = FALSE)$data[, 2])))
  }
  expect_equal(accuracy_plot_metrics(obs, distribution = draws),
               accuracy_plot_metrics(obs[c(1, 4)], distribution = draws[c(1, 4), ]))
  expect_warning(result <- accuracy_plot_metrics(obs, distribution = draws, na.rm = FALSE),
                 "missing values")
  expect_true(all(is.na(result)))
})

test_that("empty sample rows and all-missing inputs preserve result structures", {
  empty <- matrix(numeric(), nrow = 0, ncol = 2)
  for (f in interval_functions) {
    expect_true(all(is.na(f(numeric(), distribution = empty))))
    expect_true(all(is.na(f(1, distribution = matrix(NA_real_, 1, 2)))))
  }
  expect_identical(pit(obs = numeric(), distribution = empty), numeric())
  expect_identical(pit(obs = NA_real_, distribution = matrix(1)), numeric())
  expect_true(all(is.na(qcp(numeric(), distribution = empty))))
  expect_true(all(is.na(qcp(NA_real_, pred = 0, predictive_sd = 1))))
  for (f in list(gg_coverage, gg_qcp)) {
    expect_true(all(is.na(f(numeric(), distribution = empty)$data[, 2])))
  }
  expect_warning(result <- accuracy_plot_metrics(numeric(), distribution = empty),
                 "no complete")
  expect_true(all(is.na(result)))
})

test_that("new predictive modes reject conflicting and malformed inputs", {
  functions <- c(interval_functions, list(qcp, gg_qcp, gg_coverage, accuracy_plot_metrics))
  for (f in functions) {
    expect_error(f(1:3, pred = 1:3, predictive_sd = rep(1, 3), distribution = matrix(1, 3, 2)),
                 "not both")
    expect_error(f(1:3, pred = 1:3), "requires")
    expect_error(f(1:3, predictive_sd = rep(1, 3)), "requires")
    for (bad in c(0, -1, Inf)) {
      expect_error(f(1:3, pred = 1:3, predictive_sd = c(1, bad, 1)), "strictly positive")
    }
    expect_error(f(1:3, pred = 1:2, predictive_sd = rep(1, 3)), "same length")
    expect_error(f(1:3, distribution = matrix(1, 2, 3)), "same number of rows")
    expect_error(f(1:3, distribution = matrix(1, 3, 0)), "too few")
    expect_error(f(1:3, distribution = matrix("a", 3, 2)), "numeric matrix")
    expect_error(f(1:3, distribution = matrix(Inf, 3, 2)), "infinite")
    expect_error(f(1:3, distribution = matrix(1, 3, 2), na.rm = 1), "TRUE or FALSE")
  }
  for (f in interval_functions) {
    expect_error(f(1:3, lower = 0:2, upper = 2:4, distribution = matrix(1, 3, 2)), "not both")
    expect_error(f(1:3, lower = 0:2), "both")
    expect_error(f(1:3, upper = 2:4), "both")
    for (bad in list(0, 1, NA_real_, Inf, c(.5, .9))) {
      expect_error(f(1:3, distribution = matrix(1, 3, 2), level = bad), "between")
    }
  }
  for (f in list(qcp, gg_qcp, gg_coverage, accuracy_plot_metrics)) {
    for (bad in list(0, 1, NA_real_, Inf, numeric(), "bad")) {
      expect_error(f(1:3, distribution = matrix(1, 3, 2), levels = bad), "levels")
    }
  }
  for (f in list(qcp, gg_qcp)) {
    expect_error(f(1:3, quantiles = matrix(1, 3), levels = .5,
                   distribution = matrix(1, 3, 2)), "not both")
  }
  for (f in list(gg_coverage, accuracy_plot_metrics)) {
    expect_error(f(1:3, lower = list(`0.5` = 0:2), upper = list(`0.5` = 2:4),
                   distribution = matrix(1, 3, 2)), "not both")
    expect_error(f(1:3, distribution = matrix(1, 3, 2), level = .5), "Use `levels`")
  }
  expect_error(pit(distribution = matrix(1)), "requires `obs`")
  expect_error(pit(.5, obs = 1, distribution = matrix(1)), "not both")
  expect_error(pit(obs = 1, pred = 1, distribution = matrix(1)), "not both")
  expect_error(pit(obs = 1:3, distribution = matrix(1)), "same number of rows")
  expect_error(pit(obs = 1, distribution = matrix(Inf)), "infinite")
  expect_error(pit(obs = 1, distribution = matrix(1), na.rm = 1), "TRUE or FALSE")
})

test_that("sample plots preserve design and ordinary ggplot additions", {
  draws <- rbind(0:4, 1:5, 2:6)
  obs <- 1:3
  for (f in list(gg_coverage, gg_qcp)) {
    for (levels in list(.5, c(.25, .5, .75))) {
      p <- f(obs, distribution = draws, levels = levels)
      expect_s3_class(p, "ggplot")
      expect_equal(p$data$nominal, levels)
      expect_no_warning(built <- ggplot2::ggplot_build(p))
      expect_equal(built$data[[1]]$colour, "grey50")
      expect_true(all(built$data[[2]]$colour == "red3"))
      modified <- p + ggplot2::theme_minimal() + ggplot2::labs(title = "Calibration") +
        ggplot2::annotate("text", x = .2, y = .8, label = "Test")
      expect_no_warning(ggplot2::ggplot_build(modified))
      expect_s3_class(modified, "ggplot")
      expect_message(rescaled <- p + ggplot2::scale_x_continuous(breaks = c(0, .5, 1)),
                     "Scale for x")
      expect_no_warning(ggplot2::ggplot_build(rescaled))
    }
  }
})

test_that("large normal predictive samples approximate the analytic modes", {
  set.seed(20260909)
  n <- 80
  pred <- seq(-2, 2, length.out = n)
  sd <- seq(.5, 1.5, length.out = n)
  obs <- pred + seq(-2.7, 2.7, length.out = n) * sd
  draws <- matrix(rnorm(n * 10000, mean = pred, sd = sd), nrow = n)
  for (f in interval_functions) {
    analytic <- f(obs, pred = pred, predictive_sd = sd, level = .8)
    sampled <- f(obs, distribution = draws, level = .8)
    expect_equal(sampled, analytic, tolerance = .04)
  }
  levels <- c(.1, .3, .5, .8, .95)
  expect_equal(qcp(obs, distribution = draws, levels = levels),
               qcp(obs, pred = pred, predictive_sd = sd, levels = levels), tolerance = .03)
  expect_equal(pit(obs = obs, distribution = draws),
               pit(obs = obs, pred = pred, predictive_sd = sd), tolerance = .02)
  for (f in list(gg_coverage, gg_qcp)) {
    sampled <- f(obs, distribution = draws, levels = levels)$data
    analytic <- f(obs, pred = pred, predictive_sd = sd, levels = levels)$data
    expect_identical(sampled$nominal, analytic$nominal)
    # Compare coverage on an absolute probability scale, not relative error
    # near zero: one validation case changes the proportion by 1/n.
    expect_lt(max(abs(sampled[, 2] - analytic[, 2])), .03)
  }
  expect_equal(accuracy_plot_metrics(obs, distribution = draws, levels = levels),
               accuracy_plot_metrics(obs, pred = pred, predictive_sd = sd, levels = levels),
               tolerance = .04)
})
