# Mathematical oracle: integrate constant CDF/indicator pieces at their
# breakpoints. This deliberately does not use the production clipping method.
decomposition_by_thresholds <- function(obs, draws) {
  n <- length(obs)
  m <- ncol(draws)
  sorted <- lapply(seq_len(n), function(i) sort(draws[i, ]))
  reliability <- potential <- 0
  for (j in seq_len(m - 1L)) {
    width <- observed_mass <- numeric(n)
    for (i in seq_len(n)) {
      limits <- sorted[[i]][c(j, j + 1L)]
      width[i] <- diff(limits)
      cuts <- sort(unique(c(limits, obs[i][obs[i] > limits[1] & obs[i] < limits[2]])))
      if (length(cuts) > 1L) {
        midpoints <- (head(cuts, -1L) + tail(cuts, -1L)) / 2
        observed_mass[i] <- sum(diff(cuts)[midpoints >= obs[i]])
      }
    }
    weight <- mean(width)
    if (weight > 0) {
      frequency <- mean(observed_mass) / weight
      reliability <- reliability + weight * (frequency - j / m)^2
      potential <- potential + weight * frequency * (1 - frequency)
    }
  }
  for (tail in c("lower", "upper")) {
    boundary <- vapply(sorted, function(x) if (tail == "lower") min(x) else max(x), 0)
    outside <- if (tail == "lower") obs <= boundary else obs > boundary
    distance <- abs(obs - boundary)
    distance[!outside] <- 0
    frequency <- mean(outside)
    if (frequency > 0) {
      weight <- mean(distance) / frequency
      reliability <- reliability + weight * frequency^2
      potential <- potential + weight * frequency * (1 - frequency)
    }
  }
  c(reliability = reliability, potential_crps = potential)
}

empirical_crps_pairwise <- function(obs, draws) {
  mean(vapply(seq_along(obs), function(i) {
    forecast <- draws[i, ]
    mean(abs(forecast - obs[i])) - mean(abs(outer(forecast, forecast, "-"))) / 2
  }, numeric(1)))
}

test_that("CRPS components match hand-calculated interior and exterior cases", {
  draws <- matrix(c(0, 2), nrow = 1)
  cases <- list(
    list(y = 0, expected = c(1/2, 1/2, 0)),
    list(y = 1, expected = c(1/2, 0, 1/2)),
    list(y = 2, expected = c(1/2, 1/2, 0)),
    list(y = -1, expected = c(3/2, 3/2, 0)),
    list(y = 3, expected = c(3/2, 3/2, 0))
  )
  for (case in cases) {
    expect_silent(result <- crps_decomposition(case$y, draws))
    expect_identical(names(result), c("crps", "reliability", "potential_crps"))
    expect_equal(unname(unlist(result)), case$expected, tolerance = 1e-12)
    expect_true(all(vapply(result, is.double, logical(1))))
    expect_identical(dim(result), c(1L, 3L))
  }
  tails <- crps_decomposition(c(-1, 1, 3), draws[rep(1, 3), , drop = FALSE])
  expect_equal(unname(unlist(tails)), c(7/6, 2/9, 17/18), tolerance = 1e-12)
  repeated <- crps_decomposition(c(-1, 0, 2), matrix(0, 3, 4))
  expect_equal(unname(unlist(repeated)), c(1, 4/9, 5/9), tolerance = 1e-12)
  expect_equal(unname(unlist(crps_decomposition(rep(2, 5), matrix(2, 5, 8)))),
               c(0, 0, 0))
})

test_that("CRPS and both components agree with independent mathematical oracles", {
  set.seed(9092026)
  for (n in c(1, 2, 7, 21)) {
    for (m in c(2, 3, 8, 17)) {
      for (kind in c("continuous", "rounded", "degenerate")) {
        for (replicate in seq_len(3)) {
          obs <- stats::rnorm(n, sd = 2)
          draws <- matrix(stats::rnorm(n * m), nrow = n)
          if (kind == "rounded") {
            obs <- round(obs)
            draws <- round(draws)
          } else if (kind == "degenerate") {
            draws <- matrix(rep(stats::rnorm(n), m), nrow = n)
          }
          expected_score <- empirical_crps_pairwise(obs, draws)
          expected_parts <- decomposition_by_thresholds(obs, draws)
          got <- crps_decomposition(obs, draws)
          expect_equal(got$crps, expected_score, tolerance = 1e-12)
          expect_equal(unlist(got[c("reliability", "potential_crps")]),
                       expected_parts, tolerance = 1e-12)
          expect_equal(got$crps, crps(obs, draws), tolerance = 1e-12)
          expect_equal(got$crps, got$reliability + got$potential_crps,
                       tolerance = 1e-12)
          expect_true(all(unlist(got) >= 0))
          expect_lte(got$reliability, got$crps + 1e-12)
        }
      }
    }
  }
})

test_that("all tied positions and zero-width bins contribute correctly", {
  for (members in list(c(-2, -1, 0, 3), c(0, 0, 0, 2), c(-1, 1, 1, 1))) {
    obs <- rep(members, each = 2)
    draws <- matrix(rep(members, each = length(obs)), nrow = length(obs))
    got <- crps_decomposition(obs, draws)
    expect_equal(got$crps, empirical_crps_pairwise(obs, draws))
    expect_equal(unlist(got[-1]), decomposition_by_thresholds(obs, draws))
  }
})

test_that("decomposition is invariant to representation, order and common translation", {
  obs <- c(-3.2, 0.7, 3.1, 2.6)
  draws <- matrix(c(-1, 0, 1, 2, 0, 1, 2, 3, 1, 2, 3, 4), nrow = 4)
  original <- crps_decomposition(obs, draws)
  expect_equal(crps_decomposition(obs, as.data.frame(draws)), original)
  expect_equal(crps_decomposition(obs, draws[, c(3, 1, 2)]), original)
  rows <- c(4, 2, 1, 3)
  expect_equal(crps_decomposition(obs[rows], draws[rows, ]), original)
  rows <- rep(seq_along(obs), 3)
  expect_equal(crps_decomposition(obs[rows], draws[rows, ]), original)
  expect_equal(crps_decomposition(obs + 123, draws + 123), original, tolerance = 1e-12)
  for (scale in c(0.001, 2, 1e6)) {
    expect_equal(crps_decomposition(obs * scale, draws * scale),
                 original * scale, tolerance = 1e-12)
  }
  # Reflection preserves the components here because no observation is tied
  # to an extreme member; the documented tail tie convention is asymmetric.
  expect_equal(crps_decomposition(-obs, -draws), original, tolerance = 1e-12)
  expect_equal(crps_decomposition(obs, draws[, rep(1:3, each = 2)]), original,
               tolerance = 1e-12)
  before <- list(obs = obs, draws = draws, seed = .Random.seed)
  invisible(crps_decomposition(obs, draws))
  expect_identical(obs, before$obs)
  expect_identical(draws, before$draws)
  expect_identical(.Random.seed, before$seed)
})

test_that("incomplete distribution rows are filtered consistently", {
  obs <- c(0, NA, 2, 3, 4)
  draws <- matrix(rep(0:4, 3), nrow = 5)
  draws[3, 2] <- NA_real_
  draws[4, 3] <- NaN
  expect_silent(got <- crps_decomposition(obs, draws))
  expect_equal(got, crps_decomposition(obs[c(1, 5)], draws[c(1, 5), ]))
  expect_silent(missing <- crps_decomposition(obs, draws, na.rm = FALSE))
  expect_identical(missing, data.frame(crps = NA_real_, reliability = NA_real_,
                                      potential_crps = NA_real_))
  expect_identical(crps_decomposition(NA_real_, matrix(1, 1, 2)), missing)
  expect_identical(crps_decomposition(numeric(), matrix(numeric(), 0, 2)), missing)
  expect_equal(crps_decomposition(c(NA, 1), matrix(c(0, 0, 2, 2), 2)),
               crps_decomposition(1, matrix(c(0, 2), 1)))
})

test_that("malformed decomposition inputs fail with clear errors", {
  expect_error(crps_decomposition("a", matrix(1, 1, 2)), "numeric vector")
  expect_error(crps_decomposition(1, c(0, 2)), "matrix or data frame")
  expect_error(crps_decomposition(1, data.frame(a = "a", b = "b")), "numeric matrix")
  expect_error(crps_decomposition(1:2, matrix(1, 1, 2)), "same number of rows")
  expect_error(crps_decomposition(1, matrix(1, 1, 1)), "too few")
  expect_error(crps_decomposition(1, matrix(numeric(), 1, 0)), "too few")
  expect_error(crps_decomposition(Inf, matrix(1, 1, 2)), "infinite")
  expect_error(crps_decomposition(1, matrix(c(0, Inf), 1)), "infinite")
  for (flag in list(NA, 1, "yes", c(TRUE, FALSE))) {
    expect_error(crps_decomposition(1, matrix(c(0, 2), 1), na.rm = flag), "TRUE or FALSE")
  }
})
