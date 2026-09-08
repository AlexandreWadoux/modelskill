test_that("metrics agree with analytical bias and anticorrelation cases", {
  obs <- 1:5
  bias <- model_metrics(obs + 1, obs)
  expect_equal(unname(unlist(bias[c("bias", "mae", "rmse", "correlation", "r2", "R2", "ccc", "Cb")])),
               c(-1, 1, 1, 1, 1, 0.5, 0.8, 0.8))
  reverse <- model_metrics(6 - obs, obs)
  expect_equal(reverse$correlation, -1)
  expect_equal(reverse$r2, 1)
  expect_equal(reverse$R2, -3)
  expect_equal(reverse$ccc, -1)
  expect_equal(reverse$Cb, 1)
})

test_that("concordance matches the original scale and location expression", {
  obs <- c(1, 2, 3, 4, 5)
  pred <- c(1, 2, 2, 5, 4)
  v <- sd(pred) / sd(obs)
  sx2 <- var(pred) * 4 / 5
  sy2 <- var(obs) * 4 / 5
  u <- (mean(pred) - mean(obs)) / (sx2 * sy2)^0.25
  cb <- 2 / (v + 1 / v + u^2)
  got <- model_metrics(pred, obs)
  expect_equal(got$Cb, cb, tolerance = 1e-12)
  expect_equal(got$ccc, cor(pred, obs) * cb, tolerance = 1e-12)
})

test_that("all APIs accept columns and preserve partial names", {
  obs <- 1:5
  mods <- list(first = obs, second = obs + 1)
  expect_equal(model_metrics(as.data.frame(mods), obs), model_metrics(mods, obs))
  expect_equal(model_metrics(as.matrix(as.data.frame(mods)), obs), model_metrics(mods, obs))
  expect_equal(rownames(model_metrics(list(named = obs, obs + 1), obs)),
               c("named", "Model 2"))
  for (fn in list(gg_taylor, gg_solar, gg_target)) {
    expect_s3_class(fn(as.data.frame(mods), obs), "ggplot")
    expect_s3_class(fn(cbind(obs), obs), "ggplot")
  }
})

test_that("invalid types, dimensions and nonfinite values fail early", {
  for (fn in list(model_metrics, diagram_stats, gg_taylor, gg_solar, gg_target)) {
    expect_error(fn(1:3, matrix(1:3)), "numeric vector")
    expect_error(fn(list(a = matrix(1:3)), 1:3), "numeric vector")
    expect_error(fn(c(1, Inf, 3), 1:3), "infinite")
    expect_error(fn(1:3, c(1, -Inf, 3)), "infinite")
    expect_error(fn(list(a = 1:3, a = 1:3), 1:3), "unique")
    expect_error(fn(1:3, 1:3, na.rm = 1), "TRUE or FALSE")
  }
  for (value in list(NA, Inf, "2", c(1, 2), -1, 1.2)) {
    expect_error(model_metrics(1:3, 1:3, digits = value), "digits")
  }
  expect_error(model_metrics(numeric(), numeric()), "non-empty|at least one")
})

test_that("missing values use each model's complete pairs consistently", {
  obs <- c(1, NA, 3, 5, 7)
  pred <- c(2, 2, NaN, 4, 8)
  keep <- complete.cases(pred, obs)
  expect_equal(model_metrics(pred, obs), model_metrics(pred[keep], obs[keep]))
  expect_equal(diagram_stats(pred, obs), diagram_stats(pred[keep], obs[keep]))
  for (fn in list(gg_taylor, gg_solar, gg_target)) {
    expect_s3_class(fn(pred, obs), "ggplot")
    expect_error(fn(pred, obs, na.rm = FALSE), "missing pairs")
    expect_error(fn(c(NA, NA, 2), 1:3), "two complete pairs")
    expect_error(fn(c(1, 2, NA), c(3, 3, 4)), "non-zero")
  }
  missing_metrics <- model_metrics(rep(NA_real_, 3), 1:3)
  expect_true(all(is.na(missing_metrics[vapply(missing_metrics, is.numeric, logical(1))])))
  one <- model_metrics(2, 1)
  expect_equal(one$bias, -1)
  expect_true(is.na(one$correlation))
})

test_that("constant predictions have an explicit finite convention", {
  m <- model_metrics(rep(3, 5), 1:5)
  expect_true(is.na(m$correlation))
  expect_true(is.na(m$r2))
  expect_equal(m$R2, 0)
  expect_equal(m$Cb, 0)
  expect_equal(m$ccc, 0)
  d <- diagram_stats(rep(3, 5), 1:5)
  expect_true(is.na(d$r))
  expect_equal(d$sde, 1)
  expect_equal(d$signed_sde, -1)
})

test_that("diagram coordinates match complete-data equations", {
  obs <- 1:5
  pred <- c(1, 2, 2, 5, 4)
  d <- diagram_stats(pred, obs)
  expected <- sqrt(1 + (sd(pred) / sd(obs))^2 -
                     2 * sd(pred) / sd(obs) * cor(pred, obs))
  obs_sd_pop <- sqrt(mean((obs - mean(obs))^2))
  expect_equal(d$sde, expected, tolerance = 1e-12)
  expect_equal(d$nME, mean(obs - pred) / obs_sd_pop, tolerance = 1e-12)
  expect_equal(d$signed_sde, expected * sign(sd(pred) - sd(obs)))
  expect_equal(d$n, 5L)
  expect_equal(gg_solar(pred, obs)$data$uRMSDnorm_sigmaD, d$sde)
  expect_equal(gg_target(pred, obs)$data$uRMSDnorm_sigmaD, d$signed_sde)
  t <- gg_taylor(pred, obs)$data
  expect_equal(t$x, d$sd_ratio * d$r)
  expect_equal(t$y, d$sd_ratio * sqrt(1 - d$r^2))
})

test_that("near-perfect and extreme-unit inputs remain numerically usable", {
  obs <- 1:5
  pred <- c(1, 2, 2, 5, 4)
  baseline <- model_metrics(pred, obs)
  for (scale in c(1e-200, 1e200)) {
    if (scale > 1) {
      expect_warning(got <- model_metrics(pred * scale, obs * scale),
                     "Some metrics exceed numeric precision")
      expect_true(is.infinite(got$mse))
    } else {
      got <- model_metrics(pred * scale, obs * scale)
    }
    expect_equal(got$rmse / scale, baseline$rmse)
    expect_equal(got$R2, baseline$R2)
    expect_equal(got$Cb, baseline$Cb)
    expect_equal(diagram_stats(pred * scale, obs * scale)$sde,
                 diagram_stats(pred, obs)$sde)
  }
  expect_equal(diagram_stats(obs, obs)$sde, 0)
  near <- diagram_stats(obs + c(0, 0, 1e-10, 0, 0), obs)
  expect_gt(near$sde, 0)
  expect_true(is.finite(near$sde))
})

test_that("plot controls reject unusable options", {
  for (fn in list(gg_taylor, gg_solar, gg_target)) {
    expect_error(fn(1:5, 1:5, label = 1), "label")
    expect_error(fn(1:5, 1:5, point_size = -1), "point_size")
    expect_error(fn(1:5, 1:5, label_size = Inf), "label_size")
  }
  for (fn in list(gg_solar, gg_target)) {
    expect_error(fn(1:5, 1:5, colorval = NA_real_), "finite")
    expect_error(fn(1:5, 1:5, colorval.name = 2), "character")
    expect_error(fn(1:5, 1:5, by = 1e-20), "10000")
    expect_error(fn(1:5, 1:5, by = "a"), "by")
  }
})

test_that("default palettes and annotation colours remain unchanged", {
  solar <- gg_solar(1:5, 1:5)
  built <- ggplot2::ggplot_build(solar)
  expect_equal(unname(built$plot$scales$get_scales("fill")$map(
    c("r>0", "r>0.7", "r>0.9", "r>0.95"))),
    c("#FEECA4", "#FEF4B6", "#FFFCD7", "#FFFFE5"))
  expect_equal(solar$scales$get_scales("colour")$palette(c(0, 1)),
    viridis::scale_color_viridis(option = "A")$palette(c(0, 1)))
  target <- gg_target(1:5, 1:5)
  expect_equal(target$scales$get_scales("fill")$palette(c(0, 1)),
    viridis::scale_fill_viridis(option = "A")$palette(c(0, 1)))
  taylor <- gg_taylor(1:5, 1:5)
  expect_true(any(vapply(taylor$layers, function(x)
    identical(x$aes_params$colour, "red3"), logical(1))))
})

test_that("plots actually render and support themes, scales and annotations", {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 900, height = 800)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  obs <- 1:10
  mods <- list(accurate = obs + c(rep(0, 9), 0.2), biased = obs + 1)
  for (fn in list(gg_taylor, gg_solar, gg_target)) {
    for (label in c(FALSE, TRUE)) {
      p <- fn(mods, obs, label = label)
      expect_warning(print(p), NA)
      changed <- p + ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::labs(title = "Comparison") +
        ggplot2::annotate("point", x = 0, y = 0)
      expect_warning(print(changed), NA)
      expect_s3_class(changed, "ggplot")
    }
  }
  p <- gg_solar(mods, obs) + ggplot2::scale_colour_gradient(low = "blue", high = "red")
  expect_warning(print(p), NA)
  p <- gg_target(mods, obs) + ggplot2::scale_fill_gradient(low = "blue", high = "red")
  expect_warning(print(p), NA)
})

test_that("label rendering preserves the random-number state", {
  set.seed(123)
  initial <- .Random.seed
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 900, height = 800)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  for (fn in list(gg_taylor, gg_solar, gg_target)) {
    print(fn(list(a = 1:5, b = c(1, 2, 2, 5, 4)), 1:5, label = TRUE))
    expect_identical(.Random.seed, initial)
  }
})

test_that("equal variance retains the original positive target sign", {
  d <- diagram_stats(5:1, 1:5)
  expect_equal(d$sd_ratio, 1)
  expect_equal(d$r, -1)
  expect_equal(d$sde, 2)
  expect_equal(d$signed_sde, 2)
  d <- diagram_stats(list(bias = 2:6, perfect = 1:5), 1:5)
  expect_equal(d$sde, c(0, 0), tolerance = 1e-14)
})

test_that("diagram normalization gives exact finite-sample RMSE geometry", {
  for (case in list(
    list(obs = 1:5, pred = c(1, 2, 2, 5, 4)),
    list(obs = c(1, 3), pred = c(1, 4))
  )) {
    d <- diagram_stats(case$pred, case$obs)
    m <- model_metrics(case$pred, case$obs)
    obs_sd_pop <- sqrt(mean((case$obs - mean(case$obs))^2))

    expect_equal(
      d$nME^2 + d$sde^2,
      m$rmse^2 / obs_sd_pop^2,
      tolerance = 1e-12
    )
    expect_equal(
      sqrt(d$nME^2 + d$sde^2),
      m$rmse / obs_sd_pop,
      tolerance = 1e-12
    )
    expect_equal(
      1 - d$nME^2 - d$sde^2,
      m$R2,
      tolerance = 1e-12
    )
  }
})

test_that("custom axes preserve their specified reference endpoints", {
  p <- gg_solar(1:5, 1:5, x.axis_begin = -1.1, x.axis_end = 1.1,
                y.axis_end = 1.9, by = 0.1)
  segments <- Filter(function(l) inherits(l$geom, "GeomSegment"), p$layers)
  expect_true(any(vapply(segments, function(l)
    is.data.frame(l$data) && "xend" %in% names(l$data) &&
      any(l$data$xend == 1.1), logical(1))))
  p <- gg_target(1:5, 1:5, axis_begin = -2.2, axis_end = 2.2, by = 0.2)
  expect_warning(ggplot2::ggplotGrob(p), NA)
})

test_that("unrepresentable results are explicitly reported", {
  expect_error(model_metrics(c(1e300, 2e300), c(1e-300, 2e-300)),
               "numeric precision")
  expect_warning(model_metrics(c(-1e308, -9e307), c(1e308, 9e307)),
                 "numeric precision")
})