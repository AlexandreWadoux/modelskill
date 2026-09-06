test_that("undefined correlation is separate from constant-model geometry", {
  obs <- 1:5
  mods <- list(Mean = rep(3, 5), Perfect = obs)
  stats <- diagram_stats(mods, obs)
  expect_equal(stats$r, c(NA_real_, 1))
  expect_equal(stats$sd_ratio, c(0, 1))
  expect_equal(stats$sde, c(1, 0))
  p <- gg_taylor(mods, obs)
  expect_true(is.na(p$data$Cor[1]))
  expect_equal(p$data$x, c(0, 1))
  expect_equal(p$data$y, c(0, 0))
  for (fn in list(gg_solar, gg_target)) {
    p <- fn(mods, obs)
    b <- ggplot2::ggplot_build(p)
    points <- which(vapply(p$layers, function(l)
      inherits(l$geom, "GeomPoint"), logical(1)))
    aesthetic <- if (identical(fn, gg_target)) "fill" else "colour"
    expect_equal(b$data[[points[1]]][[aesthetic]][1], "grey50")
    expect_true(is.na(p$data$r[1]))
  }
})

test_that("all-constant predictions render with default or supplied colours", {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 900, height = 800)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  obs <- 1:5
  for (fn in list(gg_taylor, gg_solar, gg_target)) {
    expect_warning(print(fn(rep(3, 5), obs, label = TRUE)), NA)
  }
  nse <- model_metrics(rep(3, 5), obs)$NSE
  for (fn in list(gg_solar, gg_target)) {
    expect_warning(print(fn(rep(3, 5), obs, colorval = nse,
                            colorval.name = "NSE", label = TRUE)), NA)
  }
})
