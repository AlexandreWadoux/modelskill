test_that("every reported metric agrees with explicit formula references", {
  o <- c(1, 2, 4, 8)
  p <- c(2, 2, 3, 6)
  e <- c(-1, 0, 1, 2)
  v <- mean((o - mean(o))^2)
  w <- mean((p - mean(p))^2)
  covar <- mean((o - mean(o)) * (p - mean(p)))
  den <- v + w + (mean(o) - mean(p))^2
  r <- covar / sqrt(v * w)
  expected <- c(bias = .5, mae = 1, mse = 1.5, rmse = sqrt(1.5),
    nrmse = sqrt(1.5) / sd(o), crmse = sqrt(1.25), correlation = r,
    r2 = r^2, R2 = 1 - 6 / sum((o - mean(o))^2), sd_ratio = sqrt(w/v),
    ccc = 2 * covar / den, Cb = 2 * sqrt(v*w) / den,
    mdae = 1, rpd = sd(o)/sqrt(1.5), rpiq = 3.25/sqrt(1.5),
    sep = sqrt(5/3), rer = 7/sqrt(1.5), mape = 37.5, mpe = -12.5,
    smape = 100 * (2/3 + 2/7 + 2/7)/4,
    msle = mean((log1p(o)-log1p(p))^2),
    rmsle = sqrt(mean((log1p(o)-log1p(p))^2)), rae = 1/2.25,
    rrmse = 100 * sqrt(1.5)/3.75,
    willmott_d = 1 - 6/sum((abs(p-3.75)+abs(o-3.75))^2),
    kge = 1-sqrt((r-1)^2+(sqrt(w/v)-1)^2+(mean(p)/mean(o)-1)^2))
  got <- model_metrics(p, o, extended = TRUE)
  expect_identical(names(got), c("model", names(expected)))
  expect_equal(unlist(got[1, -1], use.names = FALSE), unname(expected), tolerance = 1e-12)
  for (nm in setdiff(names(expected), "Cb")) {
    f <- get(nm, asNamespace("modelskill"))
    expect_equal(f(o, p), expected[[nm]], tolerance = 1e-12, info = nm)
  }
  expect_equal(got$mse, got$bias^2 + got$crmse^2)
  expect_equal(got$R2, 1 - 4/3 * got$nrmse^2)
})

test_that("all columns use the same retained pairs and edge-case policies", {
  o <- c(1, 2, 4, 8); p <- c(2, 2, 3, 6)
  base <- model_metrics(p, o, extended = TRUE)
  expect_equal(model_metrics(c(p, NA), c(o, 0), extended = TRUE), base)
  missing <- model_metrics(c(p, NA), c(o, 0), extended = TRUE, na.rm = FALSE)
  expect_true(all(is.na(missing[-1])))
  expect_true(all(is.na(model_metrics(NA_real_, NA_real_, extended = TRUE)[-1])))
  expect_warning(expect_true(is.na(willmott_d(rep(2, 3), rep(2, 3)))), "denominator is zero")
  expect_equal(willmott_d(1:3, 3:1), 0)
  expect_equal(mpe(c(1, 2), c(2, 4)), -100)
  expect_equal(smape(c(0, 1), c(0, -1)), 100)
  expect_warning(expect_true(is.na(kge(1:3, rep(2, 3)))), "predictions have zero variance")
  for (f in list(mpe, rrmse, willmott_d)) {
    expect_error(f(1:2, 1:3), "same length")
    expect_error(f(1:2, c(1, Inf)), "infinite")
  }
})
