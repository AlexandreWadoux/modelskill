test_that("undefined prediction metrics return NA with informative warnings", {
  expect_warning(expect_true(is.na(msle(c(-1, 1), c(0, 1)))),
                 "MSLE is undefined because observations or predictions contain negative values")
  expect_warning(expect_true(is.na(rmsle(c(1, 2), c(1, -1)))),
                 "RMSLE is undefined because observations or predictions contain negative values")
  expect_warning(expect_true(is.na(mape(c(0, 1), c(0, 1)))),
                 "MAPE is undefined because observations contain zero values")
  expect_warning(expect_true(is.na(mpe(c(0, 1), c(0, 1)))),
                 "MPE is undefined because observations contain zero values")
  expect_warning(expect_true(is.na(rrmse(c(-1, 1), c(-1, 1)))),
                 "RRMSE is undefined because the mean of the observations is zero")
  expect_warning(expect_true(is.na(rae(rep(1, 3), 1:3))),
                 "RAE is undefined because the observations are constant")
  expect_warning(expect_true(is.na(nrmse(1, 1))), "NRMSE is undefined because fewer than two")
  expect_warning(expect_true(is.na(nrmse(rep(1, 3), 1:3))), "NRMSE is undefined because the observations have zero variance")
  expect_warning(expect_true(is.na(nse(1, 1))), "NSE is undefined because fewer than two")
  expect_warning(expect_true(is.na(mec(rep(1, 3), 1:3))), "MEC is undefined because the observations have zero variance")
  expect_warning(expect_true(is.na(R2(rep(1, 3), 1:3))), "R2 is undefined because the observations have zero variance")
  expect_warning(expect_true(is.na(correlation(1, 1))), "Correlation is undefined because fewer than two")
  expect_warning(expect_true(is.na(r2(1:3, rep(1, 3)))), "r2 is undefined because the predictions have zero variance")
  expect_warning(expect_true(is.na(sd_ratio(1, 1))), "SD ratio is undefined because fewer than two")
  expect_warning(expect_true(is.na(kge(1, 1))), "KGE is undefined because fewer than two")
  expect_warning(expect_true(is.na(kge(c(-1, 1), c(-1, 1)))), "KGE is undefined because the mean of the observations is zero")
  expect_warning(expect_true(is.na(kge(rep(1, 3), 1:3))), "KGE is undefined because the observations have zero variance")
  expect_warning(expect_true(is.na(kge(1:3, rep(1, 3)))), "KGE is undefined because the predictions have zero variance")
  expect_warning(expect_true(is.na(sep(1, 1))), "SEP is undefined because fewer than two")
  expect_warning(expect_true(is.na(willmott_d(rep(1, 3), rep(1, 3)))),
                 "Willmott's d is undefined because the potential-error denominator is zero")
})

test_that("perfect ratio metrics return Inf when their numerator is positive", {
  obs <- 1:4
  expect_identical(rpd(obs, obs), Inf)
  expect_identical(rpiq(obs, obs), Inf)
  expect_identical(rer(obs, obs), Inf)
  expect_silent(rpd(obs, obs))
  expect_silent(rpiq(obs, obs))
  expect_silent(rer(obs, obs))
  expect_warning(expect_true(is.na(rpd(rep(1, 3), rep(1, 3)))),
                 "RPD is undefined because the observations have zero variance and RMSE is zero")
  expect_warning(expect_true(is.na(rpiq(rep(1, 3), rep(1, 3)))),
                 "RPIQ is undefined because the observation interquartile range and RMSE are both zero")
  expect_warning(expect_true(is.na(rer(rep(1, 3), rep(1, 3)))),
                 "RER is undefined because the observation range and RMSE are both zero")
})

test_that("missing-data handling warns only when no valid pairs or na.rm is FALSE", {
  expect_silent(expect_equal(mape(c(1, NA, 3), c(1, 2, 2)), mape(c(1, 3), c(1, 2))))
  expect_silent(expect_equal(nse(c(1, NA, 3), c(1, 2, 2)), nse(c(1, 3), c(1, 2))))
  expect_warning(expect_true(is.na(rmse(c(NA_real_, NA_real_), c(NA_real_, NA_real_)))),
                 "RMSE is undefined because no valid observation-prediction pairs remain")
  expect_warning(expect_true(is.na(kge(c(NA_real_, NA_real_), c(NA_real_, NA_real_)))),
                 "KGE is undefined because no valid observation-prediction pairs remain")
  expect_warning(expect_true(is.na(mae(c(1, NA), c(1, 2), na.rm = FALSE))),
                 "MAE is undefined because inputs contain missing values and `na.rm = FALSE`")
})

test_that("valid prediction metrics retain values without warnings", {
  obs <- c(1, 2, 3, 4)
  pred <- c(1, 2, 2, 5)
  expect_silent(expect_equal(nse(obs, pred), 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)))
  expect_silent(expect_equal(rpd(obs, pred), stats::sd(obs) / rmse(obs, pred)))
  expect_silent(expect_true(is.finite(kge(obs, pred))))
})
