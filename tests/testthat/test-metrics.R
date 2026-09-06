test_that("metrics match the original definitions for complete data", {
  obs <- c(1, 2, 3, 4, 5)
  pred <- c(1, 2, 2, 5, 4)
  got <- model_metrics(pred, obs)

  expect_equal(got$ME, mean(obs - pred))
  expect_equal(got$MAE, mean(abs(obs - pred)))
  expect_equal(got$RMSE, sqrt(mean((obs - pred)^2)))
  expect_equal(got$r, cor(pred, obs))
  expect_equal(got$r2, cor(pred, obs)^2)
  expect_equal(got$NSE, 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2))
})

test_that("multiple models and names are returned in rows", {
  obs <- 1:5
  got <- model_metrics(list(first = obs, second = obs + 1), obs)
  expect_s3_class(got, "data.frame")
  expect_equal(dim(got), c(2L, 8L))
  expect_equal(rownames(got), c("first", "second"))
  expect_equal(got$r, c(1, 1))
})

test_that("missing values use complete pairs", {
  got <- model_metrics(c(1, NA, 3), c(1, 2, 4))
  expect_equal(got$ME, mean(c(1, 4) - c(1, 3)))
})

test_that("invalid inputs give informative errors", {
  expect_error(model_metrics(c(1, 2), 1:3), "same length")
  expect_error(model_metrics(list(a = c(1, 2), b = "bad"), 1:2), "Every element")
  expect_error(model_metrics(c(1, 2), 1:2, digits = 1.5), "non-negative integer")
})

test_that("rounding is optional", {
  full <- model_metrics(c(1, 2, 2, 5, 4), 1:5)
  rounded <- model_metrics(c(1, 2, 2, 5, 4), 1:5, digits = 2)
  expect_false(identical(full$RMSE, rounded$RMSE))
  expect_equal(rounded$RMSE, round(full$RMSE, 2))
})

test_that("single unnamed vectors receive a model name", {
  got <- model_metrics(1:5, 1:5)
  expect_equal(rownames(got), "Model")
  expect_equal(got$r, 1)
})

test_that("na.rm FALSE reports missing statistics", {
  got <- model_metrics(c(1, NA, 3), c(1, 2, 4), na.rm = FALSE)
  expect_true(all(is.na(got[1, ])))
})

test_that("empty model collections are rejected", {
  expect_error(model_metrics(list(), numeric()), "at least one")
})

test_that("constant observations are handled explicitly", {
  got <- model_metrics(1:3, c(2, 2, 2))
  expect_true(is.na(got$NSE))
})
