test_that("gg_coverage is composable for one or several levels", {
  obs <- 1:3
  one <- gg_coverage(obs, lower = c(0, 1, 2), upper = c(2, 3, 4), level = .8)
  many <- gg_coverage(obs,
    lower = list(`0.5` = c(1, 2, 3), `0.8` = c(0, 1, 2)),
    upper = list(`0.5` = c(1, 2, 3), `0.8` = c(2, 3, 4)))
  expect_s3_class(one, "ggplot")
  expect_equal(one$data$nominal, .8)
  expect_equal(many$data$nominal, c(.5, .8))
  expect_true(inherits(many + ggplot2::labs(title = "Calibration"), "ggplot"))
})

test_that("gg_coverage rejects malformed intervals", {
  expect_error(gg_coverage(1:3, lower = list(`0.8` = 0:2), upper = 2:4), "named lists")
  expect_error(gg_coverage(1:3, lower = list(bad = 0:2), upper = list(bad = 2:4)), "names")
  expect_error(gg_coverage(1:3, lower = 0:2, upper = 2:4, level = 0), "between")
})
