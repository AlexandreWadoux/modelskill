# Run this file from RStudio to test modelskill and display all figures.
# First set the working directory to the modelskill package folder, then click
# the Source button in RStudio.

devtools::load_all()

set.seed(42)
observed <- seq(0, 10, length.out = 100) + rnorm(100, 0, 1)
predictions <- list(
  Good_model = observed + rnorm(100, 0, 0.5),
  Biased_model = observed + 1,
  Noisy_model = observed + rnorm(100, 0, 2),
  Mean_model = rep(mean(observed), length(observed))
)

cat("Model-skill indices:\n")
indices <- model_metrics(predictions, observed)
print(round(indices, 3))

cat("\nShowing Taylor diagram...\n")
taylor_plot <- gg_taylor(predictions, observed, label = TRUE)
print(taylor_plot)

cat("Showing solar diagram...\n")
solar_plot <- gg_solar(
  predictions, observed,
  colorval = indices$NSE,
  colorval.name = "NSE",
  label = TRUE,
  x.axis_begin = -1.5,
  x.axis_end = 1.5,
  y.axis_end = 2,
  by = 0.25
)
print(solar_plot)

cat("Showing target diagram...\n")
target_plot <- gg_target(
  predictions, observed,
  colorval = indices$NSE,
  colorval.name = "NSE",
  label = TRUE,
  axis_begin = -2,
  axis_end = 2,
  by = 0.5
)
print(target_plot)

cat("Showing a customised Taylor diagram...\n")
print(
  taylor_plot +
    ggplot2::labs(title = "My model comparison") +
    ggplot2::theme_minimal()
)

cat("All modelskill examples completed successfully.\n")
