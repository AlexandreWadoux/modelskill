# modelskill quick start
# Run this file from RStudio after opening the modelskill folder.

# Install these once if necessary:
# install.packages(c("devtools", "ggplot2", "ggrepel", "ggthemes",
#                    "latex2exp", "viridis", "testthat"))

devtools::load_all()

# 1. Make a small, reproducible example.
set.seed(123)
obs <- seq(0, 10, length.out = 100) + rnorm(100, sd = 1)
mods <- list(
  Good_model = obs + rnorm(100, sd = 0.5),
  Biased_model = obs + 1,
  Smooth_model = stats::filter(obs, rep(1 / 5, 5), sides = 2),
  Mean_model = rep(mean(obs), length(obs))
)
mods$Smooth_model <- as.numeric(mods$Smooth_model)

# 2. Calculate the numerical indices.
metrics <- model_metrics(mods, obs)
print(metrics)

# 3. Create all three diagrams.
taylor <- gg_taylor(mods, obs, label = TRUE)
solar <- gg_solar(
  mods, obs, colorval = metrics$NSE, colorval.name = "NSE",
  label = TRUE, x.axis_begin = -1.5, x.axis_end = 1.5,
  y.axis_end = 2, by = 0.25
)
target <- gg_target(
  mods, obs, colorval = metrics$NSE, colorval.name = "NSE",
  label = TRUE, axis_begin = -2, axis_end = 2, by = 0.5
)

print(taylor)
print(solar)
print(target)

# 4. Standard ggplot2 customisation works normally.
taylor + ggplot2::labs(title = "Model comparison") +
  ggplot2::theme_minimal()
