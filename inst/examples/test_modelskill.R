# Very simple modelskill test script
# Install modelskill first, then open this file in RStudio and click Source.

library(modelskill)

obs <- c(1, 2, 3, 4, 5, 6)
mods <- list(
  Perfect = obs,
  Biased = obs + 1,
  Noisy = c(1, 2.5, 2.7, 4.5, 5, 7)
)

# Numerical evaluation
results <- model_metrics(mods, obs)
print(results)
stopifnot(is.data.frame(results), nrow(results) == 3)

# Taylor diagram
p_taylor <- gg_taylor(mods, obs, label = TRUE)
print(p_taylor)
print(p_taylor + ggplot2::labs(title = "My Taylor diagram"))

# Solar diagram
p_solar <- gg_solar(
  mods, obs,
  colorval = results$R2,
  colorval.name = "NSE",
  label = TRUE,
  x.axis_begin = -1.5,
  x.axis_end = 1.5,
  y.axis_end = 2,
  by = 0.25
)
print(p_solar)

# Target diagram
p_target <- gg_target(
  mods, obs,
  colorval = results$R2,
  colorval.name = "NSE",
  label = TRUE,
  axis_begin = -2,
  axis_end = 2,
  by = 0.5
)
print(p_target)

# Confirm that the plot objects can be extended with ggplot2.
stopifnot(inherits(p_taylor, "ggplot"))
stopifnot(inherits(p_solar, "ggplot"))
stopifnot(inherits(p_target, "ggplot"))
