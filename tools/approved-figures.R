devtools::load_all()
obs <- seq(1, 10, length.out = 40)
mods <- list(
  Accurate = obs + 0.3 * sin(seq_along(obs)),
  Biased = obs + 1,
  Smoothed = mean(obs) + 0.65 * (obs - mean(obs)),
  Noisy = obs + 1.8 * sin(seq_along(obs) * 1.7)
)
metrics <- model_metrics(mods, obs)
for (name in c("taylor", "solar", "target")) {
  fun <- get(paste0("gg_", name))
  args <- list(mods = mods, obs = obs, label = TRUE)
  if (name != "taylor") {
    args$colorval <- metrics$NSE
    args$colorval.name <- "NSE"
    args$by <- 0.2
  }
  p <- do.call(fun, args)
  ggplot2::ggsave(paste0("review/", name, "-approved.png"),
                 p, width = 8, height = 7, dpi = 130)
}
