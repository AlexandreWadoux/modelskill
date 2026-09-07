# Preview alternatives only. This script does not change package defaults.
devtools::load_all()
dir.create("review", showWarnings = FALSE)
obs <- seq(1, 10, length.out = 40)
mods <- list(
  Accurate = obs + 0.3 * sin(seq_along(obs)),
  Biased = obs + 1,
  Smoothed = mean(obs) + 0.65 * (obs - mean(obs)),
  Noisy = obs + 1.8 * sin(seq_along(obs) * 1.7)
)
metrics <- model_metrics(mods, obs)
write_comparison <- function(plots, file) {
  grDevices::png(file, width = 700 * length(plots), height = 800, res = 110)
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, length(plots))))
  for (i in seq_along(plots)) {
    print(plots[[i]], vp = grid::viewport(layout.pos.row = 1, layout.pos.col = i))
  }
}
standard_titles <- ggplot2::theme(
  axis.title.x = ggplot2::element_text(
    hjust = 0.5, margin = ggplot2::margin(t = 10)),
  axis.title.y = ggplot2::element_text(
    hjust = 0.5, margin = ggplot2::margin(r = 10))
)
solar <- gg_solar(mods, obs, colorval = metrics$R2, colorval.name = "NSE",
                  label = TRUE, by = 0.2)
solar_titles <- solar +
  ggplot2::scale_x_continuous(position = "bottom") +
  ggplot2::labs(x = "Normalized mean error (ME*)",
                y = "Standardized error SD (SDE*)") + standard_titles
write_comparison(list(
  solar + ggplot2::labs(title = "A: Current default"),
  solar_titles + ggplot2::labs(title = "B: Conventional axis titles"),
  solar_titles + ggplot2::coord_fixed() +
    ggplot2::labs(title = "C: Titles + equal axis units")
), "review/solar-options.png")

target <- gg_target(mods, obs, colorval = metrics$R2, colorval.name = "NSE",
                    label = TRUE, by = 0.2)
target_titles <- target +
  ggplot2::labs(x = "Signed standardized error SD",
                y = "Normalized mean error (ME*)") + standard_titles
write_comparison(list(
  target + ggplot2::labs(title = "A: Current default"),
  target_titles + ggplot2::labs(title = "B: Conventional axis titles"),
  target_titles + ggplot2::coord_fixed() +
    ggplot2::labs(title = "C: Titles + equal axis units")
), "review/target-options.png")

# Restore only the omitted innermost contour in a temporary function.
restored <- gg_taylor
code <- paste(deparse(body(restored)), collapse = "\n")
stopifnot(grepl("seq(2 * max_radius/12,", code, fixed = TRUE))
body(restored) <- parse(text = sub(
  "seq(2 * max_radius/12,", "seq(max_radius/12,", code, fixed = TRUE))[[1]]
write_comparison(list(
  gg_taylor(mods, obs, label = TRUE) +
    ggplot2::labs(title = "A: Current default"),
  restored(mods, obs, label = TRUE) +
    ggplot2::labs(title = "B: Restore innermost red contour")
), "review/taylor-options.png")
