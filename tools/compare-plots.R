devtools::load_all()
baseline <- new.env(parent = globalenv())
for (file in list.files("review/baseline", full.names = TRUE)) {
  sys.source(file, envir = baseline)
}
obs <- c(1, 2, 3, 4, 5, 6, 7, 8)
mods <- list(Accurate = obs + c(0.1, -0.2, 0.3, 0, -0.2, 0.1, 0, 0.1),
             Biased = obs + 1, Noisy = c(2, 1, 5, 2, 7, 4, 6, 8))
for (name in c("gg_taylor", "gg_solar", "gg_target")) {
  before <- baseline[[name]](mods, obs, label = TRUE)
  after <- get(name)(mods, obs, label = TRUE)
  grDevices::png(paste0("review/", name, "-comparison.png"),
                 width = 1600, height = 800, res = 110)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  print(before + ggplot2::labs(title = "Before review"),
        vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(after + ggplot2::labs(title = "After review"),
        vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  grDevices::dev.off()
}
