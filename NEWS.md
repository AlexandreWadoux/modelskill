# modelskill 0.0.0.9000

* Add exported diagram_stats() for independent access to diagram statistics.
* Accept numeric matrices and data frames (one model per column) consistently.
* Share pair filtering and numerical calculations across diagrams.
* Add na.rm at the end of each diagram interface; existing positional calls work.
* Reject invalid flags, infinite data, duplicate model names and invalid sizes.
* Preserve partial model names instead of discarding all supplied names.
* Fix missing-observation handling and reject degenerate paired plot subsets.
* Evaluate centred error directly and scale intermediate calculations to avoid
  cancellation and overflow. Regular complete-data definitions remain unchanged.
* Constant inputs now warn about the existing r = 0 convention. Constant
  predictions have concordance zero; fewer than two pairs give NA correlations.
* Keep original palettes, radii, sample SD normalization and axis-title placement.
* Replace repeated constant graphical layers with annotations, update legend
  syntax, expose Taylor point data, and remove redundant ggthemes dependency.
* Expand analytical, input, geometry, palette and real-device rendering tests.
* Make repelled labels reproducible without changing the user's RNG state.
* Add a three-platform GitHub Actions package-check workflow.

Compatibility: missing/nonfinite colour values, numeric logical flags, duplicate
names and invalid dimensions now error. digits is limited to integers 0 through
22. Degenerate metric outputs are explicitly defined above. No existing arguments
were renamed or reordered.
