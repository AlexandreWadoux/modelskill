<img src="man/figures/logo.png" align="right" height="165" alt="modelskill logo" />

# <span style="color:#263746;">model</span><span style="color:#5B8DB8;">skill</span>

### Assessing and Visualising Predictive Model and Map Quality

`modelskill` provides common validation statistics, predictive-uncertainty
validation, and graphical summary diagnostics including Taylor, solar and
target diagrams.

All plotting functions return standard `ggplot2` objects, so figures can be modified using the usual `ggplot2` syntax. Numeric vectors, lists, matrices and data frames are supported; no spatial object or external dataset is required.

> **Development status**  
> `modelskill` is currently under active development and has not yet been submitted to CRAN.

---

## Get started

A complete introduction to the package, including interpretation of the validation indices and examples of the Taylor, solar and target diagrams, is available in the package vignette:

**[Evaluating quantitative models with modelskill](vignettes/modelskill-introduction.Rmd)**

After installation, the vignette can also be opened directly in R:

```r
install.packages("remotes") # once
remotes::install_github("AlexandreWadoux/modelskill")
vignette("modelskill-introduction", package = "modelskill")
```

## Example

```r
library(modelskill)
obs <- seq(1, 10, length.out = 40)
models <- list(
  Accurate = obs + 0.3 * sin(seq_along(obs)),
  Biased = obs + 1,
  Smoothed = mean(obs) + 0.65 * (obs - mean(obs))
)
indices <- model_metrics(models, obs)
indices
diagram_stats(models, obs)

gg_taylor(models, obs, label = TRUE)
gg_solar(models, obs, colorval = indices$NSE,
         colorval.name = "NSE", label = TRUE)
gg_target(models, obs, colorval = indices$NSE,
          colorval.name = "NSE", label = TRUE)

# Uncertainty calibration and sharpness for a 95% prediction interval
lower95 <- models$Accurate - 1.96
upper95 <- models$Accurate + 1.96
uncertainty_metrics(obs, lower = lower95, upper = upper95, level = .95)

gg_coverage(obs,
  lower = list(`0.80` = models$Accurate - 1.282, `0.95` = lower95),
  upper = list(`0.80` = models$Accurate + 1.282, `0.95` = upper95)
)

p <- gg_taylor(models, obs)
p + ggplot2::labs(title = "Model comparison") +
  ggplot2::theme_minimal()
```

Errors are observation minus prediction. Original sample-SD normalization is
retained. Missing values are paired separately for each model by default.
Undefined correlations for constant predictions are reported as NA; constant
models remain drawable and use grey when coloured by correlation.
See `?diagram_stats` for interpretation and finite-sample conventions.

Prediction intervals should be assessed for both calibration and sharpness.
Use `uncertainty_metrics()` for interval or predictive-standard-deviation
validation, and `gg_coverage()` to compare nominal and empirical coverage.

## Diagram defaults

The original palette and diagram conventions are retained, with space reserved
for legends and labels. These examples use NSE as the solar/target point colour.

![Taylor diagram](review/taylor-approved.png)

![Solar diagram](review/solar-approved.png)

![Target diagram](review/target-approved.png)

## Scientific reference

Wadoux, A.M.J.-C., Walvoort, D.J.J. and Brus, D.J. (2022).
An integrated approach for the evaluation of quantitative soil maps through
Taylor and solar diagrams. *Geoderma*, 405, 115332.
[Paper](https://doi.org/10.1016/j.geoderma.2021.115332).

The package originates from the scientific implementation in
[MapQualityEvaluation](https://github.com/AlexandreWadoux/MapQualityEvaluation).

## Author and licence

Alexandre M.J.-C. Wadoux — author, maintainer and copyright holder.
Contact: alexandre.wadoux@yahoo.fr. [MIT licence](LICENSE.md).
