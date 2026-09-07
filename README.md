<img src="man/figures/logo.png" align="right" height="165" alt="modelskill logo" />

# modelskill

### Assessing and Visualising Continuous Prediction Model Performance

`modelskill` is an R package for the **evaluation of continuous predictions and their quantified uncertainty**. It supports machine-learning regression models as readily as statistical, geostatistical, physical, and process-based prediction models. Maps are one application, not a requirement.

It brings together:

* statistics for evaluating continuous predictions,
* statistics for evaluating quantified uncertainty, and
* graphical summary and diagnostic tools, including **Taylor, solar and target diagrams**.

All plotting functions return standard `ggplot2` objects, allowing figures to be customised using the usual `ggplot2` syntax.

<br clear="right"/>

---

## Main features

| Task                                                        | Main functions                                                                                                                                          |
| ----------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Statistics for the evaluation of continuous predictions** | `model_metrics()`, `rmse()`, `mae()`, `bias()`, `mse()`, `mec()`, `r2()`, `ccc()`, `nse()`, `kge()` and others                                          |
| **Statistics for the evaluation of quantified uncertainty** | `uncertainty_metrics()`, `coverage()`, `coverage_error()`, `interval_width()`, `interval_score()`, `crps()`, `log_score()`, `pinball_loss()` and others |
| **Summary diagrams and diagnostic plots**                   | `gg_taylor()`, `gg_solar()`, `gg_target()`, `gg_coverage()`, `gg_pit()`, `gg_qcp()`                                                                     |

---

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("AlexandreWadoux/modelskill")
```

Then load the package:

```r
library(modelskill)
```

> **Development status:** `modelskill` is currently under active development and has not yet been submitted to CRAN.

---

## Quick example

```r
library(modelskill)

obs <- seq(1, 10, length.out = 40)

models <- list(
  Accurate = obs + 0.3 * sin(seq_along(obs)),
  Biased = obs + 1,
  Smoothed = mean(obs) + 0.65 * (obs - mean(obs))
)

# Evaluate predictive performance
model_metrics(models, obs)

# Summary diagrams
gg_taylor(models, obs)
gg_solar(models, obs)
gg_target(models, obs)
```

### Graphical model evaluation

<p align="center">
  <img src="review/taylor-approved.png" width="32%" alt="Taylor diagram" />
  <img src="review/solar-approved.png" width="32%" alt="Solar diagram" />
  <img src="review/target-approved.png" width="32%" alt="Target diagram" />
</p>

The diagrams provide complementary summaries of model performance. Because they are returned as `ggplot2` objects, they can be modified directly:

```r
gg_taylor(models, obs) +
  ggplot2::labs(title = "Model comparison") +
  ggplot2::theme_minimal()
```

---

## Evaluating quantified uncertainty

`modelskill` also provides statistics and diagnostics for evaluating predictive uncertainty, including prediction intervals and predictive distributions.

For example, a 95% prediction interval can be evaluated using:

```r
lower95 <- models$Accurate - 1.96
upper95 <- models$Accurate + 1.96

uncertainty_metrics(
  obs,
  lower = lower95,
  upper = upper95,
  level = 0.95
)
```

Prediction-interval calibration can be visualised with `gg_coverage()`, while `gg_pit()` and `gg_qcp()` provide diagnostics for predictive distributions.

```r
gg_coverage(
  obs,
  lower = list(
    `0.80` = models$Accurate - 1.282,
    `0.95` = lower95
  ),
  upper = list(
    `0.80` = models$Accurate + 1.282,
    `0.95` = upper95
  )
)
```

---

## Documentation

A set of focused guides covers point-prediction evaluation, predictive-uncertainty validation, and summary diagrams:

**[Read the online documentation](https://alexandrewadoux.github.io/modelskill/articles/point-prediction.html)**

After installation, it can also be opened directly from R:

```r
vignette(package = "modelskill")
```

Individual functions are documented through the standard R help system. For example:

```r
?model_metrics
?uncertainty_metrics
?gg_taylor
?gg_solar
?gg_target
```

---

## Scientific reference

The Taylor and solar diagram implementation builds on:

> Wadoux, A.M.J.-C., Walvoort, D.J.J. & Brus, D.J. (2022).
> An integrated approach for the evaluation of quantitative soil maps through Taylor and solar diagrams.
> *Geoderma*, **405**, 115332.
> https://doi.org/10.1016/j.geoderma.2021.115332

Uncertainty-validation concepts and diagnostics are discussed by Schmidinger and Heuvelink (2023), *Geoderma*, 437, 116585. <https://doi.org/10.1016/j.geoderma.2023.116585>

---

## Author and licence

**Alexandre M.J.-C. Wadoux**
Author, maintainer and copyright holder

`modelskill` is released under the [MIT License](LICENSE.md).
