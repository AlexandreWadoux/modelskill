# modelskill

Evaluate quantitative predictions using complementary skill indices and Taylor,
solar and target diagrams. All plotting functions return ordinary ggplot objects.
Numeric vectors, lists, matrices and data frames are supported; no spatial
object or external dataset is required.

This package is under development and has not yet been submitted to CRAN.

## Get started

A full introduction, including interpretation of the indices and examples of the
Taylor, solar and target diagrams, is available in the package vignette:

**[Evaluating quantitative models with modelskill](vignettes/modelskill-introduction.Rmd)**

After installing the package, the vignette can also be opened in R with:

```r
vignette("modelskill-introduction", package = "modelskill")
```

## Install the development version

```r
install.packages("remotes") # once
remotes::install_github("AlexandreWadoux/modelskill")
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

p <- gg_taylor(models, obs)
p + ggplot2::labs(title = "Model comparison") +
  ggplot2::theme_minimal()
```

Errors are observation minus prediction. Original sample-SD normalization is
retained. Missing values are paired separately for each model by default.
Undefined correlations for constant predictions are reported as NA; constant
models remain drawable and use grey when coloured by correlation.
See `?diagram_stats` for interpretation and finite-sample conventions.

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
