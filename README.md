<img src="man/figures/logo.png" align="right" height="165" alt="modelskill logo" />

# <span style="color:#263746;">model</span><span style="color:#5B8DB8;">skill</span>

### Assessing and Visualising Predictive Model and Map Quality

`modelskill` provides a compact set of tools to evaluate quantitative predictions using complementary validation indices together with Taylor, solar and target diagrams.

All plotting functions return standard `ggplot2` objects, so figures can be modified using the usual `ggplot2` syntax. Numeric vectors, lists, matrices and data frames are supported; no spatial object or external dataset is required.

> **Development status**  
> `modelskill` is currently under active development and has not yet been submitted to CRAN.

---

## Get started

A complete introduction to the package, including interpretation of the validation indices and examples of the Taylor, solar and target diagrams, is available in the package vignette:

**[Evaluating quantitative models with modelskill](vignettes/modelskill-introduction.Rmd)**

After installation, the vignette can also be opened directly in R:

```r
vignette("modelskill-introduction", package = "modelskill")
