# Professional function review

## Subsequent author decisions

The author confirmed preservation of original sample-SD normalization.
Alexandre M.J.-C. Wadoux is sole author, maintainer and copyright holder;
public email alexandre.wadoux@yahoo.fr. MIT redistribution is confirmed.
Undefined correlations now return NA. Constant-model coordinates remain
available, with grey representing NA in default correlation colour scales.
This supersedes the review's earlier recommendation to retain reported r = 0.
Graphical alternatives are provided in solar-options.png, target-options.png
and taylor-options.png. The author selected A (current defaults), with overlap
prevention: legends outside panels, title spacing, reference-label obstacles.
Selected figures are saved as solar-approved.png, target-approved.png and
taylor-approved.png.
The public AlexandreWadoux/modelskill repository and issue tracker have been
created. The sections below record the review before these author decisions.

Scope: all original exports, helpers, tests, man pages, examples, metadata,
namespace and package configuration. Vignette content was left unchanged.
Work branch: codex/professional-review. Baseline: 43 passing assertions.

## Architecture and API

The original package repeated pair filtering, SD ratios and correlations in
three diagrams. R/inputs.R now provides shared validation and stable moments;
diagram_stats() exposes the common numerical representation. Plot functions
consume it and return ordinary ggplot objects. No custom plotting class.

All five exported functions accept numeric prediction vectors, lists, matrices
and data frames (models in columns). Partial names are retained. Duplicate or
missing (NA) supplied names error. Numeric inputs must be plain numeric vectors
within columns; no spatial class, coordinates or dataset-specific names are
required. Repeated observations are retained with equal weight.

Existing argument names and positional order are retained; na.rm is appended
to plotting interfaces. model_metrics() keeps its eight columns and model row
names. diagram_stats() returns model identifiers, pair counts and named statistics.
Taylor point data are now available in p$data, as for the other diagrams.

## Corrections and validation

* Missing observations now use complete pairs correctly in diagrams; each
  model's paired subset is checked for sufficient size and observation variance.
* Reject invalid logical flags, dimensions, sizes, colour vectors, infinite data
  and excessive tick counts before expensive plotting.
* Stable RMS and scaled moments avoid squaring overflow for large finite values.
  Centred errors avoid subtractive cancellation in nearly perfect diagrams.
* Constant predictions have finite concordance zero (the continuous limit of
  the old formula). Undefined Pearson correlation retains r = 0 with a warning.
* Metrics with fewer than two pairs return NA correlation-related quantities;
  error magnitudes remain available. Missing-only metrics remain all NA.
* Fixed reference lines and points are annotations, not repeated per model.
* Updated deprecated legend positioning/alignment syntax. Reproducible label
  repulsion uses a local seed and tests check that the RNG state is unchanged.

Backward compatibility changes are intentional: previously accepted invalid
flags, duplicate names, nonfinite colours and unsuitable dimensions now error.
digits is restricted to 0:22. Constant cases now warn and formerly NaN
concordance is zero. Supplied colour names do not reorder values; model order
is explicitly documented. No changes to ordinary complete-data formulas.

## Visual review

Before/after PNG comparisons are saved alongside this report. Taylor, solar
and target palettes, geometry and reference regions were visually inspected.
Differences in repelled labels are expected, with new labels reproducible.
Solar retains its four pale-yellow fills and both solar/target retain viridis
option A. Standard theme, labs, scales and annotation additions are rendered
in tests. Default aspect and original axis-title placement remain.

## Scientific issues requiring author judgement

The author's publicly available manuscript was consulted:
https://www.researchgate.net/publication/352932941_An_integrated_approach_for_the_evaluation_of_quantitative_soil_maps_through_Taylor_and_solar_diagrams
Publication metadata confirmed at:
https://research.wur.nl/en/publications/an-integrated-approach-for-the-evaluation-of-quantitative-soil-ma/

1. Population moments in the paper differ from R's sample-SD convention in
   the supplied plotting scripts. Retained sample normalization is documented
   and tested. With n pairs:
   RMSE^2 / sd(obs)^2 = nME^2 + (n - 1)/n * sde^2.
   Consequently the displayed radial threshold cannot be interpreted as an
   exact finite-sample NSE threshold. This needs a methodological decision.
2. Solar maps x to nME and y to SDE; target maps x to signed SDE and y to nME.
   Original labels are placed near the corresponding axis ends using margins,
   rather than ordinary Cartesian title placement. Retained, with explicit
   coordinate documentation and labs() customization.
3. Original rounded radii 0.31, 0.44 and 0.71 are retained. Do not interpret
   colour regions as unconditional measured-correlation categories.
4. Constant-input r = 0 and positive target sign for equal SDs are retained
   conventions. r = 0 does not establish an actual Pearson correlation.
5. The initial package omitted the innermost Taylor distance contour from the
   research script (start max_radius/6 rather than max_radius/12). This review
   preserves the current figure; restoring it requires graphical review.
6. Solar/target do not enforce equal axis units on screen. coord_fixed() is
   available to users; changing the default aspect would alter figure design.
7. Pairwise missing-value handling in the package differs from the research
   metric script's mixed denominators. Retained consistent pair filtering is
   documented; independent datasets with missing values need matched subsets
   for fair model comparison.

## Dependencies and documentation

Removed ggthemes: its base theme was immediately replaced by the Taylor theme.
Retained ggplot2 (minimum 3.5.0 for legend syntax), ggrepel, latex2exp and viridis.
No new runtime dependency. Function docs now cover supported inputs, units,
ME sign, normalization, missing data, degeneracy, returns and cross-references.
Added package help, citation, NEWS and a repeatable verification script.

## Verification and remaining release work

tests/testthat/test-robustness.R adds analytical bias/anticorrelation cases,
original concordance and coordinate equations, matrix/data-frame equivalence,
missing/constant/single-pair cases, finite precision, invalid parameters,
palette checks, real PNG rendering, ggplot customization and RNG-state checks.
The final development tests pass 195 assertions (baseline: 43), without warnings.
Final local check: zero errors, zero warnings, zero notes. The reviewed version
was installed in the local R library. No Git remote is configured for this
package repository, so no code or workflow was pushed.
review/check-summary.txt records the latest completed local R CMD check.
The verification script saves check results to avoid relying on truncated
terminal output. The checks use --as-cran --no-manual via devtools; remote
incoming checks are disabled by devtools. A three-platform GitHub Actions
workflow is included, but has not been executed remotely in this session.

Before release: resolve the scientific/graphical questions above; confirm the
maintainer email, author permissions and MIT licence grant; verify that the
GitHub URL and issue tracker exist; run checks on Linux/macOS and R-devel, and
build the PDF manual with a TeX installation. The package remains a development
version. Publication metadata says January 2022; the DOI and preprint date are
2021. No claim is made that this is ready to submit to CRAN today.
