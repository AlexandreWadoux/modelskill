## Submission

This is the first submission of modelskill (version 0.1.0).

The package evaluates continuous predictions and quantified predictive
uncertainty using complementary numerical metrics and ggplot2-based diagnostic
plots. Examples and vignettes use reproducible simulated data and do not require
network access.

## Check results

The final package source (2bf44d7) passed R CMD check --as-cran on GitHub
Actions on Windows, macOS and Linux (R release). Linux R-devel passed on
13484f3; the only subsequent package change assigns rather than automatically
prints the coverage help-example plot, with a corresponding NEWS correction.
The additional R-devel run for 2bf44d7 is still installing dependencies at
submission preparation. CI uses --no-manual; PDF and HTML manuals were checked
successfully locally.

Local Windows R 4.5.3 checks of the source tarball with --as-cran, including
the manual, completed with 0 errors, 0 warnings and 1 NOTE:

  New submission

This is the expected note for a first submission. All 2,085 test assertions
passed with no failures, warnings or skips. All 47 checked package/documentation
URLs passed. Examples and vignettes do not download data.

The submitted archive is the exact tarball used for the final local check,
with NOT_CRAN=false and all suggested dependencies required. Vignettes were
rebuilt during that check. All plot examples completed in under five seconds.
No calculation or plotting implementation changed during submission preparation.
