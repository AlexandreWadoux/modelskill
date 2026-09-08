## Submission

This is the first submission of modelskill (version 0.1.0).

The package evaluates continuous predictions and quantified predictive
uncertainty using complementary numerical metrics and ggplot2-based diagnostic
plots. Examples and vignettes use reproducible simulated data and do not require
network access.

## Check results

The code in commit 212f444 passed R CMD check --as-cran on GitHub Actions on
Windows, macOS and Linux (R release), and Linux (R-devel). The checks used
--no-manual on CI; PDF and HTML manuals were checked successfully locally.

Local Windows R 4.5.3 checks of the source tarball with --as-cran, including
the manual, completed with 0 errors, 0 warnings and 1 NOTE:

  New submission

This is the expected note for a first submission. All 2,085 test assertions
passed with no failures, warnings or skips. All 47 checked package/documentation
URLs passed. Examples and vignettes do not download data.

Only release-note and documentation wording corrections followed those
cross-platform checks; no calculation or plotting code changed.
