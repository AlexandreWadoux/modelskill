## Resubmission

This is a resubmission of modelskill (version 0.1.1), addressing Uwe Ligges's
feedback of 9 September 2026 on version 0.1.0.

The reported invalid file URI, LICENSE.md in README.md, has been replaced
with https://github.com/AlexandreWadoux/modelskill/blob/main/LICENSE.md.
Both the licence badge and the licence section are corrected, in README.md
and its README.Rmd source. The replacement URL was verified to return HTTP 200.
The MIT licence itself is unchanged.

The version has been increased to distinguish the resubmission. The maintainer's
subsequent equation-label edits in the performance-metrics vignette are retained.
No R functions, calculations, plotting implementations or tests have changed.

The package evaluates continuous predictions and quantified predictive
uncertainty using complementary numerical metrics and ggplot2-based diagnostic
plots. Examples and vignettes use reproducible simulated data and do not require
network access.

## Check results

All four GitHub Actions R CMD check jobs passed for the corrected source
(603e57b): Windows R release, macOS R release, Linux R release and Linux R-devel.
CI uses --as-cran --no-manual. Run:
https://github.com/AlexandreWadoux/modelskill/actions/runs/34325671048

The exact version 0.1.1 archive passed all 2,085 test assertions, with no
failures, warnings or skips. All 53 documentation URLs in that archive passed
urlchecker, including both corrected README licence links. The longest example
took 1.53 seconds in the local check.

The exact source archive passed R CMD check --as-cran --timings locally on
Windows 11 with R 4.5.3, including vignette rebuilding and PDF/HTML manuals:
0 errors, 0 warnings, 2 NOTEs:

* New submission.
* unable to verify current time (the external time verification was unavailable
  on the local checking machine; this is not a detected future timestamp).

The invalid file URI note is resolved. NOT_CRAN=false and all suggested
dependencies were required for the check. This exact checked archive is being
resubmitted without rebuilding. Local R-devel is not installed; Linux R-devel
was checked through the successful GitHub Actions run linked above.

The previous submitted source (2bf44d7) passed GitHub Actions checks on Windows,
macOS and Linux (R release), and Linux R-devel. The CRAN pretest of version
0.1.0 passed its tests, examples, vignettes and PDF/HTML manuals on Linux R-devel.

Names flagged by the CRAN spelling check (Brus, Heuvelink, Schmidinger, Wadoux
and Walvoort) are cited authors' surnames. "geostatistical" is a standard
technical term.
