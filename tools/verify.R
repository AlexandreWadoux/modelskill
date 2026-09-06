# Run with Rscript tools/verify.R from the package root.
# The development tools must already be installed.
dir.create("review", showWarnings = FALSE)
devtools::document()
tests <- devtools::test(stop_on_failure = TRUE)
saveRDS(tests, "review/test-results.rds")
result <- devtools::check(error_on = "never", quiet = FALSE)
saveRDS(result, "review/check-result.rds")
writeLines(c(
  paste("Errors:", length(result$errors)),
  paste("Warnings:", length(result$warnings)),
  paste("Notes:", length(result$notes)),
  result$errors, result$warnings, result$notes
), "review/check-summary.txt")
if (length(result$errors) || length(result$warnings)) {
  stop("Package check found errors or warnings; see review/check-summary.txt.")
}
