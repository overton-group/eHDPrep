## Submission

This is a major update (1.4.0 -> 2.0.0).

The main additions are missing value imputation (`impute_missing_values()`, with
both simple summary-statistic and k-nearest-neighbours strategies) and improved
automatic detection of variable classes. The version has been incremented to
2.0.0 rather than 1.5.0 because the automatic class detection changes the output
of `assume_var_classes()` and `apply_quality_ctrl()` for some inputs: character
and factor variables with exactly two unique non-missing values are now treated
as binary. See NEWS.md for the full list of changes.

'VIM' has been added to Suggests. It is used only by
`impute_missing_values(method = "knn")`, is guarded with
`requireNamespace("VIM", quietly = TRUE)`, and the corresponding example is
wrapped in `\donttest{}` and additionally conditioned on the package being
available.

<!-- ----------------------------------------------------------------------- -->
<!-- TODO BEFORE SUBMITTING: replace the section below with the real results  -->
<!-- from `devtools::check()` on a machine with pandoc available and 'VIM'     -->
<!-- installed, so that the vignette is built and the kNN tests actually run.  -->
<!-- Do not submit with this comment still present.                            -->
<!-- ----------------------------------------------------------------------- -->

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
