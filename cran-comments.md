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

This release also resolves the tidyselect 1.2.0 deprecation warnings that were
emitted by `apply_quality_ctrl()` and `encode_cats()` (use of `.data` in
tidyselect expressions, and `all_of()` outside a selecting function). The test
suite now runs without warnings.

## Test environments

* local macOS 15 (aarch64), R 4.6.1
* GitHub Actions: macOS-latest (release), windows-latest (release),
  ubuntu-latest (devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

The note is raised only on the local machine and concerns its check
infrastructure rather than the package:

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
Skipping checking math rendering: package 'V8' unavailable
```

The vignette PDF is compacted at build time; please build with
`--compact-vignettes=gs+qpdf` (as the GitHub Actions workflow does), otherwise
`R CMD check` reports a warning about its size.

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and
dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
