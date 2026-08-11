# eHDPrep (development version)

# eHDPrep 2.0.0

## Missing value imputation

* New function `impute_missing_values()` fills missing (`NA`) values using either
  simple summary statistics or k-nearest-neighbours (kNN).
  * `method = "auto"` (default) imputes numeric variables with their median and
    non-numeric variables with their mode (most frequent value). `"median"`,
    `"mean"`, `"mode"` and `"constant"` are also available. Variables to impute
    can be selected with tidyselect syntax; all are imputed by default.
  * `method = "knn"` estimates each missing value from the `k` most similar rows
    using Gower distance, so mixed numeric and categorical data are handled
    together and relationships between variables are better preserved. Variables
    such as row identifiers can be excluded from the distance calculation with
    `ignore`. This method uses the 'VIM' package, which is a suggested (not
    mandatory) dependency and is only required when the method is used.
  * The number of values imputed per variable is reported as a message, so
    imputation can be reviewed with `review_quality_ctrl()`.
* `apply_quality_ctrl()` gains `impute` (default `FALSE`) and `impute_method`.
  Imputation is applied after missing values are standardised but before any
  encoding, so it operates on the raw variable values. All variables except the
  identifier and free-text variables are imputed.

## Improved variable class detection

* `assume_var_classes()` gains a `factor_threshold` argument (default `5`).
  Character and factor variables with exactly two unique non-missing values are
  now labelled `"binary"`, and character variables with more than two but no more
  than `factor_threshold` unique non-missing values are labelled `"factor"`.
  Factor detection can be disabled with `factor_threshold = 0` or `NULL`.
* `import_var_classes()` now accepts the `"binary"` and `"logical"` datatypes.
* `apply_quality_ctrl()` automatically treats character and factor variables with
  exactly two unique non-missing values as binary.
* `encode_bin_cat_vec()` (and therefore `encode_binary_cats()`) now normalises
  mixed boolean encodings before matching. A variable recording the same two
  categories inconsistently (e.g. `"False"`, `"no"`, `"N"` alongside `"True"`,
  `"yes"`, `"Y"`) is collapsed to canonical `"false"`/`"true"` so that the
  default pair matches. Numeric `"0"`/`"1"` are deliberately excluded as they are
  ambiguous; handle these with `strings_to_NA()` beforehand.
* New function `coerce_numeric_vars()` converts variables classed as numeric to
  numeric, reporting how many values (and which) could not be parsed and were
  therefore set to `NA`. It is applied automatically by `apply_quality_ctrl()`.

## Quality assessment

* `assess_completeness()` and `assess_quality()` gain a `plot` argument
  (default `TRUE`). Plots are now only displayed when a graphics device is
  active, and the completeness heatmap no longer draws over existing plots.

## Bug fixes

* `apply_quality_ctrl()` no longer discards messages from the whole quality
  control pipeline. Piping into `suppressMessages()` forced the entire upstream
  chain inside the handler, which silently suppressed the reporting from
  `coerce_numeric_vars()` and `impute_missing_values()`. Suppression is now
  scoped to `extract_freetext()`, as intended.
* Imputation no longer changes the class of a variable:
  * integer variables are imputed with a rounded integer value, as the median or
    mean of a set of integers is frequently fractional (the rounding is
    reported);
  * factors gain the imputed value as an additional level rather than the value
    being silently dropped to `NA`;
  * a `constant` of an incompatible type (e.g. a character value for a numeric
    variable) is now an error instead of converting the entire variable.
* Variables containing no non-missing values are now reported as skipped by
  `impute_missing_values()` rather than being reported as imputed when no values
  had in fact changed.
* `encode_ordinals()`, `encode_cats()` and `metavariable_info()` were refactored
  to avoid notes from `R CMD check` about non-standard evaluation, and
  `metavariable_info()` now uses `dplyr::pick()`.

## Testing

* Added `tests/testthat.R`, without which the test suite was silently skipped by
  `R CMD check`. The tests are now run when the package is checked.
* Added test coverage for `impute_missing_values()`, `coerce_numeric_vars()` and
  the imputation behaviour of `apply_quality_ctrl()`.

# eHDPrep 1.3.5

* Modified the apply_quality_ctrl function to accept data even without genomics variables

# eHDPrep 1.3.4

* Fixed the cross-references issue in man/metavariable_agg.Rd and man/join_vars_to_ontol.Rd

# eHDPrep 1.3.3

* Fixed typo in vignette
* Added CITATION file and added reference to citation in vignette

# eHDPrep 1.3.2

## General
* Added a `NEWS.md` file to track changes to the package.
* Added link to eHDPrep's [github repository](https://github.com/overton-group/eHDPrep) in package documentation.
* Updated various documentation (functions and vignette).
* Now minimally requires and is compatible with dplyr 1.1.0 (#3, @DavisVaughan).

## Quality Control

* `completeness_heatmap()` has been altered to avoid an issue of plotting over other plots. Now requires plot.new() call.

## Semantic Enrichment

* Added function, `metavariable_variable_descendants()`, to more easily access relationships between meta-variables and their descendant variables.
* `semantic_enrichment()` now accepts a label attribute to label meta-variables with the parameter: `label_attr`.
* `semantic_enrichment()` now accepts an ontology as a data frame edge table, as a path to an edge table in CSV format, or as a `tidygraph`/`igraph` graph.
* Added function, `edge_tbl_to_graph()`, to convert edge tables, as data frames, to `tidygraph` graphs.
* Added data, `example_edge_table`, to demonstrate `edge_tbl_to_graph()`.
* `example_ontology` is now generated from `example_edge_table`.
* `metavariable_info()` now can exclude meta-variables with an information content less than a threshold from output with the  `IC_threshold` parameter. This will exclude non-specific meta-variables from consideration when output is passed to `metavariable_agg()` (e.g. ontology's root node).

