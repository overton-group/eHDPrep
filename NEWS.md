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

