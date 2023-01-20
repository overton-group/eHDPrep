#    Copyright (C) 2022 Queens University Belfast
#    
#    This file is part of 'eHDPrep'
#
#    'eHDPrep' is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    'eHDPrep' is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with 'eHDPrep'. If not, see <http://www.gnu.org/licenses/>.

###
##### assess quality #####
###

#' Assess completeness of a dataset
#'
#' Assesses and visualises completeness of the input data across both rows (samples)
#' and columns (variables).
#'
#' @inheritParams assess_quality
#' @param plot Should plots be rendered when function is run? (Default: TRUE)
#' @return list of completeness tibbles and plots
#' @details 
#' Returns a list of completeness assessments:
#' \describe{
#' \item{variable_completeness}{A tibble detailing completeness of variables (columns)
#' (via \code{\link{variable_completeness}}).}
#' \item{row_completeness}{A tibble detailing completeness of rows (via
#' \code{\link{row_completeness}}).}
#' \item{completeness_plot}{A plot of row and variable (column) completeness (via
#' \code{\link{plot_completeness}}).}
#' \item{completeness_heatmap}{A clustered heatmap of cell completeness (via
#' \code{\link{completeness_heatmap}}).}
#' \item{plot_completeness_heatmap}{A function which creates a clean canvas before 
#' plotting the completeness heatmap.}
#' }
#'
#' @importFrom dplyr enquo
#' @importFrom rlang !!
#' @importFrom grDevices devAskNewPage
#' @importFrom grid grid.newpage
#' @family measures of completeness
#' @export
#'
#' @examples
#' data(example_data)
#' res <- assess_completeness(example_data, patient_id)
#' 
#' # variable completeness table
#' res$variable_completeness
#' 
#' # row completeness table
#' res$row_completeness
#' 
#' # show completeness of rows and variables as a bar plot
#' res$completeness_plot
#' 
#' # show dataset completeness in a clustered heatmap
#' # (this is similar to res$completeness_heatmap but ensures a blank canvas is first created)
#' res$plot_completeness_heatmap(res)
#'
assess_completeness <- function(data, id_var, plot = TRUE) {

  id_var <- dplyr::enquo(id_var)
  compl_plt <- plot_completeness(data, !! id_var)
  compl_hm <- completeness_heatmap(data, !! id_var)
  
  res <- list(
    "variable_completeness" = variable_completeness(data),
    "row_completeness" = row_completeness(data, !! id_var),
    "completeness_plot" = compl_plt,
    "completeness_heatmap" = compl_hm,
    "plot_completeness_heatmap" = function(i) { grid::grid.newpage(); i$completeness_heatmap } 
    )

  if (plot) {
    print(compl_plt)
    devAskNewPage(ask = TRUE)
    grid::grid.newpage()
    print(compl_hm)
    devAskNewPage(ask = FALSE)
  }

  return(invisible(res))
}


#' Assess quality of a dataset
#'
#' Provides information on the quality of a dataset. Assesses dataset's
#' completeness, internal consistency, and entropy.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param id_var An unquoted  expression which corresponds to a variable (column) in
#'   \code{data} which identifies each row (sample).
#' @inheritParams validate_consistency_tbl
#' @inheritSection validate_consistency_tbl Consistency Table Requirements
#' @return Nested list of quality measurements
#' @details Wraps several quality assessment functions from \code{eHDPrep}
#'   and returns a nested list with the following structure:
#'   \describe{
#'   \item{completeness}{- A list of completeness assessments:
#'   \enumerate{
#'   \item Tibble of variable (column) completeness (via \code{\link{variable_completeness}})
#'   \item Tibble of row (sample) completeness (via \code{\link{row_completeness}})
#'   \item Plot of row and variable completeness (via \code{\link{plot_completeness}})
#'   \item Completeness heatmap (via \code{\link{completeness_heatmap}})
#'   \item A function which creates a clean canvas before  plotting the completeness heatmap.
#'   }
#'   }
#'   \item{internal_inconsistency}{- Tibble of internal inconsistencies, if any
#'   are present and if a consistency table is supplied (via
#'   \code{\link{identify_inconsistency}}).}
#'   \item{vars_with_zero_entropy}{- Names of variables (columns) with zero entropy (via
#'   \code{\link{zero_entropy_variables}})}
#'   
#' }
#' @importFrom dplyr enquo
#' @importFrom rlang !!
#' @family high level functionality
#' @export
#'
#' @examples
#' # general example
#' data(example_data)
#' res <- assess_quality(example_data, patient_id)
#' 
#' # example of internal consistency checks on more simple dataset
#' # describing bean counts
#' require(tibble)
#' 
#' # creating `data`:
#' beans <- tibble::tibble(red_beans = 1:15,
#' blue_beans = 1:15,
#' total_beans = 1:15*2,
#' red_bean_summary = c(rep("few_beans",9), rep("many_beans",6)))
#' 
#' # creating `consis_tbl`
#' bean_rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
#' "red_beans", "blue_beans", "==", NA, NA,
#' "red_beans", "total_beans", "<=", NA,NA,
#' "red_beans", "red_bean_summary", NA, "1:9", "few_beans",
#' "red_beans", "red_bean_summary", NA, "10:15", "many_beans")
#' 
#' # add some inconsistencies
#' beans[1, "red_bean_summary"] <- "many_beans"
#' beans[1, "red_beans"] <- 10
#'
#' res <- assess_quality(beans, consis_tbl = bean_rules)
#' 
#' # variable completeness table
#' res$completeness$variable_completeness
#' 
#' # row completeness table
#' res$completeness$row_completeness
#' 
#' # show completeness of rows and variables as a bar plot
#' res$completeness$completeness_plot
#' 
#' # show dataset completeness in a clustered heatmap
#' res$completeness$plot_completeness_heatmap(res$completeness)
#' 
#' # show any internal inconsistencies
#' res$internal_inconsistency
#' 
#' # show any variables with zero entropy
#' res$vars_with_zero_entropy
assess_quality <- function(data, id_var, consis_tbl) {
  
  if(missing(id_var)) {
    data <- tibble::rownames_to_column(data, var = "rowname")
    id_var <- sym("rowname")
  } else{id_var <- dplyr::enquo(id_var)}

  # assess completeness
  completeness <- assess_completeness(data, !! id_var, plot = TRUE)

  # internal consistency
  if (!missing(consis_tbl)) {
    suppressMessages(validate_consistency_tbl(data, consis_tbl))
    intern_con <- tryCatch({x <- identify_inconsistency(data, consis_tbl)},
                           message = function(mess) {return(mess$message)},
                           warning = function(warn) {
                             return(suppressWarnings(identify_inconsistency(data, consis_tbl)))
                             },
                           error = function(err) {return(err)}
                           )
  } else{intern_con <- "internal consistency checks not made (`consis_tbl` not provided)"}
  
  # identify variables with zero entropy
  zero_entropy_variables <- zero_entropy_variables(data)

  # list of results
  list("completeness" = completeness,
       "internal_inconsistency" = intern_con,
       "vars_with_zero_entropy" = zero_entropy_variables)
}


###
###### apply QC #####
###
#' Apply quality control measures to a dataset
#' 
#' The primary high level function for quality control. Applies several quality
#' control functions in sequence to input data frame (see Details for individual
#' functions).
#' 
#' The wrapped functions are applied in the following order:
#' \enumerate{
#' \item Standardise missing values (\code{\link{strings_to_NA}})
#' \item Encode binary categorical variables (columns) (\code{\link{encode_binary_cats}})
#' \item Encode (specific) ordinal variables (columns)(\code{\link{encode_ordinals}})
#' \item Encode genotype variables (\code{\link{encode_genotypes}})
#' \item Extract information from free text variables (columns) (\code{\link{extract_freetext}})
#' \item Encode non-binary categorical variables (columns) (\code{\link{encode_cats}})
#' \item Encode output as numeric matrix (optional, \code{\link{encode_as_num_mat}})
#' }
#'
#' \code{class_tbl} is used to apply the above functions to the appropriate variables (columns).
#'
#' @inheritParams assess_quality
#' @param min_freq Minimum frequency of occurrence of skipgrams (groups of
#'   proximal words) to extract from free text variables (columns).
#' @param class_tbl data frame such as the output tibble from
#'   \code{\link{assume_var_classes}} followed by
#'   \code{\link{import_var_classes}}. 
#' @param bin_cats Optional named vector of user-defined values for binary
#'   values using \code{binary_label_1 = binary_label_2} syntax (e.g.
#'   \code{c("No" = "Yes")} would assign level 1 to "No" and 2 to "Yes"). See
#'   \code{\link{encode_binary_cats}} for defaults. Applied to variables (columns)
#'   labelled "character" or "factor" in \code{class_tbl}.
#' @param min_freq Minimum frequency of occurrence
#'   \code{\link{extract_freetext}} will use to extract groups of proximal
#'   words in free-text from variables (columns) labelled "freetext" in \code{class_tbl}.
#' @param to_numeric_matrix Should QC'ed data be converted to a numeric matrix?
#'   Default: FALSE.
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr all_of
#' @family high level functionality
#' @return \code{data} with several QC measures applied.
#' @export
#'
#' @examples
#' data(example_data)
#' require(tibble)
#' 
#' # create an example class_tbl object
#' # note that diabetes_type is classes as ordinal and is not modified as its
#' # levels are not pre-coded
#' tibble::tribble(~"var", ~"datatype",
#' "patient_id", "id",
#' "tumoursize", "numeric",
#' "t_stage", "ordinal_tstage",
#' "n_stage", "ordinal_nstage",
#' "diabetes", "factor",
#' "diabetes_type", "ordinal",
#' "hypertension", "factor",
#' "rural_urban", "factor",
#' "marital_status", "factor",
#' "SNP_a", "genotype",
#' "SNP_b", "genotype",
#' "free_text", "freetext") -> data_types
#' 
#' data_QC <- apply_quality_ctrl(example_data, patient_id, data_types, 
#'    bin_cats =c("No" = "Yes", "rural" = "urban"),  min_freq = 0.6)
apply_quality_ctrl <- function(data, id_var, class_tbl, bin_cats = NULL, min_freq = 1, to_numeric_matrix = FALSE) {
  if(missing(class_tbl)) {
    stop("The argument `class_tbl` must be supplied. It is generated using the functions assume_var_classes() then import_var_classes()", call. = FALSE)
  } else{}

  id_var <- dplyr::enquo(id_var)
  
  data %>%
    strings_to_NA(dplyr::all_of(
      select_by_datatype(class_tbl, c("id","numeric", "integer", "double"), negate = TRUE))) %>%
    encode_binary_cats(dplyr::all_of(select_by_datatype(class_tbl, c("character","factor"))), values = bin_cats) %>%
    encode_ordinals(ord_levels = c("T1","T2","T3a", "T3b", "T4",NA), strict_levels = T,
                       dplyr::all_of(select_by_datatype(class_tbl, "ordinal_tstage"))) %>%
    encode_ordinals(ord_levels = c("N0","N1","N2",NA), strict_levels = T, 
                       dplyr::all_of(select_by_datatype(class_tbl, "ordinal_nstage"))) %>%
    encode_genotypes(dplyr::all_of(select_by_datatype(class_tbl, "genotype"))) %>%
    {if(length(dplyr::all_of(select_by_datatype(class_tbl, "freetext"))) > 0) {
      extract_freetext(., id_var = !! id_var, min_freq = min_freq,
                       dplyr::all_of(select_by_datatype(class_tbl, "freetext")))
      } else .} %>%
    suppressMessages() %>% # extract_freetext discusses input that user cant modify in this function
    encode_cats(dplyr::all_of(select_by_datatype(class_tbl, c("factor","character")))) %>%
    suppressWarnings() -> # encode_cats() will warn of (and ignore) binary cats
    data

  if (to_numeric_matrix) {
    data %>%
      encode_as_num_mat(id_var = !! id_var) ->
      data
  } else {}

  data
}

###
###### review QC #####
###
#' Review Quality Control
#'
#' Provides information on modifications made to a dataset at both variable
#' (column) and value (sample) levels, designed for review of quality control
#' measures.
#' 
#' Modifications are identified by comparing the original and modified dataset.
#' 
#' QC review functions are applied in the following order:
#' \enumerate{
#' \item Variable-level modifications (\code{\link{report_var_mods}})
#' \item Value-level modifications (\code{\link{mod_track}})
#' \item Value-level modifications (plot) (\code{\link{mod_track}})
#' }
#' A list containing each of these functions' outputs is returned.
#' 
#' @inheritParams mod_track
#' @return List containing data for review of quality control
#' @export
#' @family high level functionality
#'
#' @examples
#' data(example_data)
#' require(tibble)
#' 
#' tibble::tribble(~"var", ~"datatype",
#' "patient_id", "id",
#' "tumoursize", "numeric",
#' "t_stage", "ordinal_tstage",
#' "n_stage", "ordinal_nstage",
#' "diabetes", "factor",
#' "diabetes_type", "ordinal",
#' "hypertension", "factor",
#' "rural_urban", "factor",
#' "marital_status", "factor",
#' "SNP_a", "genotype",
#' "SNP_b", "genotype",
#' "free_text", "freetext") -> data_types
#' 
#'    
#' # create QC'ed dataset
#' post_QC_example_data <- apply_quality_ctrl(example_data,
#'                                            patient_id,
#'                                            data_types,
#'                                            bin_cats =c("No" = "Yes",
#'                                                        "rural" = "urban"),
#'                                            min_freq = 0.6)
#' 
#' # review QC
#' QC_review <- review_quality_ctrl(before_tbl = example_data,
#'                     after_tbl = post_QC_example_data,
#'                     id_var = patient_id)
#'
#' # view variable level changes
#' QC_review$variable_level_changes
#'
#' # view value level changes
#' QC_review$value_level_changes
#'
#' # view value level changes as a plot
#' QC_review$value_level_changes_plt
#'
review_quality_ctrl <- function(before_tbl, after_tbl, id_var) {
  id_var <- dplyr::enquo(id_var)

  res <- list(
    "variable_level_changes" = report_var_mods(before_tbl, after_tbl),
    # warnings not useful for high-level
    "value_level_changes" = suppressMessages(suppressWarnings(mod_track(before_tbl, after_tbl,
                                      id_var = !! id_var, plot = FALSE))),
    "value_level_changes_plt" = suppressMessages(suppressWarnings(mod_track(before_tbl, after_tbl,
                                          id_var = !! id_var, plot = TRUE)))
    )
  return(res)
}

###
###### semantic enrichment #####
###

#' Semantic enrichment
#' 
#' Enriches a dataset with additional (meta-)variables derived from the semantic
#' commonalities between variables (columns).
#' 
#' Semantic enrichment generates meta-variables from the aggregation of data
#' variables (columns) via their most informative common ancestor. Meta-variables are
#' labelled using the syntax: \code{MV_[label_attr]_[Aggregation function]}. The
#' data variables are aggregated row-wise by their maximum, minimum, mean, sum,
#' and product. Meta-variables with zero entropy (no information) are not
#' appended to the data.
#' See the "Semantic Enrichment" section in the vignette of 'eHDPrep' for more
#' information: \code{vignette("Introduction_to_eHDPrep", package = "eHDPrep")}
#' 
#' @param data Numeric data frame or matrix containing variables present in
#'   the mapping file. Required.
#' @param ontology One of: \itemize{\item Path to ontology edge table in .csv format (String)\item Edge
#'   table in data frame format \item Graph containing the chosen ontology -
#'   must be in \code{\link[tidygraph:tidygraph]{tidygraph}} format or coercible
#'   to this format.}. Required. 
#' @param mapping_file Path to csv file or data frame containing mapping
#'   information. Should contain two columns only. The first column should
#'   contain column names, present in the data frame. The second column should
#'   contain the name of entities present in the ontology object. Required.
#' @param root name of root node identifier in column 1 to calculate node depth
#'   from. Required.
#' @inheritParams metavariable_info
#' @inheritParams metavariable_agg
#' @note A warning may be shown regarding the '.add' argument being deprecated, this is
#'   believed to be an issue with 'tidygraph' which may be resolved in a future release: 
#'   <https://github.com/thomasp85/tidygraph/issues/131>. Another warning may be shown regarding the 'neimode' argument being deprecated, this is
#'   believed to be an issue with 'tidygraph' which may be resolved in a future release: 
#'   <https://github.com/thomasp85/tidygraph/issues/156>. These warning messages are not believed to have
#'   an effect on the functionality of 'eHDPrep'.
#' @param ... additional arguments to pass to \code{\link[readr]{read_csv}} when reading `mapping_file`.
#' 
#' @importFrom dplyr as_tibble
#' @importFrom readr read_csv
#' @family high level functionality
#'
#' @return Semantically enriched dataset
#' @export
#'
#' @examples
#' require(magrittr)
#' require(dplyr)
#' data(example_ontology)
#' data(example_mapping_file)
#' data(example_data)
#' 
#' #' # define datatypes
#' tibble::tribble(~"var", ~"datatype",
#' "patient_id", "id",
#' "tumoursize", "numeric",
#' "t_stage", "ordinal_tstage",
#' "n_stage", "ordinal_nstage",
#' "diabetes_merged", "character",
#' "hypertension", "factor",
#' "rural_urban", "factor",
#' "marital_status", "factor",
#' "SNP_a", "genotype",
#' "SNP_b", "genotype",
#' "free_text", "freetext") -> data_types
#'
#' # create post-QC data
#' example_data %>%
#'   merge_cols(diabetes_type, diabetes, "diabetes_merged", rm_in_vars = TRUE) %>%
#'   apply_quality_ctrl(patient_id, data_types,
#'                      bin_cats =c("No" = "Yes", "rural" = "urban"),
#'                      to_numeric_matrix = TRUE) %>%
#'                      suppressMessages() ->
#'                      post_qc_data
#'
#' # minimal example on first four coloums of example data:
#' semantic_enrichment(post_qc_data[1:10,1:4],
#'                     dplyr::slice(example_ontology, 1:7,24),
#'                     example_mapping_file[1:3,], root = "root") -> res
#' # see Note section of documentation for information on possible warnings.
#'
#' # summary of result:
#' tibble::glimpse(res)
#'
#' \donttest{
#' # full example:
#'  res <- semantic_enrichment(post_qc_data, example_ontology,
#'  example_mapping_file, root = "root")
#'  # see Note section of documentation for information on possible warnings.
#' }
semantic_enrichment <- function(data, ontology, mapping_file, mode = "in", root, label_attr = "name", ...) {
  
  # convert ontology to graph object if it is a path
  if(is.character(ontology)) {
    ontology_edge_tbl <- readr::read_csv(ontology)
    ontology <- edge_tbl_to_graph(ontology_edge_tbl)
  } else if (is.data.frame(ontology)) { # convert if data frame
    ontology <- edge_tbl_to_graph(ontology)
  } else {}
  
  
  # accept both paths and R data frames for mapping_file
  if (missing(mapping_file)) {
    stop("`mapping_file` is missing and must be supplied.", call. = FALSE)
  } else if (is.character(mapping_file)) {
    mapping_file <- readr::read_csv(file = file, ...)
  } else if (is.data.frame(mapping_file)) {
    mapping_file <- dplyr::as_tibble(mapping_file)
    if(ncol(mapping_file) != 2) {
      stop("`mapping_file` is data frame must have two columns.\n
           \u2716 You've supplied a data frame with ",
           ncol(mapping_file), " columns.", call. = FALSE)}
  } else {stop("`mapping_file` is not a valid path to a .csv file or a data", call. = FALSE)}
  
  # perform semantic enrichment
  ontology %>%
    join_vars_to_ontol(mapping_file, root = root, mode = mode) %>%
    metavariable_info(mode) %>%
    metavariable_agg(data, label_attr)

}
