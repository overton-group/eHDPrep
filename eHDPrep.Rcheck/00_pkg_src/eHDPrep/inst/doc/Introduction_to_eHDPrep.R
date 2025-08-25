## ----include=FALSE------------------------------------------------------------
old_opts <- options()
old_knitr_opts <- knitr::opts_chunk$get()
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.align = "center"
)

options(pillar.width = 85)
options(pillar.max_dec_width = 3)
options(pillar.sigfig = 2)

## ----setup, include=FALSE-----------------------------------------------------
library(eHDPrep)

## -----------------------------------------------------------------------------
data(example_data)
tibble::glimpse(example_data)

## ----echo=FALSE, fig.cap="Suggested workflow of low-level quality control functions in eHDPrep. Dashed lines and boxes represent optional steps.", fig.align='center'----
# ![](./images/Figure_1.png)
knitr::include_graphics("./images/Figure_1.png")

## ----eval = FALSE-------------------------------------------------------------
# # Not run, just examples:
# #excel
# data <- import_dataset(file = "./dataset.xlsx", format = "excel")
# #csv
# data <- import_dataset(file = "./dataset.csv", format = "csv")
# #tsv
# data <- import_dataset(file = "./dataset.tsv", format = "tsv")
# 

## ----fig.show = "hide"--------------------------------------------------------
data(example_data)

# create a consistency table containing consistency rules
# below states: if a patient has a type of diabetes, they should have diabetes
ct <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                      "diabetes_type", "diabetes", NA, "Type I", "Yes",
                      "diabetes_type", "diabetes", NA, "Type II", "Yes") 

res <- assess_quality(data = example_data, id_var = patient_id, consis_tbl = ct)

## -----------------------------------------------------------------------------
res$completeness$row_completeness
res$completeness$variable_completeness

## ----fig.height=3, out.width = "100%", fig.cap="Percentage completeness (x-axis) by count (y-axis) for both rows (red) and variables (purple) of `example_data`."----
res$completeness$completeness_plot

## ----fig.height=5, out.width = "100%", fig.cap="Completeness heatmap for `example_data`. Yellow cells represent missing values and blue cells represent non-missing values."----
plot.new()
res$completeness$completeness_heatmap

## -----------------------------------------------------------------------------
res$internal_inconsistency


## -----------------------------------------------------------------------------
res$vars_with_zero_entropy

## -----------------------------------------------------------------------------
tmp = tempfile(fileext = ".csv")
assume_var_classes(data = example_data, out_file = tmp)

# (user makes manual edits externally)

import_var_classes(file = tmp)

## ----echo = FALSE-------------------------------------------------------------
# create an example class_tbl object
# note that diabetes_type is classes as ordinal and is not modified as its
# levels are not pre-coded
tibble::tribble(~"var", ~"datatype",
"patient_id", "id",
"tumoursize", "numeric",
"t_stage", "ordinal_tstage",
"n_stage", "ordinal_nstage",
"diabetes", "factor",
"diabetes_type", "factor",
"hypertension", "factor",
"rural_urban", "factor",
"marital_status", "factor",
"SNP_a", "genotype",
"SNP_b", "genotype",
"free_text", "freetext") -> data_types


## -----------------------------------------------------------------------------
data_types


## -----------------------------------------------------------------------------
apply_quality_ctrl(data = example_data,
                   id_var = patient_id,
                   class_tbl = data_types,
                   bin_cats =c("No" = "Yes", "rural" = "urban"),
                   min_freq = 0.6)


## ----echo=FALSE---------------------------------------------------------------
tibble::tribble(~"var", ~"datatype",
"patient_id", "id",
"tumoursize", "numeric",
"t_stage", "ordinal_tstage",
"n_stage", "ordinal_nstage",
"diabetes_merged", "factor",
"hypertension", "factor",
"rural_urban", "factor",
"marital_status", "factor",
"SNP_a", "genotype",
"SNP_b", "genotype",
"free_text", "freetext") -> data_types_diabetes_m

## -----------------------------------------------------------------------------
data_types_diabetes_m

## -----------------------------------------------------------------------------
require(magrittr) # for pipe: %>%
example_data %>%
  # first merge diabetes variables
  merge_cols(primary_var = diabetes_type,
             secondary_var = diabetes,
             merge_var_name = "diabetes_merged",
             rm_in_vars = TRUE) %>%
  # pass data with diabetes_merged to high-level QC function
  apply_quality_ctrl(id_var = patient_id, class_tbl = data_types_diabetes_m,
                     bin_cats =c("No" = "Yes", "rural" = "urban")) ->
  post_QC_example_data
  
  post_QC_example_data


## -----------------------------------------------------------------------------
example_data %>%
  # first merge diabetes variables
    merge_cols(primary_var = diabetes_type,
             secondary_var = diabetes,
             merge_var_name = "diabetes_merged",
             rm_in_vars = TRUE) %>%
  # pass data with diabetes_merged to high-level QC function
  apply_quality_ctrl(id_var = patient_id, class_tbl = data_types_diabetes_m,
                     bin_cats =c("No" = "Yes", "rural" = "urban"),
                     # Relevant line:
                     to_numeric_matrix = TRUE) ->
  post_QC_example_data_m

  # concise summary of output:
  tibble::glimpse(post_QC_example_data_m)


## -----------------------------------------------------------------------------
qc_review <- review_quality_ctrl(before_tbl = example_data,
                                 after_tbl = post_QC_example_data,
                                 id_var = patient_id)


## -----------------------------------------------------------------------------
qc_review$variable_level_changes

## -----------------------------------------------------------------------------
qc_review$value_level_changes

# summary of above
qc_review$value_level_changes %>% 
  dplyr::distinct(across(!patient_id))

## ----out.width = "100%", fig.height=3, fig.cap = "Proportion of values modified per patient in `example_data` following quality control. This plot summarises the modifications made to the data during quality control."----
qc_review$value_level_changes_plt

## ----eval = FALSE-------------------------------------------------------------
# #csv
# export_dataset(x = post_QC_example_data,
#                file = "./post_QC_example_data.csv",
#                format = "csv")
# #tsv
# export_dataset(x = post_QC_example_data,
#                file = "./post_QC_example_data.csv",
#                format = "tsv")

## ----fig.height=3, fig.cap="Percentage completeness (x-axis) by count (y-axis) for variables of `example_data.`"----
variable_completeness(example_data)

row_completeness(data = example_data, id_var = patient_id)

plot_completeness(data = example_data, id_var = patient_id, plot = "variables")

## ----fig.height=3, fig.cap="Percentage completeness (x-axis) by count (y-axis) for rows of `example_data`."----
plot_completeness(data = example_data, id_var = patient_id, plot = "rows")

## ----fig.height=5, fig.show='hold', fig.cap="Completeness heatmap of `example_data` after pre-defined strings have been converted to `NA`. Yellow cells represent missing values and blue cells represent non-missing values."----
# show_rownames is passed to pheatmap() through the `...` parameter
hm <- completeness_heatmap(data = strings_to_NA(example_data),
                     id_var = patient_id, 
                     show_rownames = F)
plot.new()
hm

## ----fig.height=5, fig.show='hold', fig.cap="Completeness heatmap of `example_data` after pre-defined strings have been converted to `NA`. Yellow cells represent missing values and blue cells represent non-missing values. Variables are annotated by their data type."----
hm <- completeness_heatmap(data = strings_to_NA(example_data),
                     id_var = patient_id, 
                     show_rownames = FALSE,
                     annotation_tbl = data_types)
plot.new()
hm


## ----fig.height=3, fig.cap="Density plot comparing percentage row completeness of `example_data` before specified strings have been converted to `NA` (purple) and after (green)."----
compare_completeness(tbl_a = example_data, tbl_b = strings_to_NA(example_data),
                     dim = 1, tbl_a_lab = "example_data",
                     tbl_b_lab = "strings_to_NA(\nexample_data\n)")

## -----------------------------------------------------------------------------
example_incon_rules <-  tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                                        "diabetes_type", "diabetes", NA, "Type I", "Yes",
                                        "diabetes_type", "diabetes", NA, "Type II", "Yes"
                                        )
example_incon_rules

## -----------------------------------------------------------------------------
# validate the consistency rule table
validate_consistency_tbl(data = example_data, consis_tbl = example_incon_rules)

## -----------------------------------------------------------------------------
identify_inconsistency(data = example_data, consis_tbl = example_incon_rules)

## -----------------------------------------------------------------------------
merge <- merge_cols(data = example_data,
                    primary_var = diabetes_type,
                    secondary_var = diabetes,
                    merge_var_name = "diabetes_merged")


## ----fig.height=3, fig.cap="Comparison of information content between two input variables and each input variable's mutual information with the merged variable (output). This plot can inform variable merging strategies. Mutual information of `merge\\$diabetes` with `output` is lower than information content of `merge\\$diabetes` which informs the user that some information loss has occurred in this merging strategy."----
merge_IC <- compare_info_content(input1 = merge$diabetes,
                                 input2 = merge$diabetes_type,
                                 composite = merge$diabetes_merged)

merge_IC

compare_info_content_plt(compare_info_content_res = merge_IC)

## -----------------------------------------------------------------------------
# default values
example_data_NAs1 <- strings_to_NA(data = example_data)

# predefined value "equivocal" is removed
unique(example_data_NAs1$t_stage)

# custom values (T1 does not represent missingness, just used as an example)
example_data_NAs2 <-strings_to_NA(data = example_data,
                                  strings_to_replace = "T1")

# custom value "T1" is removed
unique(example_data_NAs2$t_stage)

# numeric value is removed in patient_id
nums_to_NA(data = example_data, patient_id, nums_to_replace = c(1,3))


## -----------------------------------------------------------------------------
encode_cats(data = example_data, marital_status) %>%
  dplyr::select(dplyr::starts_with("marital_status"))


## -----------------------------------------------------------------------------
example_data %>%
  encode_ordinals(ord_levels = c("N0","N1","N2"), n_stage) %>%
  dplyr::select(n_stage)

# demonstrating how ordered factors can be converted to numeric vectors
example_data %>%
  encode_ordinals(ord_levels = c("N0","N1","N2"), n_stage) %>%
  dplyr::select(n_stage) %>%
  dplyr::mutate(dplyr::across(n_stage, as.numeric))

## -----------------------------------------------------------------------------
encode_genotypes(data = example_data, SNP_a, SNP_b) %>%
  dplyr::select(dplyr::starts_with("SNP"))

## -----------------------------------------------------------------------------
# Identify skipgrams in example_data$free_text
skipgrams <- skipgram_identify(x = example_data$free_text,
                  ids = example_data$patient_id,
                  num_of_words = 2,
                  max_interrupt_words = 5)
skipgrams

# Summarise frequency of skipgrams to consider which should be added to the
# data.
skipgram_freq(skipgram_tokens = skipgrams, min_freq = 0.5)

# Append chosen skipgrams to example_data
## a) by minimum frequency
skipgram_append(skipgram_tokens = skipgrams,
                id_var = patient_id,
                min_freq = 0.6,
                data = example_data)

## b) by specific skipgram(s)
skipgram_append(skipgram_tokens = skipgrams,
                id_var = patient_id,
                skipgrams2append = c("sixteen_week", "bad_strain"),
                data = example_data)



## -----------------------------------------------------------------------------
extract_freetext(data = example_data,
                 id_var = patient_id,
                 min_freq = 0.6, free_text)

## -----------------------------------------------------------------------------
# merge data
example_data_merged <- merge_cols(data = example_data,
                                  primary_var = diabetes_type,
                                  secondary_var = diabetes,
                                  merge_var_name = "diabetes_merged",
                                  rm_in_vars = T)

# review this step's effects on the involved variables:
count_compare(before_tbl = example_data,
          after_tbl = example_data_merged,
          cols2compare = c("diabetes", "diabetes_type", "diabetes_merged"),
          kableout = F)

## ----fig.height=3, fig.cap="Proportion of values modified per patient in `example_data` following conversion of specific values to `NA`."----
#variable level modifications
report_var_mods(before_tbl = example_data,
                after_tbl = example_data_merged)

# value level modifications showing which exact missingness values
# were removed
mod_track(before_tbl = example_data,
          after_tbl = strings_to_NA(example_data), 
          id_var = patient_id)

# plot value level modifications
mod_track(before_tbl = example_data,
          after_tbl = strings_to_NA(example_data),
          id_var = patient_id, plot = T)


## -----------------------------------------------------------------------------
# example of data which has been quality controlled.
example_data %>%
  merge_cols(primary_var = diabetes_type,
             secondary_var = diabetes,
             merge_var_name = "diabetes_merged",
             rm_in_vars = TRUE) %>%
  apply_quality_ctrl(id_var = patient_id,
                     class_tbl = data_types_diabetes_m,
                     bin_cats =c("No" = "Yes", "rural" = "urban"),
                     min_freq = 0.6) ->
  post_qc_data

post_qc_data %>%
  encode_as_num_mat(id_var = patient_id) %>%
  tibble::glimpse()


## -----------------------------------------------------------------------------
post_qc_data %>%
  ordinal_label_levels()


## ----echo=FALSE, fig.cap="Workflow of low-level semantic enrichment functions in eHDPrep. The dashed lines and box represent an optional step.", fig.align='center'----
knitr::include_graphics("./images/Figure_6.png")

## ----eval=T-------------------------------------------------------------------
example_data %>%
  # first merge diabetes variables
  merge_cols(primary_var = diabetes_type,
             secondary_var = diabetes,
             merge_var_name = "diabetes_merged",
             rm_in_vars = TRUE) %>%
  # pass data with diabetes_merged to high-level QC function
  apply_quality_ctrl(id_var = patient_id,
                     class_tbl = data_types_diabetes_m,
                     bin_cats =c("No" = "Yes", "rural" = "urban"),
                     to_numeric_matrix = TRUE) ->
  post_qc_data


## -----------------------------------------------------------------------------
data(example_ontology)
example_ontology

## ----out.width = "100%", fig.cap="Visualisation of `example_ontology` using the ggraph package."----
require(ggplot2)
ggraph::ggraph(example_ontology, layout = "sugiyama") +
    ggraph::geom_edge_diagonal(arrow = arrow(length = unit(3, 'mm')),
                       colour = "slategray3") +
    ggraph::geom_node_label(aes(label = name),
                            size = 2.5, repel = FALSE, hjust="inward") +
    theme_void() +
    theme(legend.position = "none") +
    coord_flip()


## -----------------------------------------------------------------------------
data(example_mapping_file)
example_mapping_file


## ----warning=F----------------------------------------------------------------
qc_se_data <- semantic_enrichment(data = post_qc_data,
                                  ontology = example_ontology,
                                  mapping_file = example_mapping_file,
                                  mode = "in",
                                  root = "root")

## -----------------------------------------------------------------------------
tibble::glimpse(qc_se_data)

## -----------------------------------------------------------------------------
qc_se_data %>%
  dplyr::select(tumoursize, t_stage, n_stage,
                dplyr::starts_with("MV_property_of_cancer")) %>%
                tibble::glimpse()

## ----echo = F-----------------------------------------------------------------
# Some summary stats of SE

# number of aggregations
qc_se_data %>%
  dplyr::select(dplyr::starts_with("MV_")) %>%
  length() ->
  num_aggs

# number of meta-variables used (not above / 5 because of zero entropy vars)
qc_se_data %>%
  dplyr::select(dplyr::starts_with("MV_")) %>%
  names() %>%
  substr(.,4,nchar(.)-4) %>%
  unique() %>%
  length() ->
  num_MVs

## -----------------------------------------------------------------------------
example_edge_tbl

## -----------------------------------------------------------------------------
example_ontology <- edge_tbl_to_graph(example_edge_tbl)
example_ontology

## -----------------------------------------------------------------------------
joined_nw <- join_vars_to_ontol(ontol_graph = example_ontology,
                                var2entity_tbl = example_mapping_file,
                                root = "root", k = 0.5)

## ----out.width = "100%", fig.width=11, fig.cap="Visualisation of `example_ontology` using the ggraph package, coloured by the category of the node."----
ggraph::ggraph(joined_nw, layout = "sugiyama") +
    ggraph::geom_edge_diagonal(arrow = arrow(length = unit(3, 'mm')),
                       colour = "slategray3") +
    ggraph::geom_node_label(aes(
      label = name, color = node_category), size = 2.5,
      repel = F, hjust="inward") +
    theme_void() +
    scale_color_brewer(palette = "Set2") +
    coord_flip() +
    theme(legend.position = c(0.08, 0.85))
    

## ----out.width = "100%", fig.width=9, warning=FALSE, fig.cap="Visualisation of `example_ontology` using the ggraph package, coloured by the category of the node. Node size is proportional to node information content. Node labels denote node information content. Dataset variable nodes (right hand side of figure) are not visible as information content is only applicable to ontological entities."----
ggraph::ggraph(joined_nw, layout = "sugiyama") +
    ggraph::geom_edge_diagonal(arrow = arrow(length = unit(3, 'mm')),
                       colour = "slategray3") +
    ggraph::geom_node_point(aes(
      color = node_category, size = information_content)) +
    ggraph::geom_node_label(aes(
      label = round(information_content,2),
      color = node_category),  size = 2.5, hjust="inward") +
    scale_color_brewer(palette = "Set2") +
    theme_void() +
    coord_flip()

## -----------------------------------------------------------------------------
example_ontology %>%
  join_vars_to_ontol(var2entity_tbl = example_mapping_file, root = "root") %>%
  metavariable_info() ->
  metavariables_nw

## ----out.width="100%", fig.width=9, fig.cap="Visualisation of `example_ontology` using the ggraph package. Ontological entities which link two or more dataset variables as descendants are labelled with numeric identifiers for the set of variables linked. Variable sets 5 and 8 variables are shown to have multiple common ancestors. This demonstrates the need to consider the information content of common ancestors so that the most informative common ancestor is used in the labelling of meta-variables."----
metavariables_nw %>%
  # annotations are also considered a set. This isn't helpful for this visualisation
  # Therefore, the sets of non-meta-variables are removed below
  tidygraph::mutate(variable_set = ifelse(!is_metavariable, NA, variable_set)) %>%
  tidygraph::mutate(variable_set = as.factor(variable_set)) %>%
  ggraph::ggraph(layout = "sugiyama") +
    ggraph::geom_edge_diagonal(arrow = arrow(length = unit(3, 'mm')),
                       colour = "slategray3") +
    ggraph::geom_node_label(aes(label = ifelse(is_metavariable, 
                                       as.factor(as.numeric(variable_set)),
                                       name),
                        color = ifelse(is_metavariable, 
                                       as.character(as.numeric(variable_set)),
                                       node_category)),
                    repel = F, size = 2.5, hjust="inward") +
    theme_void() +
    theme(legend.position = "none") +
    coord_flip()

## -----------------------------------------------------------------------------
metavariable_variable_descendants(metavariables_nw)

## -----------------------------------------------------------------------------
example_ontology %>%
    join_vars_to_ontol(var2entity_tbl = example_mapping_file, root = "root") %>%
    metavariable_info() %>%
    metavariable_agg(data = post_qc_data) ->
    qc_se_data

## summary of output
tibble::glimpse(qc_se_data)


## ----include=FALSE------------------------------------------------------------
# restore original options
options(old_opts)
knitr::opts_chunk$set(old_knitr_opts)

