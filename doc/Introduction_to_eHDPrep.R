## ---- include=FALSE-----------------------------------------------------------
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

## ---- eval = FALSE------------------------------------------------------------
#  # Not run, just examples:
#  #excel
#  data <- import_dataset(file = "./dataset.xlsx", format = "excel")
#  #csv
#  data <- import_dataset(file = "./dataset.csv", format = "csv")
#  #tsv
#  data <- import_dataset(file = "./dataset.tsv", format = "tsv")
#  

## -----------------------------------------------------------------------------
data(example_data)

# create a consistency table containing consistency rules
# below states: if a patient has a type of diabetes, they should have diabetes
ct <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
                      "diabetes_type", "diabetes", NA, "Type I", "Yes",
                      "diabetes_type", "diabetes", NA, "Type II", "Yes") 

res <- assess_quality(data = example_data, id_var = patient_id, consis_tbl = ct)

res$completeness$variable_completeness
res$completeness$row_completeness


## ---- fig.height=3, out.width = "100%"----------------------------------------
res$completeness$completeness_plot


## ---- fig.height=5, out.width = "100%"----------------------------------------
plot.new()
res$completeness$completeness_heatmap

## -----------------------------------------------------------------------------
res$internal_inconsistency


## -----------------------------------------------------------------------------
res$vars_with_zero_entropy

## ---- eval = FALSE------------------------------------------------------------
#  
#  assume_var_classes(data = example_data, out_file = "./datatypes.csv")
#  
#  # (user makes manual edits externally)
#  
#  import_var_classes(file = "./datatypes.csv")
#  

## ---- echo = FALSE------------------------------------------------------------
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


## ---- echo=FALSE--------------------------------------------------------------
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

## ---- out.width = "100%", fig.height=3----------------------------------------
qc_review$value_level_changes_plt

## ---- eval = FALSE------------------------------------------------------------
#  #csv
#  export_dataset(x = post_QC_example_data,
#                 file = "./post_QC_example_data.csv",
#                 format = "csv")
#  #tsv
#  export_dataset(x = post_QC_example_data,
#                 file = "./post_QC_example_data.csv",
#                 format = "tsv")

## ---- fig.height=3------------------------------------------------------------
variable_completeness(example_data)

row_completeness(data = example_data, id_var = patient_id)

plot_completeness(data = example_data, id_var = patient_id, plot = "variables")

plot_completeness(data = example_data, id_var = patient_id, plot = "rows")


## -----------------------------------------------------------------------------
# show_rownames is pased to pheatmap() through ...
completeness_heatmap(data = strings_to_NA(example_data),
                     id_var = patient_id, 
                     show_rownames = F)

## -----------------------------------------------------------------------------
completeness_heatmap(data = strings_to_NA(example_data),
                     id_var = patient_id, 
                     show_rownames = FALSE,
                     annotation_tbl = data_types)


## ---- fig.height=3------------------------------------------------------------
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


## ---- fig.height=5------------------------------------------------------------
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

## ---- fig.height=3------------------------------------------------------------
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


## ---- eval=T------------------------------------------------------------------
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

## ---- out.width = "100%"------------------------------------------------------
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


## ---- warning=F---------------------------------------------------------------
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

## ---- echo = F----------------------------------------------------------------
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
joined_nw <- join_vars_to_ontol(ontol_graph = example_ontology,
                                var2entity_tbl = example_mapping_file,
                                root = "root", k = 0.5)

## ---- out.width = "100%", fig.width=11----------------------------------------
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
    

## ---- out.width = "100%", fig.width=9, warning=FALSE--------------------------
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

## ----out.width="100%", fig.width=9--------------------------------------------
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
example_ontology %>%
    join_vars_to_ontol(var2entity_tbl = example_mapping_file, root = "root") %>%
    metavariable_info() %>%
    metavariable_agg(data = post_qc_data) ->
    qc_se_data

## summary of output
tibble::glimpse(qc_se_data)


