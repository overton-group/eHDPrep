pkgname <- "eHDPrep"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('eHDPrep')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("apply_quality_ctrl")
### * apply_quality_ctrl

flush(stderr()); flush(stdout())

### Name: apply_quality_ctrl
### Title: Apply quality control measures to a dataset
### Aliases: apply_quality_ctrl

### ** Examples

data(example_data)
require(tibble)

# create an example class_tbl object
# note that diabetes_type is classes as ordinal and is not modified as its
# levels are not pre-coded
tibble::tribble(~"var", ~"datatype",
"patient_id", "id",
"tumoursize", "numeric",
"t_stage", "ordinal_tstage",
"n_stage", "ordinal_nstage",
"diabetes", "factor",
"diabetes_type", "ordinal",
"hypertension", "factor",
"rural_urban", "factor",
"marital_status", "factor",
"SNP_a", "genotype",
"SNP_b", "genotype",
"free_text", "freetext") -> data_types

data_QC <- apply_quality_ctrl(example_data, patient_id, data_types, 
   bin_cats =c("No" = "Yes", "rural" = "urban"),  min_freq = 0.6)



cleanEx()
nameEx("assess_completeness")
### * assess_completeness

flush(stderr()); flush(stdout())

### Name: assess_completeness
### Title: Assess completeness of a dataset
### Aliases: assess_completeness

### ** Examples

data(example_data)
res <- assess_completeness(example_data, patient_id)

# variable completeness table
res$variable_completeness

# row completeness table
res$row_completeness

# show completeness of rows and variables as a bar plot
res$completeness_plot

# show dataset completeness in a clustered heatmap
# (this is similar to res$completeness_heatmap but ensures a blank canvas is first created)
res$plot_completeness_heatmap(res)




cleanEx()
nameEx("assess_quality")
### * assess_quality

flush(stderr()); flush(stdout())

### Name: assess_quality
### Title: Assess quality of a dataset
### Aliases: assess_quality

### ** Examples

# general example
data(example_data)
res <- assess_quality(example_data, patient_id)

# example of internal consistency checks on more simple dataset
# describing bean counts
require(tibble)

# creating `data`:
beans <- tibble::tibble(red_beans = 1:15,
blue_beans = 1:15,
total_beans = 1:15*2,
red_bean_summary = c(rep("few_beans",9), rep("many_beans",6)))

# creating `consis_tbl`
bean_rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
"red_beans", "blue_beans", "==", NA, NA,
"red_beans", "total_beans", "<=", NA,NA,
"red_beans", "red_bean_summary", NA, "1:9", "few_beans",
"red_beans", "red_bean_summary", NA, "10:15", "many_beans")

# add some inconsistencies
beans[1, "red_bean_summary"] <- "many_beans"
beans[1, "red_beans"] <- 10

res <- assess_quality(beans, consis_tbl = bean_rules)

# variable completeness table
res$completeness$variable_completeness

# row completeness table
res$completeness$row_completeness

# show completeness of rows and variables as a bar plot
res$completeness$completeness_plot

# show dataset completeness in a clustered heatmap
res$completeness$plot_completeness_heatmap(res$completeness)

# show any internal inconsistencies
res$internal_inconsistency

# show any variables with zero entropy
res$vars_with_zero_entropy



cleanEx()
nameEx("assume_var_classes")
### * assume_var_classes

flush(stderr()); flush(stdout())

### Name: assume_var_classes
### Title: Assume variable classes in data
### Aliases: assume_var_classes

### ** Examples

# example below assumes incorrectly for several variables
tmp = tempfile(fileext = ".csv")
data(example_data)
assume_var_classes(example_data, tmp)



cleanEx()
nameEx("compare_completeness")
### * compare_completeness

flush(stderr()); flush(stdout())

### Name: compare_completeness
### Title: Compare Completeness between Datasets
### Aliases: compare_completeness

### ** Examples

data(example_data)
compare_completeness(example_data, strings_to_NA(example_data), dim = 2,
                     "raw", "cleaned")




cleanEx()
nameEx("compare_info_content")
### * compare_info_content

flush(stderr()); flush(stdout())

### Name: compare_info_content
### Title: Information Content Comparison Table
### Aliases: compare_info_content

### ** Examples

data(example_data)
require(dplyr)
require(magrittr)
example_data %>%
   mutate(diabetes_merged = coalesce(diabetes_type, diabetes)) %>%
   select(starts_with("diabetes")) ->
   merged_data
   
compare_info_content(merged_data$diabetes,
                     merged_data$diabetes_type,
                     merged_data$diabetes_merged)



cleanEx()
nameEx("compare_info_content_plt")
### * compare_info_content_plt

flush(stderr()); flush(stdout())

### Name: compare_info_content_plt
### Title: Information Content Comparison Plot
### Aliases: compare_info_content_plt

### ** Examples

data(example_data)
require(dplyr)
require(magrittr)
example_data %>%
   mutate(diabetes_merged = coalesce(diabetes_type, diabetes)) %>%
   select(starts_with("diabetes")) ->
   merged_data

compare_info_content(merged_data$diabetes,
                     merged_data$diabetes_type,
                     merged_data$diabetes_merged) %>%
                     compare_info_content_plt()



cleanEx()
nameEx("completeness_heatmap")
### * completeness_heatmap

flush(stderr()); flush(stdout())

### Name: completeness_heatmap
### Title: Completeness Heatmap
### Aliases: completeness_heatmap

### ** Examples

data(example_data)

# heatmap without variable category annotations:
hm <- completeness_heatmap(example_data,patient_id)
plot.new() # ensure new plot is created
hm


# heatmap with variable category annotations:
## create a dataframe containing variable annotations
tibble::tribble(~"var", ~"datatype",
"patient_id", "id",
"tumoursize", "numeric",
"t_stage", "ordinal_tstage",
"n_stage", "ordinal_nstage",
"diabetes", "factor",
"diabetes_type", "ordinal",
"hypertension", "factor",
"rural_urban", "factor",
"marital_status", "factor",
"SNP_a", "genotype",
"SNP_b", "genotype",
"free_text", "freetext") -> data_types

hm <- completeness_heatmap(example_data,patient_id, annotation_tbl = data_types)
plot.new() # ensure new plot is created
hm



cleanEx()
nameEx("count_compare")
### * count_compare

flush(stderr()); flush(stdout())

### Name: count_compare
### Title: Compare unique values before and after data modification
### Aliases: count_compare

### ** Examples

# merge data as the example modification
example_data_merged <- merge_cols(example_data, diabetes_type, diabetes, 
"diabetes_merged", rm_in_vars = TRUE)

# review the differences between the input and output of the variable merging step above:
count_compare(before_tbl = example_data,
              after_tbl = example_data_merged,
                            cols2compare = c("diabetes", "diabetes_type", "diabetes_merged"),
                            kableout = FALSE)



cleanEx()
nameEx("edge_tbl_to_graph")
### * edge_tbl_to_graph

flush(stderr()); flush(stdout())

### Name: edge_tbl_to_graph
### Title: Convert edge table to tidygraph graph
### Aliases: edge_tbl_to_graph

### ** Examples

# basic edge table
edge_tbl <- tibble::tribble(~from, ~to,
"Nstage", "TNM",
"Tstage", "TNM",
"Tumoursize", "property_of_tumour",
"Tstage", "property_of_tumour",
"property_of_tumour", "property_of_cancer",
"TNM", "property_of_cancer",
"property_of_cancer", "disease",
"disease", "root",
"root", NA)

graph <- edge_tbl_to_graph(edge_tbl)

graph

plot(graph)


# edge table with node attributes
## note that root node is included in final row to include its label
edge_tbl <- tibble::tribble(~from, ~to, ~label,
"Nstage", "TNM", "N stage",
"Tstage", "TNM", "T stage",
"Tumoursize", "property_of_tumour", "Tumour size",
"Tstage", "property_of_tumour", "T stage",
"property_of_tumour", "property_of_cancer", "Property of tumour",
"TNM", "property_of_cancer", "TNM",
"property_of_cancer", "disease", "Property of cancer",
"disease", "root", "Disease",
"root", NA, "Ontology Root")
graph <- edge_tbl_to_graph(edge_tbl)

graph

plot(graph)




cleanEx()
nameEx("encode_as_num_mat")
### * encode_as_num_mat

flush(stderr()); flush(stdout())

### Name: encode_as_num_mat
### Title: Convert data frame to numeric matrix
### Aliases: encode_as_num_mat

### ** Examples

require(dplyr)
require(magrittr)
mtcars %>%
  dplyr::as_tibble(rownames = "id") %>%
  encode_as_num_mat(id)



cleanEx()
nameEx("encode_binary_cats")
### * encode_binary_cats

flush(stderr()); flush(stdout())

### Name: encode_binary_cats
### Title: Encode categorical variables as binary factors
### Aliases: encode_binary_cats

### ** Examples

# use built-in values. Note: rural_urban is not modified
# Note: diabetes is not modified because "missing" is interpreted as a third category.
# strings_to_NA() should be applied first
encode_binary_cats(example_data, hypertension, rural_urban)

# use custom values. Note: rural_urban is now modified as well.
encoded_data <- encode_binary_cats(example_data, hypertension, rural_urban,
                   values = c("No"= "Yes", "rural" = "urban"))

# to demonstrate the new numeric encoding:
dplyr::mutate(encoded_data, hypertension_num = as.numeric(hypertension), .keep = "used") 



cleanEx()
nameEx("encode_cats")
### * encode_cats

flush(stderr()); flush(stdout())

### Name: encode_cats
### Title: Encode categorical variables using one-hot encoding.
### Aliases: encode_cats

### ** Examples

require(magrittr)
require(dplyr)

data(example_data)

# encode one variable
encode_cats(example_data, marital_status) %>%
select(starts_with("marital_status"))

# encode multiple variables
encoded <- encode_cats(example_data, diabetes, marital_status)

select(encoded, starts_with("marital_status"))
# diabetes_type included below but was not modified:
select(encoded, starts_with("diabetes")) 



cleanEx()
nameEx("encode_genotypes")
### * encode_genotypes

flush(stderr()); flush(stdout())

### Name: encode_genotypes
### Title: Encode genotype/SNP variables in data frame
### Aliases: encode_genotypes

### ** Examples

data(example_data)
require(dplyr)
require(magrittr)

# one variable
encode_genotypes(example_data, SNP_a) %>%
select(SNP_a)

# multiple variables
encode_genotypes(example_data, SNP_a, SNP_b) %>%
select(SNP_a, SNP_b)

# using tidyselect helpers
encode_genotypes(example_data, dplyr::starts_with("SNP")) %>%
select(starts_with("SNP"))




cleanEx()
nameEx("encode_ordinals")
### * encode_ordinals

flush(stderr()); flush(stdout())

### Name: encode_ordinals
### Title: Encode ordinal variables
### Aliases: encode_ordinals

### ** Examples

data(example_data)
require(dplyr)
require(magrittr)
encode_ordinals(example_data, ord_levels = c("N0","N1","N2"), n_stage)

# Note: "unequivocal" is present in  t_stage but not in `ord_levels`.
# with `strict_levels` TRUE, t_stage is unmodified and a warning message is given:

encode_ordinals(example_data,
   ord_levels = c("T1","T2","T3a", "T3b", "T4"), strict_levels = TRUE, t_stage) %>%
   select(t_stage)
   
# with `strict_levels` FALSE, it is replaced with NA:

encode_ordinals(example_data,
   ord_levels = c("T1","T2","T3a", "T3b", "T4"), strict_levels = FALSE, t_stage) %>%
   select(t_stage)



cleanEx()
nameEx("entropy")
### * entropy

flush(stderr()); flush(stdout())

### Name: entropy
### Title: Calculate Entropy of a Vector
### Aliases: entropy

### ** Examples

# no entropy:
vec <- c(1,1,1,1,1,1)
entropy(vec)

# entropy
vec <- c(1,2,3,4,5,6)
entropy(vec)



cleanEx()
nameEx("export_dataset")
### * export_dataset

flush(stderr()); flush(stdout())

### Name: export_dataset
### Title: Export data to delimited file
### Aliases: export_dataset

### ** Examples

data(example_data)
tmp = tempfile(fileext = ".csv")
export_dataset(example_data, tmp)



cleanEx()
nameEx("extract_freetext")
### * extract_freetext

flush(stderr()); flush(stdout())

### Name: extract_freetext
### Title: Extract information from free text
### Aliases: extract_freetext

### ** Examples

data(example_data)
extract_freetext(example_data, patient_id, min_freq = 0.6, free_text)



cleanEx()
nameEx("identify_inconsistency")
### * identify_inconsistency

flush(stderr()); flush(stdout())

### Name: identify_inconsistency
### Title: Identify inconsistencies in a dataset
### Aliases: identify_inconsistency

### ** Examples

require(tibble)
# example with synthetic dataset on number of bean counts
# there is a lot going on in the function so a simple dataset aids this example
#
# creating `data`:
beans <- tibble::tibble(red_beans = 1:15,
blue_beans = 1:15,
total_beans = 1:15*2,
red_bean_summary = c(rep("few_beans",9), rep("many_beans",6)))
#
# creating `consis_tbl`
bean_rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
"red_beans", "blue_beans", "==", NA, NA,
"red_beans", "total_beans", "<=", NA,NA,
"red_beans", "red_bean_summary", NA, "1:9", "few_beans",
"red_beans", "red_bean_summary", NA, "10:15", "many_beans")

identify_inconsistency(beans, bean_rules)

# creating some inconsistencies as examples
beans[1, "red_bean_summary"] <- "many_beans"
beans[1, "red_beans"] <- 10

identify_inconsistency(beans, bean_rules)




cleanEx()
nameEx("import_dataset")
### * import_dataset

flush(stderr()); flush(stdout())

### Name: import_dataset
### Title: Import data into 'R'
### Aliases: import_dataset

### ** Examples

## Not run: 
##D    # This code will not run as it requires an xlsx file
##D    # ./dataset.xlsx should be replaced with path to user's dataset
##D    
##D    # excel
##D    import_dataset(file = "./dataset.xlsx", format = "excel")
##D    #csv
##D    import_dataset(file = "./dataset.csv", format = "csv")
##D    #tsv
##D    import_dataset(file = "./dataset.tsv", format = "tsv")
## End(Not run)




cleanEx()
nameEx("import_var_classes")
### * import_var_classes

flush(stderr()); flush(stdout())

### Name: import_var_classes
### Title: Import corrected variable classes
### Aliases: import_var_classes

### ** Examples

tmp = tempfile(fileext = ".csv")
data(example_data)
assume_var_classes(example_data, tmp)
import_var_classes(tmp)




cleanEx()
nameEx("information_content_contin")
### * information_content_contin

flush(stderr()); flush(stdout())

### Name: information_content_contin
### Title: Calculate Information Content (Continuous Variable)
### Aliases: information_content_contin

### ** Examples

data(example_data)
information_content_contin(example_data$tumoursize)



cleanEx()
nameEx("information_content_discrete")
### * information_content_discrete

flush(stderr()); flush(stdout())

### Name: information_content_discrete
### Title: Calculate Information Content (Discrete Variable)
### Aliases: information_content_discrete

### ** Examples

data(example_data)
information_content_discrete(example_data$marital_status)



cleanEx()
nameEx("join_vars_to_ontol")
### * join_vars_to_ontol

flush(stderr()); flush(stdout())

### Name: join_vars_to_ontol
### Title: Join Mapping Table to Ontology Network Graph
### Aliases: join_vars_to_ontol

### ** Examples

data(example_ontology)
join_vars_to_ontol(example_ontology, example_mapping_file, root = "root", mode = "in")



cleanEx()
nameEx("merge_cols")
### * merge_cols

flush(stderr()); flush(stdout())

### Name: merge_cols
### Title: Merge columns in data frame
### Aliases: merge_cols

### ** Examples

data(example_data)

# preserve input variables (default)
res <- merge_cols(example_data, diabetes_type, diabetes)
dplyr::select(res, dplyr::starts_with("diabetes"))

# remove input variables
res <- merge_cols(example_data, diabetes_type, diabetes, rm_in_vars = TRUE)
dplyr::select(res, dplyr::starts_with("diabetes"))




cleanEx()
nameEx("metavariable_agg")
### * metavariable_agg

flush(stderr()); flush(stdout())

### Name: metavariable_agg
### Title: Aggregate Data by Metavariable
### Aliases: metavariable_agg

### ** Examples

require(magrittr)
require(dplyr)
data(example_ontology)
data(example_mapping_file)
data(example_data)

#' # define datatypes
tibble::tribble(~"var", ~"datatype",
"patient_id", "id",
"tumoursize", "numeric",
"t_stage", "ordinal_tstage",
"n_stage", "ordinal_nstage",
"diabetes_merged", "character",
"hypertension", "factor",
"rural_urban", "factor",
"marital_status", "factor",
"SNP_a", "genotype",
"SNP_b", "genotype",
"free_text", "freetext") -> data_types

# create post-QC data
example_data %>%
  merge_cols(diabetes_type, diabetes, "diabetes_merged", rm_in_vars = TRUE) %>%
  apply_quality_ctrl(patient_id, data_types,
                     bin_cats =c("No" = "Yes", "rural" = "urban"),
                     to_numeric_matrix = TRUE) %>%
                     suppressMessages() ->
                     post_qc_data

# minimal example on first four coloums of example data:
dplyr::slice(example_ontology, 1:7,24) %>%
   join_vars_to_ontol(example_mapping_file[1:3,], root = "root") %>%
   metavariable_info() %>%
   metavariable_agg(post_qc_data[1:10,1:4]) -> res
# see Note section of documentation for information on possible warnings.

# summary of result:
tibble::glimpse(res)




cleanEx()
nameEx("metavariable_info")
### * metavariable_info

flush(stderr()); flush(stdout())

### Name: metavariable_info
### Title: Compute Metavariable Information
### Aliases: metavariable_info

### ** Examples

data(example_ontology)
require(magrittr)
example_ontology %>%
join_vars_to_ontol(example_mapping_file, root = "root") -> joined_ontol

metavariable_info(joined_ontol)



cleanEx()
nameEx("metavariable_variable_descendants")
### * metavariable_variable_descendants

flush(stderr()); flush(stdout())

### Name: metavariable_variable_descendants
### Title: Extract metavariables' descendant variables
### Aliases: metavariable_variable_descendants

### ** Examples

data(example_ontology)
require(magrittr)
example_ontology %>%
join_vars_to_ontol(example_mapping_file, root = "root") -> joined_ontol

mv_info <- metavariable_info(joined_ontol)
metavariable_variable_descendants(mv_info)



cleanEx()
nameEx("mi_content_discrete")
### * mi_content_discrete

flush(stderr()); flush(stdout())

### Name: mi_content_discrete
### Title: Calculate Mutual Information Content
### Aliases: mi_content_discrete

### ** Examples

data(example_data)
mi_content_discrete(example_data$diabetes, example_data$diabetes_type)



cleanEx()
nameEx("mod_track")
### * mod_track

flush(stderr()); flush(stdout())

### Name: mod_track
### Title: Data modification tracking
### Aliases: mod_track

### ** Examples

# merge data as the example modification

require(magrittr)

 # example with one modification type (removal)
 # return table
  mod_track(example_data, strings_to_NA(example_data), patient_id)
 
 # return plot
  mod_track(example_data, strings_to_NA(example_data), patient_id, plot = TRUE)

 # example with multiple modification types (removal, substitution and addition)
example_data %>%
   strings_to_NA() %>%
   merge_cols(diabetes_type, diabetes) ->
   modded_data

# return table
mod_track(example_data, modded_data, patient_id, vars2compare = c("t_stage",
"diabetes_type_diabetes_merged" = "diabetes", "diabetes_type_diabetes_merged"
= "diabetes_type"), plot = FALSE)

# return plot
mod_track(example_data, modded_data, patient_id, vars2compare = c("t_stage",
"diabetes_type_diabetes_merged" = "diabetes", "diabetes_type_diabetes_merged"
= "diabetes_type"), plot = TRUE)



cleanEx()
nameEx("node_IC_zhou")
### * node_IC_zhou

flush(stderr()); flush(stdout())

### Name: node_IC_zhou
### Title: Calculate Node Information Content (Zhou et al 2008 method)
### Aliases: node_IC_zhou

### ** Examples

data(example_ontology)
node_IC_zhou(example_ontology, mode = "in", root = "root")



cleanEx()
nameEx("nums_to_NA")
### * nums_to_NA

flush(stderr()); flush(stdout())

### Name: nums_to_NA
### Title: Replace numeric values in numeric columns with NA
### Aliases: nums_to_NA

### ** Examples

data(example_data)

# replace all 1,2, and 3 from tumoursize and patient_id with NA.
nums_to_NA(data = example_data, tumoursize, patient_id, nums_to_replace = c(1,2,3))



cleanEx()
nameEx("ordinal_label_levels")
### * ordinal_label_levels

flush(stderr()); flush(stdout())

### Name: ordinal_label_levels
### Title: Extract labels and levels of ordinal variables in a dataset
### Aliases: ordinal_label_levels

### ** Examples

require(magrittr)  # for %>%

# create an example class_tbl object
# note that diabetes_type is classed as ordinal yet is not modified as its
# levels are not pre-coded. It should instead be encoded with encode_ordinals().
tibble::tribble(~"var", ~"datatype",
"patient_id", "id",
"tumoursize", "numeric",
"t_stage", "ordinal_tstage",
"n_stage", "ordinal_nstage",
"diabetes", "factor",
"diabetes_type", "ordinal",
"hypertension", "factor",
"rural_urban", "factor",
"marital_status", "factor",
"SNP_a", "genotype",
"SNP_b", "genotype",
"free_text", "freetext") -> data_types

# show unqiue values for t_stage in pre-QC example_data 
unique(example_data$t_stage)

# apply quality control to example_data
apply_quality_ctrl(example_data, patient_id, data_types,
bin_cats =c("No" = "Yes", "rural" = "urban"),  min_freq = 0.6) %>%
ordinal_label_levels -> res

# examine the labels and levels of t_stage in post-QC example_data
dplyr::filter(res, variable == "t_stage")




cleanEx()
nameEx("plot_completeness")
### * plot_completeness

flush(stderr()); flush(stdout())

### Name: plot_completeness
### Title: Plot Completeness of a Dataset
### Aliases: plot_completeness

### ** Examples

data(example_data)
plot_completeness(example_data, patient_id, "variables")



cleanEx()
nameEx("report_var_mods")
### * report_var_mods

flush(stderr()); flush(stdout())

### Name: report_var_mods
### Title: Track changes to dataset variables
### Aliases: report_var_mods

### ** Examples

example_data_merged <- merge_cols(example_data, diabetes_type,
diabetes, "diabetes_merged", rm_in_vars = TRUE)

report_var_mods(example_data, example_data_merged)



cleanEx()
nameEx("review_quality_ctrl")
### * review_quality_ctrl

flush(stderr()); flush(stdout())

### Name: review_quality_ctrl
### Title: Review Quality Control
### Aliases: review_quality_ctrl

### ** Examples

data(example_data)
require(tibble)

tibble::tribble(~"var", ~"datatype",
"patient_id", "id",
"tumoursize", "numeric",
"t_stage", "ordinal_tstage",
"n_stage", "ordinal_nstage",
"diabetes", "factor",
"diabetes_type", "ordinal",
"hypertension", "factor",
"rural_urban", "factor",
"marital_status", "factor",
"SNP_a", "genotype",
"SNP_b", "genotype",
"free_text", "freetext") -> data_types

   
# create QC'ed dataset
post_QC_example_data <- apply_quality_ctrl(example_data,
                                           patient_id,
                                           data_types,
                                           bin_cats =c("No" = "Yes",
                                                       "rural" = "urban"),
                                           min_freq = 0.6)

# review QC
QC_review <- review_quality_ctrl(before_tbl = example_data,
                    after_tbl = post_QC_example_data,
                    id_var = patient_id)

# view variable level changes
QC_review$variable_level_changes

# view value level changes
QC_review$value_level_changes

# view value level changes as a plot
QC_review$value_level_changes_plt




cleanEx()
nameEx("row_completeness")
### * row_completeness

flush(stderr()); flush(stdout())

### Name: row_completeness
### Title: Calculate Row Completeness in a Data Frame
### Aliases: row_completeness

### ** Examples

data(example_data)
row_completeness(example_data, patient_id)



cleanEx()
nameEx("semantic_enrichment")
### * semantic_enrichment

flush(stderr()); flush(stdout())

### Name: semantic_enrichment
### Title: Semantic enrichment
### Aliases: semantic_enrichment

### ** Examples

require(magrittr)
require(dplyr)
data(example_ontology)
data(example_mapping_file)
data(example_data)

#' # define datatypes
tibble::tribble(~"var", ~"datatype",
"patient_id", "id",
"tumoursize", "numeric",
"t_stage", "ordinal_tstage",
"n_stage", "ordinal_nstage",
"diabetes_merged", "character",
"hypertension", "factor",
"rural_urban", "factor",
"marital_status", "factor",
"SNP_a", "genotype",
"SNP_b", "genotype",
"free_text", "freetext") -> data_types

# create post-QC data
example_data %>%
  merge_cols(diabetes_type, diabetes, "diabetes_merged", rm_in_vars = TRUE) %>%
  apply_quality_ctrl(patient_id, data_types,
                     bin_cats =c("No" = "Yes", "rural" = "urban"),
                     to_numeric_matrix = TRUE) %>%
                     suppressMessages() ->
                     post_qc_data

# minimal example on first four coloums of example data:
semantic_enrichment(post_qc_data[1:10,1:4],
                    dplyr::slice(example_ontology, 1:7,24),
                    example_mapping_file[1:3,], root = "root") -> res
# see Note section of documentation for information on possible warnings.

# summary of result:
tibble::glimpse(res)




cleanEx()
nameEx("skipgram_append")
### * skipgram_append

flush(stderr()); flush(stdout())

### Name: skipgram_append
### Title: Append Skipgram Presence Variables to Dataset
### Aliases: skipgram_append

### ** Examples

data(example_data)
# identify skipgrams
toks_m <- skipgram_identify(x = example_data$free_text,
                            ids = example_data$patient_id,
                            max_interrupt_words = 5)
# add skipgrams by minimum frequency
skipgram_append(toks_m,
                id_var = patient_id,
                min_freq = 0.6,
                data = example_data)
# add specific skipgrams
skipgram_append(toks_m,
                id_var = patient_id,
                skipgrams2append = c("sixteen_week", "bad_strain"),
                data = example_data)



cleanEx()
nameEx("skipgram_freq")
### * skipgram_freq

flush(stderr()); flush(stdout())

### Name: skipgram_freq
### Title: Report Skipgram Frequency
### Aliases: skipgram_freq

### ** Examples

data(example_data)
toks_m <- skipgram_identify(x = example_data$free_text,
                            ids = example_data$patient_id,
                            max_interrupt_words = 5)
skipgram_freq(toks_m, min_freq = 0.5)



cleanEx()
nameEx("skipgram_identify")
### * skipgram_identify

flush(stderr()); flush(stdout())

### Name: skipgram_identify
### Title: Identify Neighbouring Words (Skipgrams) in a free-text vector
### Aliases: skipgram_identify

### ** Examples

data(example_data)
skipgram_identify(x = example_data$free_text,
                  ids = example_data$patient_id,
                  max_interrupt_words = 5)



cleanEx()
nameEx("strings_to_NA")
### * strings_to_NA

flush(stderr()); flush(stdout())

### Name: strings_to_NA
### Title: Replace values in non-numeric columns with NA
### Aliases: strings_to_NA

### ** Examples

data(example_data)

# original unique values in diabetes column:
unique(example_data$diabetes)
# Using default values
res <- strings_to_NA(example_data)
unique(res$diabetes)


# original unique values in diabetes_type column:
unique(example_data$diabetes_type)
# Using custom values
res <- strings_to_NA(example_data, strings_to_replace = "Type I")
unique(res$diabetes_type)




cleanEx()
nameEx("validate_consistency_tbl")
### * validate_consistency_tbl

flush(stderr()); flush(stdout())

### Name: validate_consistency_tbl
### Title: Validate internal consistency table
### Aliases: validate_consistency_tbl

### ** Examples

require(tibble)
# example with synthetic dataset on number of bean counters
# there is a lot going on in the function so a simple dataset aids this example
#
# creating `data`:
beans <- tibble::tibble(red_beans = 1:15,
blue_beans = 1:15,
total_beans = 1:15*2,
red_bean_summary = c(rep("few_beans",9), rep("many_beans",6)))
#
# creating `consis_tbl`
bean_rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
"red_beans", "blue_beans", "==", NA, NA,
"red_beans", "total_beans", "<=", NA,NA,
"red_beans", "red_bean_summary", NA, "1:9", "few_beans",
"red_beans", "red_bean_summary", NA, "10:15", "many_beans")

validate_consistency_tbl(beans, bean_rules)



cleanEx()
nameEx("variable_completeness")
### * variable_completeness

flush(stderr()); flush(stdout())

### Name: variable_completeness
### Title: Calculate Variable Completeness in a Data Frame
### Aliases: variable_completeness

### ** Examples

data(example_data)
variable_completeness(example_data)



cleanEx()
nameEx("variable_entropy")
### * variable_entropy

flush(stderr()); flush(stdout())

### Name: variable_entropy
### Title: Calculate Entropy of Each Variable in Data Frame
### Aliases: variable_entropy

### ** Examples

a <- matrix(c(c(1,1,1,1,1,1, 1,2,3,4,5,6)),ncol = 2, dimnames =
list(seq(1,6), c("no_entropy","entropy")))
variable_entropy(as.data.frame(a))



cleanEx()
nameEx("zero_entropy_variables")
### * zero_entropy_variables

flush(stderr()); flush(stdout())

### Name: zero_entropy_variables
### Title: Identify variables with zero entropy
### Aliases: zero_entropy_variables

### ** Examples

data(example_data)
zero_entropy_variables(example_data)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
