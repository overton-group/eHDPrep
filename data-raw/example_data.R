# needed for "where" function
utils::globalVariables("where")

set.seed(12345)
require(vroom)
require(magrittr)
require(tibble)
require(stringr)
require(dplyr)
# create base data
tibble::tibble(patient_id = 1:1000,
               tumoursize = vroom::gen_double(n = 1000, mean = 50, sd = 20),
               t_stage = vroom::gen_factor(n = 1000, levels= c("T1","T2","T3a","T3b","T4")),
               n_stage = vroom::gen_factor(n = 1000, levels= c("N0","N1","N2")),
               diabetes = vroom::gen_factor(n = 1000, levels= c("Yes","No")),
               diabetes_type = ifelse(diabetes == "Yes", sample(c("Type I", "Type II"),1000, replace = TRUE), NA),
               hypertension = vroom::gen_factor(n = 1000, levels= c("Yes","No")),
               rural_urban = vroom::gen_factor(n = 1000, levels= c("rural","urban")),
               marital_status = vroom::gen_factor(n = 1000, levels= c("married","single","divorced")),
               SNP_a = vroom::gen_character(1000, min = 1, max = 2, values = c("c","g")),
               SNP_b = vroom::gen_character(1000, min = 1, max = 2, values = c("a","t")),
               free_text = sample(stringr::sentences, 1000,replace = TRUE)) %>%
  dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
  # add different missing values
  dplyr::mutate(diabetes = dplyr::if_else(patient_id %in% sample(1:1000, 10, replace = TRUE), "missing",diabetes)) %>%
  dplyr::mutate(t_stage = dplyr::if_else(patient_id %in% sample(1:1000, 20, replace = TRUE), "equivocal",t_stage)) %>%
  dplyr::mutate(marital_status = dplyr::if_else(patient_id %in% sample(1:1000, 50, replace = TRUE), "unknown",marital_status)) %>%
  # add inconsistencies
  dplyr::mutate(diabetes_type = dplyr::if_else(patient_id == 3, "Type I", diabetes_type)) ->
  example_data

usethis::use_data(example_data, overwrite = TRUE)