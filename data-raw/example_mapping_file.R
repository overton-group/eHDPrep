require(tibble)

example_mapping_file <- tibble::tribble(~variable, ~onto_entity,
                                        "tumoursize", "Tumoursize",
                                        "t_stage", "Tstage",
                                        "n_stage", "Nstage",
                                        "hypertension", "high_blood_pressure",
                                        "rural_urban", "residential_environment",
                                        "SNP_a", "gene_A",
                                        "SNP_b", "gene_B",
                                        "diabetes_merged_Type.I", "diabetes_Type_I",
                                        "diabetes_merged_Type.II", "diabetes_Type_II",
                                        "marital_status_divorced", "divorced",
                                        "marital_status_married", "married",
                                        "marital_status_single", "single"
                                        )

usethis::use_data(example_mapping_file, overwrite = TRUE)