require(tidygraph)
require(tibble)

edge_tbl <- tibble::tribble(~from, ~to,
                "Nstage", "TNM",
                "Tstage", "TNM",
                "Tumoursize", "property_of_tumour",
                "Tstage", "property_of_tumour",
                "property_of_tumour", "property_of_cancer",
                "TNM", "property_of_cancer",
                "property_of_cancer", "disease",
                "disease", "root",
                "high_blood_pressure", "heart_disease",
                "heart_disease", "disease",
                "diabetes_Type_I", "diabetes_mellitus",
                "diabetes_Type_II", "diabetes_mellitus",
                "diabetes_mellitus", "metabolic_disorder",
                "metabolic_disorder", "disease",
                "residential_environment", "lifestyle_information",
                "divorced", "marital_status",
                "married",  "marital_status",
                "single",  "marital_status",
                "marital_status", "lifestyle_information",
                "lifestyle_information", "root",
                "gene_A", "pathway_1",
                "gene_B", "pathway_1",
                "pathway_1", "metabolic_pathway",
                "metabolic_pathway", "root",
                "root", NA)

example_ontology <- tidygraph::as_tbl_graph(edge_tbl)

usethis::use_data(example_ontology, overwrite = TRUE)
