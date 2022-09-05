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

#' Example data for eHDPrep
#'
#' A dataset containing synthetic example values to demonstrate functionality of
#' 'eHDprep'
#'
#' @format A data frame with 1,000 rows and 10 variables: \describe{
#'   \item{patient_id}{1 to 1000, effictively row numbers}
#'   \item{tumoursize}{double. random values with a mean of 50 and SD of 20}
#'   \item{t_stage}{character. T stage random values} \item{n_stage}{character.
#'   N stage random values} \item{diabetes}{character. Patient diabetes
#'   category} \item{diabetes_type}{character. Patient diabetes type category}
#'   \item{hypertension}{character. Patient hypertension category}
#'   \item{rural_urban}{character. Patient domestic address category}
#'   \item{marital_status}{character. Patient marital status category}
#'   \item{SNP_a}{character. Single Nucleotide Polymorphism (SNP) of the
#'   patient} \item{SNP_b}{character. Another SNP of the patient}
#'   \item{free_text}{character. sentences from the 'stringr' package as an
#'   example of short free text variables} }
#' @source synthetic
"example_data"

#' Example ontology as a network graph for semantic enrichment
#'
#' A small custom network graph to demonstrate semantic enrichment.
#' 
#' Contains semantic links of variables in 'eHDPrep''s `example_data` following
#' quality control.
#' 
#' @format tidygraph graph
#' @source synthetic
"example_ontology"

#' Example mapping file for semantic enrichment
#' 
#' A data frame containing mappings between variables in `example_data` and
#' `example_onto`. Used to demonstrate semantic enrichment.
#' 
#' Maps variables in `example_data` to `example_ontology` in 'eHDPrep'.
#'
#' @format A data frame: \describe{
#' \item{variable}{character. names of variables in post-QC `example_data`.}
#' \item{onto_entity}{character. names of mapped entities in `example_onto`.}
#' }
#' @source synthetic
"example_mapping_file"
