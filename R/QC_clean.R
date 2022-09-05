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
#    along with 'eHDPrep'.  If not, see <http://www.gnu.org/licenses/>.

# needed for "where" function
utils::globalVariables("where")

##' Clean missing values in a character vector
##'
##' Internal. Replaces values in character vectors representing missing values
##' with the reserved value \code{NA}. "Undetermined", "unknown", "missing",
##' "fail", "fail / unknown", "equivocal", "equivocal / unknown", "*". These can
##' be replaced with the \code{values_to_replace} parameter.
##'
##' @param x input vector
##' @param values_to_replace vector containing values to be replaced with
##'   \code{NA}
##' @param ... additional arguments to pass to \code{grepl}. Use
##'   \code{ignore.case = TRUE} to ignore case.
##' @return vector with default or specified values replaced with \code{NA}
##' @noRd
##' @examples
##' nums <- c(1,2,3,4,5,6,7,8,-99)
##' NA_clean(nums, -99)
NA_clean <- function(x, values_to_replace = NULL, ...) {

  pattern <- paste0("^", values_to_replace, "$")
  pattern <- paste0(pattern, collapse = "|")
  x[grepl(pattern = pattern, x = x, ...)] <- NA
  x
}

#' Replace values in non-numeric columns with NA
#'
#' Replaces specified or pre-defined strings in non-numeric columns with
#' \code{NA}. 
#'
#' @details Columns to process can be specified in custom arguments (\code{...}) or will
#' be applied to all non-numeric columns.
#' Default strings which will be replaced with \code{NA} are as follows:
#' "Undetermined", "unknown", "missing", "fail", "fail / unknown", 
#' "equivocal", "equivocal / unknown", "*".
#' String search is made using \code{\link[base]{grepl}} and supports
#' \code{\link[base]{regex}} so metacharacters (\code{. \ | ( ) [ ] {  } ^ $ * + ? $})
#' should be escaped with a "\code{\\}" prefix.
#' Matches are case sensitive by default but can ignore case with the parameter: 
#' \code{ignore.case = TRUE} in \code{...}).
#'
#' @param strings_to_replace character vector of values to be replaced with
#'   \code{NA}.
#' @inheritParams nums_to_NA
#'
#' @return data with specified values replaced with NA.
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' # original unique values in diabetes column:
#' unique(example_data$diabetes)
#' # Using default values
#' res <- strings_to_NA(example_data)
#' unique(res$diabetes)
#' 
#' 
#' # original unique values in diabetes_type column:
#' unique(example_data$diabetes_type)
#' # Using custom values
#' res <- strings_to_NA(example_data, strings_to_replace = "Type I")
#' unique(res$diabetes_type)
#' 
strings_to_NA <- function(data, ..., strings_to_replace = NULL) {
  # use built in values if no strings are specified
  if(is.null(strings_to_replace)) {
    strings_to_replace <- c("Undetermined",
                                 "unknown",
                                 "missing",
                                 "fail",
                                 "fail / unknown",
                                 "equivocal",
                                 "equivocal / unknown",
                                 #"9",
                                 "*")
  } else {}

  if (missing(...)) {
    data %>%
      dplyr::mutate(dplyr::across(!where(is.numeric),
                                  ~ NA_clean(.x, strings_to_replace, ignore.case = TRUE)
                                  )
      )
    } else {
        vars <- dplyr::enquos(...)
        
        # stop if numeric vars are selected
        data %>%
          dplyr::select(!!! vars & where(is.numeric)) %>%
          names() -> 
          num_vars
        
        if(length(num_vars > 0 )) {
          stop("`...` must not contain numeric variables.\n",
               paste0("\u2716 `", num_vars, "` is a numeric variable\n"), call. = FALSE)
        } else {}
        
        data %>%
          dplyr::mutate(dplyr::across(!!! vars,
                                      ~ NA_clean(.x, strings_to_replace, ignore.case = TRUE)
          ))
      }

}

#' Replace numeric values in numeric columns with NA
#'
#' Replaces specified numbers in numeric columns with \code{NA}.
#' 
#' Columns to process can be specified in \code{...} or the function will be
#' applied to all numeric columns.
#' 
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param nums_to_replace numeric vector of values to be replaced with
#'   \code{NA}. Case is ignored.
#' @inheritParams dplyr::select
#' @return \code{data} with specified values replaced with NA
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' # replace all 1,2, and 3 from tumoursize and patient_id with NA.
#' nums_to_NA(data = example_data, tumoursize, patient_id, nums_to_replace = c(1,2,3))
nums_to_NA <- function (data, ..., nums_to_replace = NULL) {
  if(is.null(nums_to_replace)) {
         warning("`nums_to_replace` is missing and no values will be replaced with NA",call.= FALSE)
  } else {}

  if (missing(...)) {
    data %>%
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  NA_clean,
                                  nums_to_replace)
                    )
  } else {
    vars <- dplyr::enquos(...)

    data %>%
      dplyr::mutate(dplyr::across(.cols = c(!!! vars),
                                  .fns = NA_clean, nums_to_replace)
                    )
  }
}

##' Encode a categorical vector with binary categories
##'
##' In a character vector, converts binary categories to factor levels.
##'
##' Binary categories to convert can be specified with a named character vector,
##' specified in \code{values}. The syntax of the named vector is:
##' \code{negative_finding = positive_finding}.  If \code{values} is not
##' provided, the default list will be used: \code{"No"="Yes", "No/unknown" =
##' "Yes", "no/unknown" = "Yes", "Non-user" = "User", "Never" = "Ever", "WT" =
##' "MT"}.
##'
##' @param x non-numeric input vector
##' @param values Optional named vector of user-defined values for binary values
##'   using \code{binary_label_1 = binary_label_2} syntax (e.g. \code{c("No" =
##'   "Yes")} would assign level 1 to "No" and 2 to "Yes").
##' @param numeric_out If true, numeric vector is returned. If false, factor is
##'   returned.
##' @return Factor with false finding encoded as 1 and true finding encoded as
##'   2. Alternatively, numeric vector if \code{numeric_out} parameter is
##'   \code{TRUE}.
##' @importFrom forcats fct_relevel
##' @examples
##' data(example_data)
##' # return factor vector
##' eHDPrep:::encode_bin_cat_vec(example_data$hypertension)
##'
##' # return numeric vector
##' eHDPrep:::encode_bin_cat_vec(example_data$hypertension, numeric_out = TRUE)
##'
##' # use custom values
##' eHDPrep:::encode_bin_cat_vec(example_data$rural_urban,
##'                              values = c("rural" = "urban"))
##' 
encode_bin_cat_vec <- function(x, values = NULL, numeric_out = FALSE) {
  if(is.factor(x)){ x <- as.character(x)} else{x} # convert factor inputs to character

  # if(length(unique(x)) != 2)
  #   stop("Input vector should have 2 unique values", call. = FALSE)

  # default values to clean
  # first is negative finding, second is positive finding
  default_values <- c("No"="Yes", "No/unknown" = "Yes", "no/unknown" = "Yes",
                      "Non-user" = "User", "Never" = "Ever", "WT" = "MT")

  # use default values if no others are supplied
  if (is.null(values)) {values <- default_values} else{values}

  for (i in 1:length(values)) {
      if (all(unique(x) %in% c(names(values)[[i]],values[[i]], NA))) {
        out <- forcats::fct_relevel(factor(x), c(names(values)[[i]],values[[i]]))
      if (numeric_out) {out <- sapply(out, function(x) as.double(x)-1)}
        return(out)
      }
  }

   return(x)

}

#' Encode categorical variables as binary factors
#' 
#' In a data frame, converts binary categories to factors. Ordering of levels is
#' standardised to: \code{negative_finding, positive_finding}. This embeds a
#' standardised numeric relationship between the binary categories while
#' preserving value labels.
#' 
#' Binary categories to convert can be specified with a named character vector,
#' specified in \code{values}. The syntax of the named vector is:
#' \code{negative_finding = positive_finding}. If \code{values} is not
#' provided, the default list will be used: \code{"No"="Yes", "No/unknown" =
#' "Yes", "no/unknown" = "Yes", "Non-user" = "User", "Never" = "Ever", "WT" =
#' "MT"}.
#'
#' @inheritParams encode_bin_cat_vec
#' @inheritParams nums_to_NA
#' @inherit nums_to_NA
#'
#' @return dataset with specified binary categories converted to factors.
#' @export
#'
#' @examples
#' # use built-in values. Note: rural_urban is not modified
#' # Note: diabetes is not modified because "missing" is interpreted as a third category.
#' # strings_to_NA() should be applied first
#' encode_binary_cats(example_data, hypertension, rural_urban)
#'
#' # use custom values. Note: rural_urban is now modified as well.
#' encoded_data <- encode_binary_cats(example_data, hypertension, rural_urban,
#'                    values = c("No"= "Yes", "rural" = "urban"))
#' 
#' # to demonstrate the new numeric encoding:
#' dplyr::mutate(encoded_data, hypertension_num = as.numeric(hypertension), .keep = "used") 

encode_binary_cats <- function(data, ..., values = NULL) {
  warn_missing_dots(missing(...))
  values <- dplyr::enquo(values)
  vars <- dplyr::enquos(...)

  data %>%
    dplyr::mutate(dplyr::across(c(!!! vars),
                                encode_bin_cat_vec,
                                values = !! values))
}

##' Encode ordinal variables
##'
##' Converts character or factor variables in the input data frame to ordered factors
##' embedding numeric relationship between values while preserving value labels.
##' 
##' @param ord_levels character vector containing values in desired order
##'   (lowest to highest).
##' @param strict_levels logical constant. If \code{TRUE}, variables in
##'   \code{...} which contain values other than \code{ord_levels} (including
##'   \code{NA}) are not modified and a warning is given. If \code{FALSE},
##'   values not in \code{ord_levels} are converted to \code{NA}.
##' @inheritParams nums_to_NA
##' @return dataframe with specified variables encoded as ordered factors.
##' @importFrom dplyr mutate across enquos summarise everything as_label
##' @importFrom tidyr pivot_longer
##' @importFrom rlang .data
##' @export
##' @examples
##' data(example_data)
##' require(dplyr)
##' require(magrittr)
##' encode_ordinals(example_data, ord_levels = c("N0","N1","N2"), n_stage)
##'
##' # Note: "unequivocal" is present in  t_stage but not in `ord_levels`.
##' # with `strict_levels` TRUE, t_stage is unmodified and a warning message is given:
##' 
##' encode_ordinals(example_data,
##'    ord_levels = c("T1","T2","T3a", "T3b", "T4"), strict_levels = TRUE, t_stage) %>%
##'    select(t_stage)
##'    
##' # with `strict_levels` FALSE, it is replaced with NA:
##' 
##' encode_ordinals(example_data,
##'    ord_levels = c("T1","T2","T3a", "T3b", "T4"), strict_levels = FALSE, t_stage) %>%
##'    select(t_stage)
encode_ordinals <- function(data, ord_levels, ..., strict_levels = TRUE) {

  if (missing(...)) {
    warn_missing_dots(TRUE)
    return(invisible(data))
  } else {
    vars <- dplyr::enquos(...)

    #check for missing levels
    data %>%
      dplyr::select(!!! vars) %>%
      dplyr::summarise(dplyr::across(everything(), ~!.x %in% ord_levels)) %>%
      # sum number of missing levels
      dplyr::summarise(dplyr::across(c(!!! vars), sum)) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      # filter variables with missing levels
      dplyr::filter(.data$value > 0) ->
      missing_levels
    
    # warn if any missing levels
    if(nrow(missing_levels) > 0 & strict_levels) {
      warning("When parameter `strict_levels` is TRUE, all levels must be present in `ord_levels` for variables specified with `...`.\nThe following variables have not been modified:\n",
              paste0("\u2716 `", missing_levels$name, "` contains ", missing_levels$value, " problematic values.\n"),
              "\n\u2139 Ensure missing values (i.e. `NA`) have been included in `ord_levels` if it should be considered.",  call. = FALSE)
      
      # remove variables containing missing levels from `vars` and don't modify
      vars <- vars[-which(sapply(vars, function(x) dplyr::as_label(x) %in% missing_levels$name))]
    } else {}
    
    data %>%
      dplyr::mutate(dplyr::across(c(!!! vars),
                                  ~ factor(as.character(.x),
                                           levels = ord_levels,
                                           ordered=T)
                                  )
                    )
  }
}


##' Encode a genotype/SNP vector
##'
#' Standardises homozygous SNP alleles (e.g. recorded as 'A') to two character
#' form (e.g. 'A/A') and orders heterozygous SNP alleles alphabetically (e.g.
#' "GA" becomes "A/G"). The SNP values are then converted from a character
#' vector to an ordered factor, ordered by SNP allele frequency (e.g. most
#' frequent SNP allele is 1, second most frequent value is 2, and least frequent
#' values is 3). This method embeds the numeric relationship between the SNP
#' allele frequencies while preserving value labels.
##'
##' @param x input vector containing genotype data
##' @return Ordered factor, ordered by allele frequency in variable
##' @importFrom magrittr %>%
##' @importFrom dplyr recode
##' @importFrom forcats fct_infreq
##' @examples
##' data(example_data)
##' vec <- example_data[,"SNP_a",drop = TRUE]
##' eHDPrep:::encode_genotype_vec(vec)
encode_genotype_vec <- function(x) {
  
  # make uppercase
  x <- toupper(x)
  
  # replace single values for homozygous with double
  x <- sapply(as.character(x),
              function(x) ifelse(nchar(x) == 1, paste0(x, x), x)
              )
  # order SNP allele pairs alphabettically

  x <- sapply(as.character(x),
              function(x){ ifelse(is.na(x),
                                 NA,
                                  paste0(sort(c(substr(x, 1, 1), substr(x, 2, 2))),
                                  collapse = "")) }
              )
  
  # Add "/" between each SNP's alleles
  x <- sapply(as.character(x),
              function(x){ ifelse(is.na(x),
                                 NA,
                                 paste0(substr(x, 1, 1), "/", substr(x, 2, 2))) }
              )
  
  # order SNP alleles by cohort frequency (in x)
  unname(forcats::fct_infreq(x, ordered = TRUE))
}

#' Encode genotype/SNP variables in data frame
#'
#' Standardises homozygous SNPs (e.g. recorded as "A") to two character
#' form (e.g. "A/A") and orders heterozygous SNPs alphabetically (e.g.
#' "GA" becomes "A/G"). The SNP values are then converted from a character
#' vector to an ordered factor, ordered by observed allele frequency (in the supplied cohort). The most
#' frequent allele is assigned level 1, the second most frequent value is assigned level 2, and the least frequent
#' values is assigned level 3). This method embeds the numeric relationship between the
#' allele frequencies while preserving value labels.
#'
#' @inheritParams nums_to_NA
#' @return `data` with variables (\code{...}) encoded as standardised genotypes
#' @export
#'
#' @examples
#' data(example_data)
#' require(dplyr)
#' require(magrittr)
#' 
#' # one variable
#' encode_genotypes(example_data, SNP_a) %>%
#' select(SNP_a)
#' 
#' # multiple variables
#' encode_genotypes(example_data, SNP_a, SNP_b) %>%
#' select(SNP_a, SNP_b)
#' 
#' # using tidyselect helpers
#' encode_genotypes(example_data, dplyr::starts_with("SNP")) %>%
#' select(starts_with("SNP"))
#' 
encode_genotypes <- function(data, ...) {
  warn_missing_dots(test = missing(...))

  vars <- dplyr::enquos(...)
  data %>%
    dplyr::mutate(dplyr::across(c(!!! vars), encode_genotype_vec))
}


#' One hot encode a vector
#' 
#' Uses one-hot encoding to convert nominal vectors to a tibble containing
#' variables for each of the unique values in input vector.
#'
#' @param x non-numeric vector
#' @param prefix prefix to append to output variable names
#' @importFrom dplyr as_tibble group_by add_tally ungroup select
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom  tibble rownames_to_column
#' @return tibble
#' 
#'
#' @examples
#' vec <- c("blue", "red", "green", "red", "green")
#' eHDPrep:::onehot_vec(vec, "example")
onehot_vec <- function(x, prefix) {
  prefix <- ifelse(missing(prefix),deparse(substitute(x)),prefix)

  x %>% 
    dplyr::as_tibble() %>%
    tibble::rownames_to_column() %>%
    dplyr::group_by(.data$rowname) %>%
    dplyr::add_tally() %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = .data$value,
                       values_from = n,
                       values_fill = 0,
                       names_prefix = paste0(prefix,"_"),
                       names_repair = "universal") %>%
    suppressMessages() %>%
    dplyr::select(-.data$rowname)
}


##' Encode categorical variables using one-hot encoding.
##'
##' Variables specified in \code{...} are replaced with new variables describing
##' the presence of each unique category. Generated variable names have space
##' characters replaced with "_" and commas are removed.
##'
##' @inheritParams nums_to_NA
##' @inheritParams encode_as_num_mat
##'
##' @return Tibble with converted variables.
##' @importFrom magrittr %>%
##' @importFrom rlang .data
##' @importFrom dplyr enquo enquos mutate ungroup group_by all_of select n_distinct pull filter
##' @importFrom tibble add_column
##' @importFrom tidyr pivot_longer pivot_wider
##' @export
##' @examples
##' require(magrittr)
##' require(dplyr)
##' 
##' data(example_data)
##' 
##' # encode one variable
##' encode_cats(example_data, marital_status) %>%
##' select(starts_with("marital_status"))
##' 
##' # encode multiple variables
##' encoded <- encode_cats(example_data, diabetes, marital_status)
##' 
##' select(encoded, starts_with("marital_status"))
##' # diabetes_type included below but was not modified:
##' select(encoded, starts_with("diabetes")) 
encode_cats <- function(data, ...) {
  if (missing(...)) {
    warn_missing_dots(TRUE)
    return(invisible(data))
  } else {
    # pivot wont accept a direct unquoting so is done here
    vars <- names(dplyr::select(data, !!! dplyr::enquos(...) ))
  }
  
  # warn of and ignore variables with only two or fewer categories, excluding NA. they
  # don't benefit from this function's encoding and would be better encoded
  # using encode_binary_cats()
  data %>%
    # count distinct values
    dplyr::summarise(dplyr::across(dplyr::all_of(vars), dplyr::n_distinct, na.rm = T)) %>%
    # are there more than 2 distinct values (per variable)?
    dplyr::summarise(dplyr::across(dplyr::all_of(vars), ~ .x <= 2)) %>%
    pivot_longer(dplyr::everything(), names_to = "var", values_to = "lessthaneq_2_unique_vals") %>%
    dplyr::filter(.data$lessthaneq_2_unique_vals) %>%
    dplyr::pull(.data$var) ->
    unchanged_vars
    
  if(length(unchanged_vars) > 0) {
    # remove vars with 2 or fewer distinct non-missing values
    vars <- vars[which(!vars %in% unchanged_vars)]
    # warn user
    warning("The following variables specified in `...`, contain 2 or fewer distinct non-missing values:\n- ",
            paste0(unchanged_vars, collapse = "\n- "), "\nThese variables may be more suitable for other functionality such as eHDPrep::encode_binary_cats(). They will not be modified here.", call. = FALSE)
  } else {}
  
  # one hot encoding
  data %>%
    dplyr::mutate(.OHE_n = 1) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(vars),
                        values_to = ".OHE_value",
                        names_to = ".OHE_name") %>%
    tidyr::pivot_wider(names_from = c(.data$.OHE_name,
                                      .data$.OHE_value),
                       values_from = .data$.OHE_n,
                       values_fill = 0,
                       names_sort = TRUE,
                       names_repair = "universal")
}

##' Convert data frame to numeric matrix
##'
##' Converts all columns to numeric and uses the row identifier column
##' (\code{id_var}) as row names.
##'
##' @param id_var An unquoted  expression which corresponds to a variable in
##'   \code{data} which identifies each row.
##' @inheritParams nums_to_NA
##' @return Numeric matrix with \code{id_var} values as row names
##' @importFrom magrittr %>%
##' @importFrom dplyr mutate enquo as_label across
##' @importFrom tibble column_to_rownames
##' @export
##' @examples
##' require(dplyr)
##' require(magrittr)
##' mtcars %>%
##'   dplyr::as_tibble(rownames = "id") %>%
##'   encode_as_num_mat(id)
encode_as_num_mat <- function(data, id_var) {
  # `id_var` presence check
  if(missing(id_var)) {
    stop("`id_var` must be supplied.", call. = FALSE)
    } else {id_var <- enquo(id_var)}

  tryCatch({
    data %>%
      tibble::column_to_rownames(dplyr::as_label(id_var)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  }, warning = function(w) {
    stop("There was a problem with converting `data` to a numeric matrix. 
         This is most likely because some columns were not prepared correctly. 
         Please see the vignette for information on how to process the dataset for 
         full numeric conversion: `vignette('Introduction_to_eHDPrep', package = 'eHDPrep')`
         ", call. = FALSE)
    return(NULL)
  }
  )
    
}

#' Missing dots warning
#' 
#' Internal function. Warns if dots (...) argument have not been supplied
#' @param test expression to test.
#'
#' @return warning to user that no values were modified
warn_missing_dots <-function(test) {
  if(test) {
    warning("No argument was given for `...` so no values were modified", call. = FALSE)
  } else {}
}

#' Extract labels and levels of ordinal variables in a dataset
#'
#' This function enables preservation of the text labels for ordinal variables in
#'  a dataset in preparation for conversion to a numeric matrix. A table is produced
#'  which retains the mappings between the text labels and the
#' numerical labels for future reference.
#'
#' @param data data frame with ordinal variables with labels and levels to be
#'   extracted.
#' @param out_path Optional string. Path to write output to. If not supplied, \R
#'   object will be returned.
#'
#' @return Tibble of text label and (numerical) level mappings
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' require(magrittr)  # for %>%
#' 
#' # create an example class_tbl object
#' # note that diabetes_type is classed as ordinal yet is not modified as its
#' # levels are not pre-coded. It should instead be encoded with encode_ordinals().
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
#' # show unqiue values for t_stage in pre-QC example_data 
#' unique(example_data$t_stage)
#' 
#' # apply quality control to example_data
#' apply_quality_ctrl(example_data, patient_id, data_types,
#' bin_cats =c("No" = "Yes", "rural" = "urban"),  min_freq = 0.6) %>%
#' ordinal_label_levels -> res
#' 
#' # examine the labels and levels of t_stage in post-QC example_data
#' dplyr::filter(res, variable == "t_stage")
#' 
ordinal_label_levels <- function(data, out_path = NULL) {
  
  data %>%
    dplyr::select(where(is.ordered)) ->
    data_ord
  
  data_ord %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "label") ->
    labels
   
  data_ord %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "level") %>%
    dplyr::select(.data$level) ->
    levels 
  
  dplyr::bind_cols(labels, levels) %>%
    dplyr::distinct(dplyr::across(dplyr::everything())) %>%
    dplyr::arrange(.data$variable, .data$level) ->
    out
  
  # optionally return output
  if(!is.null(out_path)) {
    readr::write_csv(x = out, file = out_path)
  } else {return(out)}

}
