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

#' Validate internal consistency table
#'
#' Runs a series of checks on a table of internal consistency rules
#' (see Consistency Table Requirements) in preparation for \code{\link{identify_inconsistency}}.
#'
#' @section Consistency Table Requirements: Table must have exactly five character columns.
#'   The columns should be ordered according to the list below which describes the
#'   values of each column:
#'   \enumerate{
#'   \item First column name of data values that will be subject to 
#'   consistency checking. String. Required.
#'   \item Second column name of data values that will be subject to 
#'   consistency checking. String. Required.
#'   \item Logical test to compare columns one and two. One of: ">",">=",
#'   "<","<=","==", "!=". String. Optional if columns 4 and 5 have non-\code{NA} values.
#'   \item Either a single character string or a colon-separated range of
#'   numbers which should only appear in column A. Optional if column 3 has a
#'   non-\code{NA} value.
#'   \item Either a single character string or a colon-separated range of
#'   numbers which should only appear in column B given the value/range
#'   specified in column 4. Optional if column 3 has a non-\code{NA} value.
#'   }
#'   Each row should detail one test to make.
#'   Therefore, either column 3 or columns 4 and 5 must contain non-\code{NA}
#'   values.
#'
#' @param data data frame which will be checked for internal consistency
#' @param consis_tbl data frame or tibble containing information on internal
#'   consistency rules (see "Consistency Table Requirements" section)
#' @importFrom rlang .data
#' @importFrom dplyr mutate filter pull
#' @importFrom purrr map_int map_lgl
#' @return Error message or successful validation message is printed.
#'   The dataset is returned invisibly.
#'@family internal consistency functions
#' @export
#' @examples
#' require(tibble)
#' # example with synthetic dataset on number of bean counters
#' # there is a lot going on in the function so a simple dataset aids this example
#' #
#' # creating `data`:
#' beans <- tibble::tibble(red_beans = 1:15,
#' blue_beans = 1:15,
#' total_beans = 1:15*2,
#' red_bean_summary = c(rep("few_beans",9), rep("many_beans",6)))
#' #
#' # creating `consis_tbl`
#' bean_rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
#' "red_beans", "blue_beans", "==", NA, NA,
#' "red_beans", "total_beans", "<=", NA,NA,
#' "red_beans", "red_bean_summary", NA, "1:9", "few_beans",
#' "red_beans", "red_bean_summary", NA, "10:15", "many_beans")
#'
#' validate_consistency_tbl(beans, bean_rules)
validate_consistency_tbl <- function(data, consis_tbl) {
  # Have both data and consis_tbl been supplied
  if(missing(data)) {
    stop("\u2716 `data` is missing and must be supplied.", call. = FALSE)
  } else if (missing(consis_tbl)) {
    stop("\u2716 `consis_tbl` is missing and must be supplied.", call. = FALSE)
  } else {}
  
  # check if consis_tbl has 5 cols? --------------------------------------------------------
  if (!(ncol(consis_tbl) == 5)) {
    stop("\u2716 `consis_tbl` must have five columns. You have supplied a `consis_tbl` with ",
         ncol(consis_tbl), " columns.", call. = FALSE)
  } else{}
  
  
  # check all listed variables are in dataset (1,2) ----------------------------------
  consis_tbl %>%
    mutate(var_a_not_in_data = !.[[1]] %in% names(data)) %>%
    mutate(var_b_not_in_data = !.[[2]] %in% names(data)) ->
    missing_vars
  
  missing_a_vars <- dplyr::filter(missing_vars, .data$var_a_not_in_data) %>% dplyr::pull(1)
  missing_b_vars <- dplyr::filter(missing_vars, .data$var_b_not_in_data) %>% dplyr::pull(2)
  
  if(length(missing_a_vars) > 0) {
    stop("All values in the first column of `consis_tbl` must be variable names in `data`.\n
         \u2716 The following values are not in `data`:\n", 
         paste0(c("- ", missing_a_vars), collapse = "\n- "), call. = FALSE)
  } else if (length(missing_b_vars) > 0) {
    stop("All values in the second column of `consis_tbl` must be variable names in `data`.\n
         \u2716 The following values are not in `data`:\n", 
         paste0(c("- ", missing_b_vars), collapse = "\n- "), call. = FALSE)
  } else{}
  
  # check either col 3 or col 4 and 5 are non-missing -----------------------------------
  consis_tbl %>%
    dplyr::mutate(
      # are all 3 missing?
      missing_all_tests = is.na(.[[3]]) & is.na(.[[4]]) & is.na(.[[5]]),
      # is lgl test present but range tests are also present?
      lgl_and_range = !is.na(.[[3]]) & (!is.na(.[[4]]) | !is.na(.[[5]])),
      # are one of the range tests present but not the other?
      missing_range = (is.na(.[[4]]) & !is.na(.[[5]])) | (!is.na(.[[4]]) & is.na(.[[5]]))
      ) ->
    missing_vals
  
  if(any(dplyr::pull(missing_vals, .data$missing_all_tests))) {
    stop("There must (exclusively) be a value in either col 3 or in col 4 and 5.\n\u2716 Rows ",
         paste0(which(dplyr::pull(missing_vals, .data$missing_all_tests)), collapse = ", "),
         " are missing all tests.")
  } else if (any(dplyr::pull(missing_vals, .data$lgl_and_range))) {
    stop("There must (exclusively) be a value in either col 3 or in col 4 and 5.\n\u2716 Rows ",
         paste0(which(dplyr::pull(missing_vals, .data$lgl_and_range)), collapse = ", "),
         " contain values in col 3 and 4 and/or 5.")
  } else if (any(dplyr::pull(missing_vals, .data$missing_range))) {
    stop("If there is a value in either col 4 or 5, there must be a value in the other\n\u2716 Rows ",
         paste0(which(dplyr::pull(missing_vals, .data$missing_range)), collapse = ", "),
         " do not adhere to this.")
  } else{}

  # check logical tests column (3) -----------------------------------------------------
  valid_lgl_tests <- c("<","<=","==","!=",">",">=",NA)
  if(any(!dplyr::pull(consis_tbl,3) %in% valid_lgl_tests)) {
    stop("\u2716 Values in the third column of `consis_tbl` must be one of:\n<,<=,==,!=,>,>=,NA",
         call. = FALSE)
    # TODO: Expand error to say which values are invalid
  } else{}
  
  # check ranges cols (4,5) -------------------------------------------------------------
  consis_tbl %>%
    # check var_a_range and var_b_range split  (by colon) to either no, one, or two values
    dplyr::mutate(var_a_range_check = stringr::str_split(.[[4]], ":") %>%
                    purrr::map_int(length) %>%
                    purrr::map_lgl(function(x) !(x >= 0 & x <= 2))
                  ) %>%
    dplyr::mutate(var_b_range_check = stringr::str_split(.[[5]], ":") %>%
                    purrr::map_int(length) %>%
                    purrr::map_lgl(function(x) !(x >= 0 & x <= 2))
                  ) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    # retain problematic rows only
    dplyr::filter(.data$var_a_range_check == TRUE | .data$var_b_range_check == TRUE) ->
    consis_tbl_range_check
  
  if(nrow(consis_tbl_range_check) > 0) {
    stop(paste0(c("\u2716 Values in columns 4 and 5 should represent ranges of values to check.
         They must be a single value (for categories) or a string containing two colon-seperated values
         for numeric ranges (e.g. '1:10').
         The following rows are invalid as they do not conform to this:",
                consis_tbl_range_check$row), collapse = "\n - "),
         call. = FALSE)
  }

  # Return ----------------------------------------------------------------------------------
  message("Consistency table is valid.")
  invisible(data)
}

#' Import internal consistency file
#'
#' Internal. Imports file specified in \code{path} into \R as a \code{tibble}. The file is
#' validated using \code{\link{validate_consistency_tbl}}.
#'
#' @inheritParams import_dataset
#' @noRd
#' @return A tibble containing internal consistency information
import_consistency_file <- function(file = NULL, format = "excel") {
  import_dataset(file, format) %>%
    validate_consistency_tbl()
}


#'Identify inconsistencies in a dataset
#'
#'Tests pairs of variables for consistency between their values according to
#' a table of rules or 'consistency table'.
#'
#'Multiple types of checks for inconsistency are supported:
#'\enumerate{
#' \item Comparing by logical operators (<, <=, ==, !=, >=, >)
#' \item Comparing permitted categories (e.g. cat1 in varA only if cat2 in varB)
#' \item Comparing permitted numeric ranges (e.g. 20-25 in varC only if 10-20 in
#' varD)
#' \item Mixtures of 2 and 3 (e.g. cat1 in varA only if 20-25 in varC) 
#'}
#'
#'The consistency tests rely on such rules being specified in a
#'separate data frame (\code{consis_tbl}; see section "Consistency Table Requirements").
#'
#'Variable A is given higher priority than Variable B when A is a category. If A
#'(as char) is not equal to the value in col 4, the check is not made. This is
#'to account for one way dependencies (i.e. VarA is fruit, VarB is apple)
#'
#'@inheritParams validate_consistency_tbl
#'@inheritSection validate_consistency_tbl Consistency Table Requirements
#'@param id_var An unquoted expression which corresponds to a variable in
#'  \code{data} which identifies each row.
#'@importFrom dplyr mutate summarise everything rename_with left_join rename
#'  row_number mutate if_else all_of
#'@importFrom purrr pmap map_chr
#'@importFrom magrittr %>%
#'@importFrom rlang .data
#'@return tibble detailing any identified internal inconsistencies in
#'  \code{data}, if any are found. If no inconsistencies are found, \code{data}
#'  is returned invisibly.
#'@family internal consistency functions
#'@export
#' @examples
#' require(tibble)
#' # example with synthetic dataset on number of bean counts
#' # there is a lot going on in the function so a simple dataset aids this example
#' #
#' # creating `data`:
#' beans <- tibble::tibble(red_beans = 1:15,
#' blue_beans = 1:15,
#' total_beans = 1:15*2,
#' red_bean_summary = c(rep("few_beans",9), rep("many_beans",6)))
#' #
#' # creating `consis_tbl`
#' bean_rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
#' "red_beans", "blue_beans", "==", NA, NA,
#' "red_beans", "total_beans", "<=", NA,NA,
#' "red_beans", "red_bean_summary", NA, "1:9", "few_beans",
#' "red_beans", "red_bean_summary", NA, "10:15", "many_beans")
#'
#' identify_inconsistency(beans, bean_rules)
#'
#' # creating some inconsistencies as examples
#' beans[1, "red_bean_summary"] <- "many_beans"
#' beans[1, "red_beans"] <- 10
#'
#' identify_inconsistency(beans, bean_rules)
#' 
identify_inconsistency <- function(data = NULL, consis_tbl = NULL, id_var = NULL) {
  
  # prep id_var -----------------------------------------------------------------------------------
  # add row ids if none specified
  if(is.null(id_var)) {
    data %>%
      dplyr::mutate(row = dplyr::row_number()) ->
      data
    id_var <- "row"
  } else if (!id_var %in% colnames(data)) { # check id_var is in data
    stop("`",{{id_var}}, "` is not a column name in `", deparse(substitute(data)), "`", call. = FALSE)
  } else{}

  # prep data ---------------------------------------------------------------------------------------
  # convert data to tibble of colnames and values as a single list
  data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), list)) %>%
    # Pivot to longer format
    tidyr::pivot_longer(cols = !dplyr::all_of(id_var),
                        names_to = "var",
                        values_to = "vector") ->
    vec_lists

  # prep consis_tbl ---------------------------------------------------------------------------------
  consis_tbl %>%
    # rename column names in consis_tbl
    dplyr::rename_with(function(x) c("var_a","var_b","lgl_test", "var_a_range","var_b_range")) %>%
    dplyr::mutate(lgl_incon = NA, range_incon = NA) %>% # placeholder cols for later tests
    dplyr::left_join(vec_lists, by = c("var_a" = "var")) %>% # join lists of vectors from data
    dplyr::rename("values_a" = "vector") %>%
    dplyr::left_join(dplyr::select(vec_lists,!dplyr::all_of(id_var)),
                     by = c("var_b" = "var")) %>%
    dplyr::rename("values_b" = "vector") ->
    consis_tbl

  # logical tests (3) --------------------------------------------------------------------------------
  if(!all(is.na(consis_tbl$lgl_test))) { # if not all NA
    consis_tbl %>%
      # create lgl test as string
      dplyr::mutate(lgl_values = dplyr::if_else(!is.na(.data$lgl_test),
                                         purrr::pmap(list(.data$values_a,
                                                          .data$lgl_test,
                                                          .data$values_b),
                                                     stringr::str_c),
                                         list("NA"))) %>%
      # parse and evaluate lgl test
      dplyr::mutate(lgl_incon = .data$lgl_values %>%
                      purrr::map(. %>%
                                   purrr::map_lgl(function(x) !eval(rlang::parse_expr(x))))) %>%
      dplyr::mutate(lgl_values = dplyr::na_if(.data$lgl_values, "NA")) ->
      consis_tbl
  } else{}

  # range tests (4,5) --------------------------------------------------------------------------------
  # TODO: could support missing values. Maybe inconsistency table should have (missing) as a value
  if(!all(is.na(consis_tbl$var_a_range))) { # if not all NA
    consis_tbl %>%
      # split ranges
      tidyr::separate(.data$var_a_range,
                      c("var_a_min","var_a_max"),":",remove = FALSE, convert = TRUE) %>%
      tidyr::separate(.data$var_b_range,
                      c("var_b_min","var_b_max"),":",remove = FALSE, convert = TRUE) %>%
      # tidyr::separate gives following warining, which is not helpful here:
      # # tidyr::separate warns 
      suppressWarnings() %>% 
      
      # check datatypes to inform how to compare:
      dplyr::mutate(a_ischar = purrr::map_lgl(.data$values_a, is.character)) %>%
      dplyr::mutate(b_ischar = purrr::map_lgl(.data$values_b, is.character)) %>%
      dplyr::mutate(compare_type = dplyr::case_when(
        .data$a_ischar & .data$b_ischar ~ "chr_chr",
        .data$a_ischar & !(.data$b_ischar) ~ "chr_num",
        !(.data$a_ischar) & .data$b_ischar ~ "num_chr",
        !(.data$a_ischar) & !(.data$b_ischar) ~ "num_num",
        TRUE ~ "unknown_compar")) %>%
      dplyr::select(-c(.data$a_ischar, .data$b_ischar)) %>%
      # iterate over each row and compare depending on compare_type
      dplyr::mutate(range_incon =
                      purrr::pmap(list(
                        .data$var_a_min, # ..1
                        .data$var_b_min, # ..2
                        .data$var_b_max, # ..3
                        .data$values_a, # ..4
                        .data$values_b, # ..5
                        .data$compare_type, # ..6
                        .data$var_a_max # ..7
                      ),

                      ~ dplyr::case_when(
                        
                        # when a is char and b is numeric
                        # and a's chr value == its permitted value
                        # is b in its permitted range?
                        ..6 == "chr_num" & (..1 == ..4) ~
                          !(
                            #(..1 == ..4) &
                            ((as.numeric(..2) <= ..5) &
                               (..5 <= as.numeric(..3)))
                          ),
                        # when a and b are char
                        # are they both == permitted value?
                        ..6 == "chr_chr" & (..1 == ..4) ~
                          !(
                            # (..1 == ..4) & 
                              (..2 == ..5)
                          ),
                        # when a is numeric and b is char
                        # is a in range and
                        # does b's char value == its permitted value
                        ..6 == "num_chr" & ((..1 <= ..4) & ..4 <= ..7) ~
                          !(
                            # ((..1 <= ..4) & ..4 <= ..7) &
                              (..5 == ..2)
                          ),
                        # when a and b are numeric
                        # are they both in their permitted range?
                        ..6 == "num_num" & ((..1 <= ..4) & (..4 <= ..7)) ~
                          !(
                            # ((..1 <= ..4) & (..4 <= ..7)) &
                              ((..2 <= ..5) & (..5 <= ..3))
                          ),
                        # else: false
                        TRUE ~ FALSE
                      )
                      )
      ) %>%
      suppressWarnings() -> # warns of NAs introduced but this is desired.
      consis_tbl
    
  } else{}
  
  # tidy res table -----------------------------------------------------------------
  # convert back to simple tibble (no list cols) and return internal inconsistencies
  consis_tbl %>%
    dplyr::mutate(values_a = .data$values_a %>%
                    purrr::map(. %>%
                                 purrr::map_chr(as.character))) %>%
    dplyr::mutate(values_b = .data$values_b %>%
                    purrr::map(. %>%
                                 purrr::map_chr(as.character))) %>%
    tidyr::unnest(dplyr::everything()) %>%
    dplyr::filter(.data$lgl_incon | .data$range_incon) %>%
    dplyr::select(-dplyr::any_of(c("var_a_min",
                                   "var_a_max",
                                   "var_b_min",
                                   "var_b_max",
                                   "lgl_values",
                                   "lgl_incon",
                                   "a_ischar",
                                   "b_ischar",
                                   "range_incon",
                                   "compare_type"))) ->
    res
  
  # return ---------------------------------------------------------------------------
  if(nrow(res) > 0) {
    warning("One or more inconsistencies were identified. They are shown in the returned tibble.",
            call. = FALSE)
    return(res)
  } else {
    message("No inconsistencies were found.")
    invisible(data)}
}
