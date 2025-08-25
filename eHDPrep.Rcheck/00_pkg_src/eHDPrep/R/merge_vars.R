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

#' Merge columns in data frame
#'
#' Merges two columns in a single data frame. The merging draws on the
#' functionality of \code{'dplyr'}'s \code{\link[dplyr]{coalesce}} where missing
#' values from one vector are replaced by corresponding values in a second
#' variable. The name of the merged variable is specified in
#' \code{merge_var_name}. \code{primary_var} and \code{secondary_var} can be
#' removed with \code{rm_in_vars}. Variables must be combinable (i.e. not a
#' combination of numeric and character).
#'
#' @param data data frame containing \code{primary_var} and
#'   \code{secondary_var}.
#' @param primary_var Data variable
#'   which contains the best quality / most detailed information. Missing values
#'   will be supplied by values in corresponding rows from \code{secondary_var}.
#' @param merge_var_name character constant. Name for merged variable. Default:
#'   [\code{primary_var}]_[\code{secondary_var}]_merged
#' @param rm_in_vars logical constant. Should \code{primary_var} and
#'   \code{secondary_var} be removed? Default = FALSE.
#' @param secondary_var Data variable
#'   which will be used to fill missing values in \code{primary_var}.
#' @importFrom dplyr mutate if_else coalesce select all_of select
#' @importFrom rlang .data := sym
#' @importFrom purrr map_chr
#' @importFrom dplyr as_label enquo
#' @seealso \code{\link[dplyr]{coalesce}}
#' @return data frame with coalesced \code{primary_var} and \code{secondary_var}
#' @export
#'
#' @examples
#' data(example_data)
#'
#' # preserve input variables (default)
#' res <- merge_cols(example_data, diabetes_type, diabetes)
#' dplyr::select(res, dplyr::starts_with("diabetes"))
#'
#' # remove input variables
#' res <- merge_cols(example_data, diabetes_type, diabetes, rm_in_vars = TRUE)
#' dplyr::select(res, dplyr::starts_with("diabetes"))
#'
merge_cols <- function(data, primary_var, secondary_var,
                       merge_var_name = NULL, rm_in_vars = FALSE) {

  # checks
  if(missing(data)) {
    stop("`data` argument must be supplied", call. = FALSE)
  } else if (missing(primary_var)) {
    stop("`primary_var argument must be supplied", call. = FALSE)
  } else if (missing(secondary_var)) {
    stop("`secondary_var argument must be supplied", call. = FALSE)
  } else if (!is.data.frame(data)) {
    stop("`data` must be a data frame or coercible to one", call. = FALSE)
  } else if (!(is.logical(rm_in_vars) & length(rm_in_vars) == 1)) {
    stop("`rm_in_vars` must be a logical vector of length 1", call. = FALSE)
  } else if (purrr::map_chr(dplyr::select(data, {{primary_var}}), class) != 
             purrr::map_chr(dplyr::select(data, {{secondary_var}}), class)) {
    stop("`primary_var` must be the same class as `secondary_var`", call. = FALSE)
  }
  else {}
  
  # add default if merge_var_name missing
  if(missing(merge_var_name)) {
    default_merge_var_name <- 
      paste0(c(
        dplyr::as_label(dplyr::enquo(primary_var)),
        dplyr::as_label(dplyr::enquo(secondary_var)),
        "merged"), collapse = "_")
    merge_var_name <- rlang::sym(default_merge_var_name)
  } else {}


  data %>%
    dplyr::mutate({{merge_var_name}} := dplyr::coalesce({{primary_var}}, {{secondary_var}})) ->
    data
  
  # (optionally) remove input variables
  if(rm_in_vars) {
  data %>%
    dplyr::select(-{{primary_var}}, -{{ secondary_var }})
  } else {data}

}
