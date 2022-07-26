#    Copyright (C) 2022 Queens University Belfast
#    
#    This file is part of eHDPrep
#
#    eHDPrep is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    eHDPrep is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with eHDPrep. If not, see <http://www.gnu.org/licenses/>.

###
##### Import dataset #####
###
#' Import data into R
#'
#' Imports a rectangular single table into \R from a .xls, .xlsx, .csv, or .tsv file.
#' 
#' First row is interpreted as column headers by default. For more details see
#' \code{\link[readxl]{read_excel}} (.xlsx/.xls), \code{\link[readr]{read_csv}} (.csv), or
#' \code{\link[readr]{read_tsv}} (.tsv).
#'
#' @param file Character constant. Path to file.
#' @param format Character constant. "excel" (default, for .xls or.xlsx files),
#'   csv", or "tsv".
#' @param ... Parameters to pass to \code{\link[readxl]{read_excel}},
#'   \code{\link[readr]{read_csv}} or \code{\link[readr]{read_tsv}}

#' @importFrom readxl read_excel
#' @importFrom readr read_csv read_tsv
#' @return data as a \code{tibble}
#' @seealso \code{\link[readxl]{read_excel}} for additional parameters for
#'   importing .xls or .xlsx files, \code{\link[readr]{read_csv}} for .csv
#'   files, \code{\link[readr]{read_tsv}} for .tsv files
#' @family import to/export from \R functions
#' @export
#' @examples
#' \dontrun{
#'    # This code will not run
#'    # ./dataset.xlsx should be replaced with path to user's dataset
#'    
#'    # excel
#'    import_dataset(file = "./dataset.xlsx", format = "excel")
#'    #csv
#'    import_dataset(file = "./dataset.csv", format = "csv")
#'    #tsv
#'    import_dataset(file = "./dataset.tsv", format = "tsv")
#' }
#' 
import_dataset <- function(file, format = "excel", ...) {
  if(format == "excel") {
    readxl::read_excel(file, ...)
  } else if (format == "tsv") {
    readr::read_tsv(file = file, ...)
  }
    else if (format == "csv") {
      readr::read_csv(file = file, ...)

    } else {
      stop(
        "`format` must be one of 'csv' or 'tsv'.", call. = FALSE)
    }

}


###
##### Define datatypes #####
###

#' Assume variable classes in data
#'
#' Classes/data types of data variables are assumed with this function and
#' exported to a .csv file for amendment. Any incorrect classes can then be
#' corrected and imported using \code{\link{import_var_classes}}.
#'
#' @param data data frame
#' @param out_file file where variables and their assumed classes are stored for
#'   user verification. Default: "./datatypes.csv"
#'
#' @return Writes a .csv file in the working directory (default ‘datatypes.csv’)
#'   containing the variables and their assumed data types / classes.
#' @seealso \code{\link[eHDPrep]{import_var_classes}}
#' @export
#'
#' @examples
#' # example below assumes incorrectly for several variables
#' ## Not run:
#' \dontrun{
#'    data(example_data)
#'    assume_var_classes(example_data)
#' }
#' ## End(Not run)
assume_var_classes <- function(data, out_file = "./datatypes.csv") {
  data %>%
    purrr::map_chr(class) %>%
    tibble::enframe(name = "var", value = "datatype") ->
    classes

  readr::write_csv(classes, file = out_file, na = "")
}

#' Import corrected variable classes
#'
#' Reads in output of \code{\link{assume_var_classes}}, ensures all specified
#' datatypes are one of ("id", "numeric", "double", "integer", "character",
#' "factor","ordinal", "genotype", "freetext", "logical") as required for high
#' level eHDPrep functions.
#'
#' @param file character string. Path to output of
#'   \code{\link{assume_var_classes}} which should be manually verified outside
#'   of \R and corrected where any data type is incorrect.
#'
#' @return data frame containing the data type values of variables, as described
#'   in \code{file}
#' @seealso \code{\link[eHDPrep]{assume_var_classes}}
#' @export
#'
#' @examples
#' ## Not run:
#' \dontrun{
#'    data(example_data)
#'    assume_var_classes(example_data)
#'    import_var_classes()
#'  }
#' ## End(Not run)
import_var_classes <- function(file = "./datatypes.csv") {
  var_classes <- readr::read_csv(file)
  permitted_datatypes <- c("id", "numeric", "double", "integer", "character", "factor",
                           "ordinal", "genotype", "freetext", "logical")
  # verify data structure
  if(!((all(names(var_classes) == c("var","datatype"))) &
       (length(names(var_classes)) == 2))) {

    stop("File specified by `file` must have two columns.
         They must be named 'var' and 'datatype'", call. = FALSE)

  } else if (!all(var_classes$datatype %in% permitted_datatypes)) { # verify datatypes

    stop("Values in `datatype` must be one of ",paste0(permitted_datatypes, ", "),"\n",
         paste0("\u2716 ",
                var_classes$datatype[which(!var_classes$datatype %in% permitted_datatypes)],
                                     " is not a permitted datatype\n"),
         call. = FALSE
    )
  } else{return(var_classes)}

}

#' Find variable names of a specified data type in class_tbl
#'
#' Internal function
#' 
#' @param class_tbl output of \code{\link{import_var_classes}}
#' @param extract_class character vector of classes to import
#' @param negate If TRUE, return non-matching elements. Default: FALSE
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @return character vector of variable names matching \code{extract_class}
#' @noRd
select_by_datatype <- function(class_tbl, extract_class, negate = FALSE) {
  # TODO: remove reference to class and replace with data type
  
  if(negate) {
    class_tbl %>%
      dplyr::filter(!(.data$datatype %in% extract_class)) %>%
      dplyr::pull(.data$var)
  } else {
    class_tbl %>%
      dplyr::filter(.data$datatype %in% extract_class) %>%
      dplyr::pull(.data$var)
  }

}

###
##### Export dataset #####
###
#' Export data to delimited file
#'
#' Save dataset in .csv or .tsv format. A wrapper function for \code{readr}'s
#' \code{\link[readr]{write_csv}} and \code{\link[readr]{write_tsv}}.
#'
#' @param format Character constant. "csv" (default) or "tsv"
#' @param ... parameters to pass to \code{\link[readr]{write_csv}} or \code{\link[readr]{write_tsv}}.
#' @inheritParams readr::write_csv
#' @importFrom readr write_csv write_tsv
#' @seealso \code{\link[readr]{write_csv}} and \code{\link[readr]{write_tsv}}
#' @family import to/export from \R functions
#' @export

export_dataset <- function(x, file, format = "csv", ...) {
  stopifnot(format %in% c("csv","tsv"))

  if(format == "csv") {
    readr::write_csv(x, file, ...)
  } else if (format == "tsv") {
    readr::write_tsv(x, file, ...)
  } else {stop(
    "`format` must be one of 'csv' or 'tsv'.",
    call. = FALSE)}
}
