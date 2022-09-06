#    Copyright (C) 2021 Queens University Belfast
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

##' Kable logical data highlighting
##'
##' Adds colour highlighting to cell values if they are encoded as logical
##' values. Output should then be passed to \code{knitr}'s \code{kable}
##' function.
##'
##' This is useful for identifying the encoding used in a value (e.g. the
##' difference between the string "TRUE" and truth value of logic \code{TRUE}).
##' This highlighting can also be useful when visually assessing cell values in
##' a table. The colour naming format (HTML or LaTeX) is automatically detected.
##' There are four cell types considered:
##' \enumerate{
##'    \item non-logical cells are coloured black)
##'    \item{\code{TRUE} cells are coloured red (default) or green if \code{rg}
##'    is \code{TRUE}}
##'    \item{\code{FALSE} cells are coloured cyan (default) or red if \code{rg}
##'    is \code{TRUE}}
##'    \item{\code{NA} cells are coloured gray}
##'    }
##'
##' Note: When passed to \code{kable()}, the \code{escape} parameter should be
##' \code{FALSE} for colours to be rendered correctly.
##'
##' @param .data Table to be highlighted.
##' @param rg Should red and green be used for \code{TRUE} and \code{FALSE} values, respectively? If FALSE (default), colour-blind-friendly colours are applied.
##' @importFrom magrittr %>%
##' @importFrom dplyr mutate_all
##' @importFrom knitr is_latex_output
##' @importFrom kableExtra cell_spec
##' @return Table with cell colours specified.
##' @seealso \code{\link[knitr]{kable}} \code{\link[kableExtra]{cell_spec}}
cellspec_lgl <- function(.data, rg = FALSE) {

  format <- ifelse(knitr::is_latex_output(), "latex", "html")

  true_colour <- ifelse(rg, "green", "red")
  false_colour <- ifelse(rg, "red", "cyan")

  .data %>%
    dplyr::mutate_all(~cell_spec(.x, format = format,
                          color = dplyr::case_when(is.na(.x) ~ "lightgray",
                                            is.logical(.x) & .x == TRUE ~ true_colour,
                                            is.logical(.x) & .x == FALSE  ~ false_colour,
                                            TRUE ~ "black")))
}

##' Compare unique values before and after data modification
##'
##' Performs comparison of variables before and after a change has been applied
##' in order to allow manual inspection and review of modifications made during
##' the dataset preparation process.
##'
##' The purpose of this function is to summarise individual alterations in a
##' dataset and works best with categorical variables. The output contains two
##' tables derived from the parameters \code{before_tbl} and \code{after_tbl}.
##' Each table shows the unique combinations of values in variables specified in
##' the parameter \code{cols2compare} if the variable is present. The tables are
##' presented as two sub-tables and therefore share a single table caption. This
##' caption is automatically generated describing the content of the two
##' sub-tables when the parameter \code{caption} is not specified. The
##' default output is a \code{kable} containing two sub-kables however if the
##' parameter \code{kableout} is \code{FALSE}, a list containing the two
##' \code{tibble}s are returned. This may preferable for further analysis on the
##' tables' contents.
##'
##' @param cols2compare Variables to compare between tables.
##' @param before_tbl Data frame from before modification was made.
##' @param after_tbl  Data frame from after modification was made.
##' @param kableout Should output be a \code{kable} from \code{knitr}? If not,
##'   returns a \code{tibble}. (Default: TRUE)
##' @param caption Caption for \code{kable}'s \code{caption} parameter.
##' @param only_diff Keep only rows which differ between the tables (good for
##'   variables with many unique values, such as numeric variables).
##' @param latex_wrap Should tables be aligned vertically rather than
##'   horizontally? Useful for wide table which would otherwise run off a page
##'   in LaTeX format.
##'
##' @importFrom magrittr %>%
##' @importFrom dplyr select group_by_all tally arrange ungroup mutate_all
##'   any_of desc n
##' @importFrom knitr kable is_latex_output opts_current
##' @importFrom kableExtra cell_spec kable_styling
##' @return Returns list of two tibbles or a kable (see \code{kableout}
##'   argument), each tallying unique values in specified columns in each input
##'   table.
##' @export
##' @examples
##' # merge data as the example modification
##' example_data_merged <- merge_cols(example_data, diabetes_type, diabetes, 
##' "diabetes_merged", rm_in_vars = TRUE)
##' 
##' # review the differences between the input and output of the variable merging step above:
##' count_compare(before_tbl = example_data,
##'               after_tbl = example_data_merged,
##'                             cols2compare = c("diabetes", "diabetes_type", "diabetes_merged"),
##'                             kableout = FALSE)

count_compare <- function(cols2compare, before_tbl = NULL, after_tbl = NULL, only_diff = FALSE,
                          kableout = TRUE, caption = NULL, latex_wrap = FALSE) {

  # preprocess ----------------------------------------------------------------------------
  # subtable names
  a_left <- ifelse(is_latex_output(),"a","left")
  b_right <- ifelse(is_latex_output(),"b","right")
  # caption
  if(is.null(caption)) {
    # TODO: Update caption to adjust for only_diff
    caption = paste0("A comparison of the combinations of unique values of ",
                     paste0(cols2compare, collapse = ", "),
                     " in `",
                     deparse(substitute(before_tbl)),
                     "` (", a_left, ") before modification and `",
                     deparse(substitute(after_tbl)),
                     "` (", b_right, ") after modification. Each row contains a unique combination
                  of the assessed variables with the column `n` representing the
                  frequency of each combination's occurrence.")
  }

  # subset using cols2compare
  before_tbl <- dplyr::select(before_tbl,
                              dplyr::any_of(cols2compare))
  after_tbl <- dplyr::select(after_tbl,
                              dplyr::any_of(cols2compare))

  # optionally retain only values which differ (good for num vars)
  if(only_diff) {
    before_tbl2 <- dplyr::anti_join(before_tbl,after_tbl)
    after_tbl <- dplyr::anti_join(after_tbl,before_tbl)
    before_tbl <- before_tbl2
  }
  else{}

  # tally -----------------------------------------------------------------------------
  
  # before_tbl
  before_tbl %>%
    dplyr::group_by_all() %>% # apply groups to all (for tallying)
    dplyr::tally() %>% # add tally column (n)
    dplyr::arrange(dplyr::desc(n)) %>% # order by descending n
    dplyr::ungroup() %>% # remove groupings
    { if(kableout) cellspec_lgl(.) else .} -> # table formatting
    before_tbl

  # after_tbl
  after_tbl %>%
    dplyr::group_by_all() %>% # apply groups to all (for tallying)
    dplyr::tally() %>% # add tally column (n)
    dplyr::arrange(dplyr::desc(n)) %>% # order by descending n
    dplyr::ungroup() %>% # remove groupings
    { if(kableout) cellspec_lgl(.) else .} -> # table formatting
    after_tbl


  both_tbl <- list("before_tbl" = before_tbl, "after_tbl" = after_tbl)

  #PDF output --------------------------------------------------------------------
  if (knitr::is_latex_output() & kableout) {

    # caption
    if(is.null(caption)) {
      caption <- paste0("A comparison of the combinations of unique values of ",
                        paste0(cols2compare, collapse = ", "),
                        " in `",
                        deparse(substitute(before_tbl)),
                        "` (a) before modification and `",
                        deparse(substitute(after_tbl)),
                        "` (b) after modification. Each row contains a unique combination
                    of the assessed variables with the column n representing the
                    frequency of each combination's occurrence.")
    } else {}

    caption <- gsub("\\_", "\\\\_", caption)

    if(latex_wrap) {
      # splits the two tables across two lines, for if they overlap
      cat(c("\\begin{table}[!h]
                      \\caption{",caption,"}
                      \\label{tab:",knitr::opts_current$get("label"),"}
                      \\begin{subtable}{1\\linewidth}
                        \\centering
                          \\caption{}
                          \\begin{tabular}{ll} ",
            knitr::kable(before_tbl, escape = F),
            " \\end{tabular}
                      \\end{subtable}
                      \\newline
                      \\vspace{2mm} %2mm vertical space
                      \\newline
                      \\begin{subtable}{1\\linewidth}
                        \\centering
                          \\caption{}
                          \\begin{tabular}{ll} ",
            knitr::kable(after_tbl, escape = F),
            " \\end{tabular}
                      \\end{subtable}
                      \\end{table}"), sep = "")

    } else if (latex_wrap == FALSE) {
      cat(c("\\begin{table}[!htb]
                        \\caption{",caption,"}
                        \\label{tab:",knitr::opts_current$get("label"),"}
                        \\begin{subtable}{.5\\linewidth}
                          \\centering
                            \\caption{}
                            \\begin{tabular}{ll} ",
            knitr::kable(before_tbl, escape = F),
            " \\end{tabular}
                        \\end{subtable}%
                        \\begin{subtable}{.5\\linewidth}
                          \\centering
                            \\caption{}
                            \\begin{tabular}{ll} ",
            knitr::kable(after_tbl, escape = F),
            " \\end{tabular}
                          \\end{subtable}
                        \\end{table}"), sep = "")

    }
  }
  # Non-latex kable output ------------------------------------------------------
  else if (!(knitr::is_latex_output()) & kableout) { # for other outputs
    knitr::kables(list(
      knitr::kable(before_tbl, escape = F),
      knitr::kable(after_tbl, escape = F)
    ),
    caption = caption
    ) %>%
      kableExtra::kable_styling(bootstrap_options = c("bordered"), full_width = F)
  }
  # List output ----------------------------------------------------------------
  else {
    return(both_tbl)
  }

}


#' Data modification tracking
#'
#' This function produces a table
#' where each row represents a value in a variable which is present in the
#' cleaned dataset and which has been modified. The identifier, original and
#' modified value, modification type, and variable names in the original and
#' modified datasets are recorded.
#' 
#' @param before_tbl Data frame from before modifications were made.
#' @param after_tbl  Data frame from after modifications were made.
#' @param id_var An unquoted expression which corresponds to a variable in both
#'   \code{before_tbl} and \code{after_tbl} which identifies each row. Required.
#' @param vars2compare Character vectors of variable names to compare.
#' @param plot Should a plot be returned instead of a table of results? Default:
#'   \code{FALSE}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr select group_by_all full_join mutate ungroup mutate_all
#'   all_of rename filter case_when add_row n between
#' @importFrom tibble tibble rownames_to_column
#' @importFrom scales percent_format
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot aes geom_bar ylab xlab scale_y_continuous
#'   theme_light theme scale_fill_manual element_blank expansion
#' @return Table containing row-level modification records or plot summarising
#'   modifications.
#' @export
#' @examples
#' # merge data as the example modification
#' 
#' require(magrittr)
#' 
#'  # example with one modification type (removal)
#'  # return table
#'   mod_track(example_data, strings_to_NA(example_data), patient_id)
#'  
#'  # return plot
#'   mod_track(example_data, strings_to_NA(example_data), patient_id, plot = TRUE)
#' 
#'  # example with multiple modification types (removal, substitution and addition)
#' example_data %>%
#'    strings_to_NA() %>%
#'    merge_cols(diabetes_type, diabetes) ->
#'    modded_data
#' 
#' # return table
#' mod_track(example_data, modded_data, patient_id, vars2compare = c("t_stage",
#' "diabetes_type_diabetes_merged" = "diabetes", "diabetes_type_diabetes_merged"
#' = "diabetes_type"), plot = FALSE)
#' 
#' # return plot
#' mod_track(example_data, modded_data, patient_id, vars2compare = c("t_stage",
#' "diabetes_type_diabetes_merged" = "diabetes", "diabetes_type_diabetes_merged"
#' = "diabetes_type"), plot = TRUE)
mod_track <- function(before_tbl, after_tbl,
                          id_var, plot = FALSE, vars2compare) {
  
  # enquo id_var
  if(missing(id_var)) {stop("`id_var` must be supplied.")} else{id_var <- enquo(id_var)}
  # specify variables to compare
  if(missing(vars2compare)) {
    message("`vars2compare` not supplied. Attempting to compare all variables...")
    vars2compare <- names(before_tbl)[which(names(before_tbl) != dplyr::as_label(id_var))]
  }

  # convert named vec to tbl. fill empty names
  tibble::enframe(vars2compare, "new_var", "old_var") %>%
    dplyr::mutate(dplyr::across(where(is.numeric), function(x) "")) %>% # for when no names are provided
    dplyr::mutate(new_var = dplyr::if_else(nchar(.data$new_var) == 0, .data$old_var, .data$new_var)) ->
    vars_tbl

  # check if vars exist in both tbls
  report_var_mods(before_tbl, after_tbl) %>%
    filter((.data$variable %in% vars_tbl$old_var) & .data$presence != "Preserved") ->
    missing_vars

  if(nrow(missing_vars) > 0) {
    warning(paste0("\t - `",missing_vars$variable, "` has been ",
                   tolower(missing_vars$presence),
                   " during QC and so cannot be compared.\n"), call. = FALSE)

    vars_tbl %>%
      filter(!(.data$old_var %in% missing_vars$variable) &
               !(.data$new_var %in% missing_vars$variable)) ->
      vars_tbl
    
  } else {}

  before_tbl %>%
    dplyr::select(!! id_var, any_of(vars_tbl$old_var)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(!(!! id_var), names_to = "old_var", values_to = "old_value") ->
    before_tbl

  after_tbl %>%
    dplyr::select(!! id_var, any_of(vars_tbl$new_var)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(!(!! id_var), names_to = "new_var", values_to = "new_value") ->
    after_tbl

  vars_tbl %>%
    dplyr::left_join(before_tbl, by = "old_var") %>%
    dplyr::left_join(after_tbl, by = c("new_var", dplyr::as_label(id_var))) %>%
    dplyr::select(!! id_var, everything()) %>%
    dplyr::mutate(mod_type = dplyr::case_when(.data$old_value == .data$new_value ~ "None",
                                              is.na(.data$old_value) & is.na(.data$new_value) ~ "None",
                                              is.na(.data$old_value) & !is.na(.data$new_value) ~ "Addition",
                                              !is.na(.data$old_value) & is.na(.data$new_value) ~ "Removal",
                                              !is.na(.data$old_value) & !is.na(.data$new_value) ~ "Substitution",
                                              TRUE ~ "unexpected_difference"
                                              )
                  ) ->
    out_tbl
  
  num_comparable_rows <- out_tbl %>%
    pull(!! id_var) %>%
    unique() %>%
    length()

  # return plot or tbl (based on plot parameter)
  if (plot) {
    out_tbl %>%
      dplyr::select(!! id_var, .data$mod_type) %>% # select ID and modification type
      #dplyr::filter(.data$mod_type != "None") %>%
      dplyr::group_by_all() %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(n = ifelse(.data$mod_type == "None", 0, .data$n)) %>% # exclude "None" mods - allows all rows to be shown.
      dplyr::mutate(mod_type = factor(.data$mod_type,
                                      levels = c("Addition", "Substitution", "Removal"))) %>%
      dplyr::mutate(perc = n / num_comparable_rows) %>% # as percentage of comparable values
      ggplot2::ggplot(
        ggplot2::aes(
          fill = .data$mod_type,
          y = as.numeric(.data$perc),
          x = stats::reorder(!! id_var, as.numeric(.data$perc), sum)
        )
      ) +
      ggplot2::geom_bar(position = "stack", stat = "identity") +
      ggplot2::ylab("Proportion of Values Modified") +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::xlab(dplyr::as_label(id_var)) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.text.x=ggplot2::element_blank(), # remove x axis text
                     axis.ticks.x=ggplot2::element_blank(), # remove x axis ticks
                     panel.grid.minor = ggplot2::element_blank(), # remove minor grid
                     panel.grid.major.x = ggplot2::element_blank()) + # remove x axis major grid
      ggplot2::scale_fill_manual(values= stats::setNames(c(#"#3B0F70FF",
                                                           "#8C2981FF",
                                                           "#DE4968FF",
                                                           "#FE9F6DFF"),c(
                                                             #"None",
                                                             "Addition",
                                                             "Removal",
                                                             "Substitution")),
                                 name = "Modification\nType", drop = FALSE)
  } else{
    return(filter(out_tbl, .data$mod_type != "None"))
  }
}

#' Track changes to dataset variables
#'
#' Reports if variables have been added, removed, or are preserved between two
#' data frames. Intended to be used to review quality control / data
#' preparation.
#'
#' @inheritParams mod_track
#' @return Tibble containing two columns. `variable` contains name of each
#'   variable. `presence` contains the presence of the variable in
#'   \code{after_tbl}.
#' @importFrom dplyr if_else mutate  full_join
#' @importFrom tibble as_tibble_col
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' example_data_merged <- merge_cols(example_data, diabetes_type,
#' diabetes, "diabetes_merged", rm_in_vars = TRUE)
#'
#' report_var_mods(example_data, example_data_merged)
report_var_mods <- function(before_tbl = NULL, after_tbl = NULL) {

  # get names
  pre_QC_vars <- names(before_tbl)
  post_QC_vars <- names(after_tbl)

  # original variables
  before_tbl %>%
    names() %>%
    tibble::as_tibble_col(column_name = "variable") %>%
    # preserved or removed variables?
    dplyr::mutate(presence = dplyr::if_else(.data$variable %in% post_QC_vars,
                              "Preserved",
                              "Removed")) ->
    pre_QC_vars_tbl

  # final variables
  after_tbl %>%
    names() %>%
    tibble::as_tibble_col(column_name = "variable") %>%
    # preserved or added variables?
    dplyr::mutate(presence = dplyr::if_else(.data$variable %in% pre_QC_vars,
                              "Preserved",
                              "Added")) ->
    post_QC_vars_tbl

  dplyr::full_join(pre_QC_vars_tbl,post_QC_vars_tbl, by = c("variable", "presence"))
}
