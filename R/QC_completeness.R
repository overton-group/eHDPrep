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

# needed for "where" function
utils::globalVariables("where")

#' Calculate Variable Completeness in a Data Frame
#'
#' Calculates the completeness of each variable in a data frame.
#' 
#' This is achieved by comparing the number of \code{NA} to non-\code{NA}
#' values. Returns the count of \code{NA} as well as the percentage of \code{NA}
#' values and the percentage completeness.
#'
#' @param data Data frame.
#'
#' @return \code{Tibble} detailing completeness statistics for each variable.
#' @export
#' @importFrom dplyr mutate arrange desc everything across
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
#' @importFrom  rlang .data
#' @family measures of completeness
#'
#' @examples
#' data(example_data)
#' variable_completeness(example_data)
variable_completeness <- function(data) {
  num_vars <- dim(data)[2]
  num_obs <- dim(data)[1]

  data %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), is.na)) %>% # test each cell if is NA
    purrr::map_dfr(sum) %>% # sum NAs by variable
    tidyr::pivot_longer(dplyr::everything(), names_to = "Variable", values_to = "NAs") %>%
    dplyr::arrange(dplyr::desc(.data$NAs)) %>% # sort by number of NAs
    dplyr::mutate(NAs_percent = (.data$NAs/num_obs)*100) %>% # convert to %
    dplyr::mutate(Completeness = 100 - .data$NAs_percent)
}

#' Calculate Row Completeness in a Data Frame
#' 
#' Calculates the completeness of each row/observation in a data frame.
#' 
#' Row completeness is measured by comparing the number of \code{NA} to
#' non-\code{NA} values. Returns the count of \code{NA} as well as the
#' percentage of \code{NA} values and the percentage completeness.
#' 
#' @param data Data frame.
#' @param id_var Row identifier variable.
#'
#' @return Tibble detailing completeness statistics for each row in input data.
#' @importFrom dplyr mutate select enquo arrange desc across as_label
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column
#' @export
#' @family measures of completeness
#'
#' @examples
#' data(example_data)
#' row_completeness(example_data, patient_id)
row_completeness <- function(data, id_var) {
  num_vars <- dim(data)[2]
  num_obs <- dim(data)[1]

  if(missing(id_var)) {
    data <- tibble::rownames_to_column(data, var = "rowname")
    id_var <- sym("rowname")
  } else{id_var <- dplyr::enquo(id_var)}

  data %>%
    dplyr::mutate(dplyr::across(!(!!id_var), is.na)) %>% # test if each values is NA (except id_var)
    # transpose tibble
    tidyr::pivot_longer(-!!id_var, names_to="Variable", values_to=".tempvals") %>%
    tidyr::pivot_wider(names_from = !!id_var, values_from=".tempvals") %>%
    dplyr::select(-.data$Variable) %>% # remove variable name column
    purrr::map_dfr(sum) %>% # sum NAs by row
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = dplyr::as_label(id_var),
                        values_to = "NAs") %>%
    dplyr::arrange(dplyr::desc(.data$NAs)) %>%  # sort by number of NAs
    dplyr::mutate(NAs_percent = (.data$NAs/num_vars)*100) %>% # convert to %
    dplyr::mutate(Completeness = 100 - .data$NAs_percent) # completeness = 100 - NAs_percent

}

#' Plot Completeness of a Dataset
#'
#' Generates a bar plot of percentage completeness for one or both data frame
#' dimensions (rows/columns).
#'
#' @param data Data frame in tidy format (see \url{https://tidyr.tidyverse.org/}).
#' @param id_var Row identifier variable name.
#' @param plot Character vector containing one or both of \code{variables} and
#'   \code{rows}.
#'
#' @return Completeness bar plot.
#' @importFrom dplyr mutate
#' @importFrom forcats fct_relevel
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_bar aes theme_light theme xlab ylab labs
#'   scale_x_continuous scale_fill_manual
#' @export
#' @family measures of completeness
#'
#' @examples
#' data(example_data)
#' plot_completeness(example_data, patient_id, "variables")
plot_completeness <- function(data, id_var, plot = c("variables","rows")) {

  id_var <- dplyr::enquo(id_var)

  # generate data
  if("variables" %in% plot) {
  compl_vars <- variable_completeness(data)
  } else {}
  if("rows" %in% plot) {
  compl_obs <- row_completeness(data, !! id_var)
  } else {}

  # prepare data for plotting
  if ("variables" %in% plot & "rows" %in% plot) {
    plot_input <-
      dplyr::bind_rows("Variable" = compl_vars,
                       "Row" = compl_obs,
                       .id = "source") %>%
      dplyr::mutate(source = forcats::fct_relevel(source, "Row", "Variable"))
    fill <- c("Variable" = "#8DA0CB", "Row" = "66C2A5")
  } else if ("variables" == plot) {
    plot_input <- compl_vars %>%
      dplyr::mutate(source = "Variable")
    fill <- c("Variable" = "#8DA0CB")
  } else if ("rows" == plot) {
    plot_input <- compl_obs %>%
      dplyr::mutate(source = "Row")
    fill <- c("Row" = "66C2A5")
  } else {
    stop("`plot` must be a character string containing one or both of 'variables' and 'rows'.")
  }

  # plot
  plot_input %>%
    dplyr::mutate(Completeness = round(.data$Completeness,0)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(.data$Completeness, fill = .data$source),
                      alpha = 0.5, width = 1, position = ggplot2::position_identity()) +
    ggplot2::scale_fill_manual(values = fill) +
    ggplot2::theme_light() +
    ggplot2::theme(legend.justification = "top") + # legend top-right
    ggplot2::xlab("Completeness (%)") + # x axis label
    ggplot2::ylab("Count") + # y axis label
    ggplot2::labs(fill = "") +
    ggplot2::scale_x_continuous(breaks=seq(0,100,10), limits = c(-1,101))

}

#' Find highly distant value for data frame
#'
#' Returns a numeric value which is distant from the values in \code{data} using
#' the following equation: \deqn{output = -2 * (max(data)-min(data)))}
#'
#' @param data data frame.
#' @return Numeric vector of length 1
#'
#' @examples
#' data(mtcars)
#' eHDPrep:::distant_neg_val(mtcars)
distant_neg_val <- function(data) {
  if (all(apply(data, 2, is.numeric)) == FALSE) {
    stop("All variables must be numeric", call. = FALSE)
  } else {}
  
  min <- min(apply(data,2,min), na.rm = T)
  max <- max(apply(data,2,max), na.rm = T)
  
  # highly distant negative value
  -2* (max-min)
}

#' Completeness Heatmap
#'
#' Produces a heatmap visualising completeness across a dataset.
#'
#' \itemize{ \item Method 1: Missing values are numerically encoded with a
#' highly negative number, numerically distant from all values in \code{data},
#' using \code{\link[eHDPrep]{distant_neg_val}}. Values in categorical variables
#' are replaced with the number of unique values in the variable. Clustering
#' uses these values. Cells are coloured by presence (yellow = missing; blue =
#' present). \item Method 2: Same as Method 1 but cells are coloured by values
#' used to cluster. \item Method 3: Values in \code{data} are encoded as Boolean
#' values for clustering (present values = 1; missing values = 0). Cells are
#' coloured by presence (yellow = missing; blue = present). }
#'
#' @param data Data frame to be analysed.
#' @param annotation_tbl Data frame containing variable annotation data. Column
#'   1 should contain variable names, column 2 should contain an annotation
#'   label.
#' @param id_var Character constant of row identifier variable name.
#' @param method Integer between 1 and 3. Default: 1. See Details for more
#'   information.
#' @param show_rownames Boolean. Should rownames be shown. Default: False.
#' @param ... Parameters to be passed to \code{\link[pheatmap]{pheatmap}}.
#' @note If the heatmap overlaps with other plots on the current device, it 
#' is recommended that users run the \code{\link[grid]{grid.newpage}} 
#' function to ensure a clean page is used for this plot.
#' @importFrom tibble column_to_rownames
#' @importFrom dplyr mutate matches everything if_else n_distinct select across
#'   as_label all_of
#' @importFrom tidyr replace_na
#' @importFrom pheatmap pheatmap
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats dist hclust
#' @importFrom grid pushViewport viewport grid.draw rectGrob gpar
#' @return completeness heatmap
#' @export
#' @family measures of completeness
#' @seealso \code{\link[pheatmap]{pheatmap}}
#' @references Kolde R (2019). _pheatmap: Pretty Heatmaps_. R package version 1.0.12,
#' <https://CRAN.R-project.org/package=pheatmap>.
#' @examples
#' data(example_data)
#' completeness_heatmap(example_data,patient_id)
#' 
#' # with variable-level annotations
#' ## create a dataframe containing variable annotations
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
#' completeness_heatmap(example_data,patient_id, annotation_tbl = data_types)
completeness_heatmap <- function(data, id_var, annotation_tbl = NULL, method = 1, 
                                 show_rownames = FALSE, ...) {
  # fallback IDs, if not provided
  if(missing(id_var)) {
    data <- tibble::rownames_to_column(data)
    id_var <- dplyr::enquo(.data$rowname)
  } else{id_var <- dplyr::enquo(id_var)}
  
  # set value to be assigned to missing values in methods 1 and 2
  # id_var is omitted as it is not included in heatmap
  data %>%
    dplyr::select(- !! id_var) %>%
    dplyr::mutate(
    # encode non-numeric variables first
    dplyr::across(!where(is.numeric), dplyr::n_distinct)) %>%
    distant_neg_val() ->
    val_for_missings
  
  # fallback ordering for if variables have same pattern of missingness
  # this will order these variables by their assigned annotation

  if (!is.null(annotation_tbl)) {
    # convert first col to rownames, as required by pheatmap
    annotation_tbl <- column_to_rownames(annotation_tbl, names(annotation_tbl)[1])
    annot_ordering <- rownames(annotation_tbl)[rownames(annotation_tbl) %in% names(data)]
    annot_ordering <- annot_ordering[-which(annot_ordering == dplyr::as_label(id_var))]
  } else {
    annot_ordering <- names(data)[-which(names(data) == dplyr::as_label(id_var))]
    annotation_tbl <- NA
  }

  #grid::grid.newpage()
  #grid::grid.draw(grid::rectGrob(gp=grid::gpar(fill="white", lwd=0)))
  #setHook('grid.newpage', grid::grid.newpage())
  if (method == 1) {
    ### Non-Boolean Encoding. Boolean Encoding
    data %>%
      # convert id_var to rownames
      tibble::column_to_rownames(dplyr::as_label(id_var)) %>%
      dplyr::mutate(
        # encode non-numeric variables
        dplyr::across(!where(is.numeric),
               ~ dplyr::if_else(!is.na(.x),
                                as.double(dplyr::n_distinct(.x)),
                                as.double(val_for_missings))),
        # encode numeric variables
        dplyr::across(where(is.numeric),
               ~ tidyr::replace_na(.x, as.double(val_for_missings)))
      ) %>%
      # retain variables in annot_ordering
      dplyr::select(dplyr::all_of(annot_ordering)) %>%
      as.matrix() -> # convert to matrix
      mat

    # variables clust
    mat %>%
      t() %>% # transpose
      stats::dist("euclidean") %>% # calc distances
      stats::hclust("single") -> # calc clusters
      mat_var_clust

    # row clust
    mat %>%
      stats::dist("euclidean") %>% # calc distances
      stats::hclust("single") -> # calc clusters
      mat_row_clust

    # heatmap configuration
    data %>%
      # convert id_var to rownames
      tibble::column_to_rownames(dplyr::as_label(id_var)) %>%
      # encode values as present/missing
      dplyr::mutate(dplyr::across(dplyr::everything(), is.na)) %>%
      # encode numerically
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      # retain variables in annot_ordering
      dplyr::select(dplyr::all_of(annot_ordering)) %>%
      # convert to matrix
      as.matrix() %>%
      # generate heatmap
      pheatmap::pheatmap(color = c("#00204DCC", "#FFEA46CC"),
                         breaks = c(0,0.5,1),
                         legend_breaks = c(0,1),
                         legend_labels = c("Present", "Missing"),
                         border_color = "white",
                         annotation = annotation_tbl,
                         cluster_cols = mat_var_clust,
                         cluster_rows = mat_row_clust,
                         show_rownames = show_rownames,
                         silent = TRUE,
                         ...
      )

  } else if (method == 2) {
    ### Non-Boolean Encoding. Original value colouring.
    data %>%
      # convert id_var to rownames
      tibble::column_to_rownames(dplyr::as_label(id_var)) %>%
      dplyr::mutate(
        # encode non-numeric variables
        dplyr::across(!where(is.numeric),
                      ~ dplyr::if_else(!is.na(.x),
                                       as.double(dplyr::n_distinct(.x)),
                                       as.double(val_for_missings))),
        # encode numeric variables
        dplyr::across(where(is.numeric),
                      ~ tidyr::replace_na(.x, as.double(val_for_missings)))
      ) %>%
      # retain variables in annot_ordering
      dplyr::select(dplyr::all_of(annot_ordering)) %>%
      as.matrix() -> # convert to matrix
      mat

    # variables clust
    mat %>%
      t() %>% # transpose
      stats::dist("euclidean") %>% # calc distances
      stats::hclust("single") -> # calc clusters
      mat_var_clust

    # row clust
    mat %>%
      stats::dist("euclidean") %>% # calc distances
      stats::hclust("single") -> # calc clusters
      mat_row_clust

    # heatmap configuration
    mat %>%
      # generate heatmap
      pheatmap::pheatmap(
                         border_color = "white",
                         annotation = annotation_tbl,
                         cluster_cols = mat_var_clust,
                         cluster_rows = mat_row_clust,
                         show_rownames = show_rownames,
                         silent = TRUE,
                         ...
     )
    
  } else if (method == 3) {
    # #### Boolean Encoding
    data %>%
      tibble::column_to_rownames(dplyr::as_label(id_var)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), is.na)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      dplyr::select(dplyr::all_of(annot_ordering)) %>%
      as.matrix() -> # convert to matrix
      mat

    # variables clust
    mat %>%
      t() %>% # transpose
      stats::dist("binary") %>%
      stats::hclust("single") ->
      mat_var_clust

    # row clust
    mat %>%
      stats::dist("binary") %>%
      stats::hclust("single") ->
      mat_row_clust

    mat %>%
      # heatmap configuration
      pheatmap::pheatmap(color = c("#00204DCC", "#FFEA46CC"),
        breaks = c(0,0.5,1),
        legend_breaks = c(0,1),
        legend_labels = c("Present", "Missing"),
        border_color = "white",
        annotation = annotation_tbl,
        cluster_cols = mat_var_clust,
        cluster_rows = mat_row_clust,
        show_rownames = show_rownames,
        silent = TRUE,
        ...
        )

  }
  else {stop("method must be an integer of 1, 2, or 3")}
}


#' Compare Completeness between Datasets
#'
#' Produces a density plot comparing the completeness of two datasets
#' (\code{tbl_a} and \code{tbl_b}) for variables (if \code{dim} == 2, default)
#' or row (if \code{dim} == 1). The label used to identify the dataset's density
#' curve can be specified using \code{tbl_a_lab} and \code{tbl_b_lab}.
#'
#' @param tbl_a Data frame of the first data frame to compare.
#' @param tbl_b Data frame of the second data frame to compare.
#' @param dim Integer. Dimension to measure completeness on. 2 (Default)
#'   measures completeness by variable. 1 measures completeness by row.
#' @param tbl_a_lab String to be used to label \code{tbl_a} on the output plot.
#' @param tbl_b_lab String to be used to label \code{tbl_b} on the output plot.
#' @return Plot showing densities of completeness across both datasets.
#' @family measures of completeness
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' @importFrom rlang as_name .data
#' @importFrom dplyr quo mutate if_else
#' @importFrom ggplot2 ggplot geom_density theme theme_light labs
#'   scale_fill_manual scale_x_continuous xlab ylab
#' @export
#' @examples
#' data(example_data)
#' compare_completeness(example_data, strings_to_NA(example_data), dim = 2,
#'                      "raw", "cleaned")
#' 
compare_completeness <- function(tbl_a, tbl_b, dim = 2, tbl_a_lab = NULL, tbl_b_lab = NULL) {

  # add rowname column for row_completeness()
  tbl_a <- tibble::rownames_to_column(tbl_a)
  tbl_b <- tibble::rownames_to_column(tbl_b)

  # compute completeness
  if(missing(dim)) {
    stop("`dim` is missing and must be supplied. It must be either 1 (rows) or 2 (cols)")
  }
  else if (dim == 1) {
    tbl_a_compl <- row_completeness(tbl_a, "rowname")
    tbl_b_compl <- row_completeness(tbl_b, "rowname")
    x_label <- "Row Completeness (%)"
  }
  else if (dim == 2) {
    tbl_a_compl <- variable_completeness(tbl_a)
    tbl_b_compl <- variable_completeness(tbl_b)
    x_label <- "Variable Completeness (%)"
  }
  else {stop("`dim` must be an integer of 1 or 2", call. = FALSE)}

  # prepare labels
  if(is.null(tbl_a_lab)) {
    tbl_a_lab <- rlang::as_name(dplyr::quo(tbl_a))
  } else{}
  if(is.null(tbl_b_lab)) {
    tbl_b_lab <- rlang::as_name(dplyr::quo(tbl_b))
  } else{}
  

  # bind both tbls, specify labels, and create plot
  dplyr::bind_rows(tbl_a_compl, tbl_b_compl, .id = "version") %>%
    dplyr::mutate(version = dplyr::if_else(version == 1, tbl_a_lab, tbl_b_lab)) %>%
    ggplot2::ggplot() +
      # density geom
      ggplot2::geom_density(aes(.data$Completeness, fill = .data$version), alpha = 0.3) +
      ggplot2::theme_light() + # set theme
      # adjust legend
      ggplot2::theme(legend.position = "right", legend.justification = "top") +
      ggplot2::labs(fill = "") +
      ggplot2::scale_fill_manual(values=c("#af8dc3","#7fbf7b")) +
      ggplot2::scale_x_continuous(breaks=seq(0,100,10), limits = c(-1,101)) +
      ggplot2::xlab(x_label) + # x axis label
      ggplot2::ylab("Density") # y axis label

}
