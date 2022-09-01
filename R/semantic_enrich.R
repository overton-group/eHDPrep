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

#' Validate ontology network for semantic enrichment
#' 
#' Performs tests on a graph object in preparation for semantic enrichment.
#' 
#' The tests are:
#' \enumerate{
#'    \item Is graph coercible to \code{\link{tidygraph}} format?
#'    \item Is graph directed?
#'    \item Does graph contains one component (is one ontology)?
#' }
#' 
#' @param graph graph object to validate.
#'
#' @return input graph or validation errors
#' @examples 
#' data(example_ontology)
#' eHDPrep:::validate_ontol_nw(example_ontology)
#' 
validate_ontol_nw <- function(graph) {
  if(!tidygraph::with_graph(tidygraph::as_tbl_graph(graph),
                           tidygraph::graph_is_directed())) {
    stop("`graph` must be directed graph.", call. = FALSE)
  } else if (!tidygraph::with_graph(tidygraph::as_tbl_graph(graph),
                                     tidygraph::graph_component_count()) == 1) {
    stop("`graph` must have one component.", call. = FALSE)
  }
  
  tidygraph::as_tbl_graph(graph)
}

#' Validate mapping table for semantic enrichment
#'
#' Applies tests to a mapping table to ensure it is valid for use with
#' the data frame and ontological graph, in preparation for semantic enrichment.
#'
#' @param mapping_tbl data frame. Contains two columns. First column contains
#'   variable names of a primary dataset. Second column contains entities in
#'   an ontological graph to which the primary dataset's variable names are mapped.
#' @param data data frame. Primary dataset which contains variable names
#'   referred to in first column of the mapping table
#' @param ontol_graph ontological graph which contains entity names/IDs referred
#'   to in second column of the mapping table
#' @importFrom dplyr mutate filter pull row_number
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @return Any warnings and the mapping table returned invisibly
#' @examples 
#' data(example_mapping_file)
#' data(example_data)
#' data(example_ontology)
#' 
#' # warns that some variables don't correspond between the data and the mapping file:
#' eHDPrep:::validate_mapping_tbl(example_mapping_file, example_data, example_ontology)
validate_mapping_tbl <- function(mapping_tbl, data, ontol_graph) {
  mapping_tbl %>%
    dplyr::mutate(matching_vals = .[[1]] == .[[2]],
                  row = dplyr::row_number()) %>%
    dplyr::filter(.data$matching_vals) %>%
    dplyr::pull(.data$row)->
    matching_rows
  
  # if not two cols
  if(ncol(mapping_tbl) != 2) {
    stop("`mapping_tbl` must have two columns.\n
         \u2716 You have supplied one with ",ncol(mapping_tbl)," columns.", call. = FALSE)
    # if two cols aren't equal
  } else if (length(matching_rows) > 0){
    stop("Dataset variables must not equal their mapped ontological entities.
         \u2716 Rows ", paste0(matching_rows, collapse = ", "), " have matching values in both columns.",
         call. = FALSE)
    
  } 
  # optionally: if any col1 not in data
  else if(!missing(data)) {
    mapping_tbl %>%
      dplyr::filter(!.[[1]] %in% names(data)) %>%
      dplyr::pull(1)->
      missing_vars
    
    if(length(missing_vars) > 0) {
      warning("\u2716 The following variable names in column 1 of `mapping_tbl` are not column names in `data`:\n",
              paste0(missing_vars, collapse = ", "), call. = FALSE)
    } else{}
    
  }
  # optionally: if any col2 not in onto
  else if(!missing(ontol_graph)) {
    validate_ontol_nw(ontol_graph)
    ontol_graph %>% 
      tidygraph::as_tbl_graph() %>%
      tidygraph::pull(1) ->
      onto_ids
    
    mapping_tbl %>%
      dplyr::filter(!.[[2]] %in% onto_ids) %>%
      dplyr::pull(2) ->
      missing_maps
    
    if(length(missing_maps) > 0) {
      warning("\u2716 The following values in column 2 of `mapping_tbl` are not in `ontol_graph`:\n",
              paste0(missing_maps, collapse = ", "), call. = FALSE)
    } else{}
    
  } else{}
  invisible(mapping_tbl)
  
  
}

#' Calculate Node Information Content
#'
#' Computes the information content for each node in a directed graph using the
#' method developed by  by Lord \emph{et al.}.
#'
#' @param graph \code{tidygraph} directed graph.
#' @param mode Character constant specifying the directionality of the edges.
#' @note For use in semantic enrichment, this should be applied before joining
#'   an ontology with variable nodes.
#' @importFrom tidygraph with_graph as_tbl_graph graph_is_directed map_local_dbl
#'   mutate graph_order
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @return tidygraph with additional node attribute "information_content"
#' @noRd
#' @examples
#' data(example_ontology)
#' node_info_content(example_ontology)
#' @references Lord, P. W., Stevens, R. D., Brass, A. & Goble, C. A.
#'   Investigating semantic similarity measures across the Gene Ontology: the
#'   relationship between sequence and annotation. Bioinformatics 19, 1275–1283
#'   (2003).


node_info_content <- function(graph, mode = "in") {


  graph %>%
    validate_ontol_nw() %>%
    # Count number of descendants and self
    tidygraph::mutate(tmp_num_desc = tidygraph::map_local_dbl(tidygraph::graph_order(),
                                                     mode = mode,
                                                     mindist  = 0,
                                                     .f = function(graph, neighborhood, ...) {
                                                       nrow(tibble::as_tibble(neighborhood, active ='nodes'))
                                                     })) %>%
    # Calculate IC
    # graph_order = number of nodes in the graph
    tidygraph::mutate(information_content = -log2(.data$tmp_num_desc/tidygraph::graph_order())) %>%
    # remove temporary variable
    tidygraph::select(-.data$tmp_num_desc)
}

#' Calculate Node Information Content (Zhou et al 2008 method)
#'
#' Computes the information content for each node in a directed graph according
#' to the equation developed by Zhou \emph{et al.} (2008).
#'
#' @param graph \code{tidygraph} directed graph.
#' @param mode Character constant specifying the directionality of the edges.
#'   One of "in" or "out".
#' @param root name of root node identifier in column 1 to calculate node depth
#'   from.
#' @param k numeric value to adjust the weight of the two items of information
#'   content equation (relative number of hyponyms/descendants and relative node
#'   depth). Default = 0.5
#' @note For use in semantic enrichment, this should be applied before joining
#'   an ontology with nodes representing data variables (i.e. before applying
#'   \code{\link{join_vars_to_ontol}}.
#' @importFrom tidygraph with_graph as_tbl_graph graph_is_directed map_local_dbl
#'   mutate graph_order pull
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return tidygraph with additional node attribute "information_content"
#' @export
#' @references Zhou, Z., Wang, Y. & Gu, J. A New Model of Information Content
#'   for Semantic Similarity in WordNet. in 2008 Second International Conference
#'   on Future Generation Communication and Networking Symposia vol. 3 85–89
#'   (2008).

#' @examples 
#' data(example_ontology)
#' node_IC_zhou(example_ontology, mode = "in", root = "root")
node_IC_zhou <- function(graph, mode = "in", root, k = 0.5) {

  if ("igraph" %in% class(graph)) {
    graph <- tidygraph::as_tbl_graph(graph)
  } else{}
  
  graph %>%
    tidygraph::mutate( # calc the number of descendents (hyponyms) of concept c
      .hypo_c = tidygraph::map_local_dbl(tidygraph::graph_order(),
                                         mode = mode,
                                         mindist  = 1,
                                         .f = function(graph, neighborhood, ...) {
                                           nrow(tibble::as_tibble(neighborhood, active ='nodes'))
                                         }),
      # calc depth of concept c
      .depth_c = tidygraph::dfs_dist(which(tidygraph::pull(graph, 1) == root), mode),
      # max depth in graph
      .depth_max = max(.data$.depth_c, na.rm = T),
      # Calculate IC
      # graph_order = number of nodes in the graph
      information_content = 
        k*(1-log2(.data$.hypo_c+1) /
             log2(tidygraph::graph_order())) +
        (1-k) * (log2(.data$.depth_c)) / log2(.data$.depth_max),
      information_content = ifelse(is.infinite(.data$information_content),
                                   0, .data$information_content)
      ) %>%
    # remove temporary variables
    tidygraph::select(-c(.data$.hypo_c, .data$.depth_c, .data$.depth_max))
}


#' Join Mapping Table to Ontology Network Graph
#'
#' This function creates new nodes representing dataset variables and joins them
#' to an input ontology network using a mapping file. Prior to joining, the
#' information content of all nodes is calculated using \code{\link{node_IC_zhou}}.
#' 
#' \itemize{ \item The user-defined mappings between variables in a dataset and
#' entities/terms in an ontology are provided in an edge table
#' (\code{var2entity_tbl}). \item A node attribute column, \code{node_category} is
#' generated to describe if a node is one of "Dataset Variable", "Annotation", or
#' "Annotation Ancestor".
#' }
#' @param ontol_graph Graph containing the chosen ontology. Must be in
#'   \code{tidygraph} format or coercible to this format.
#' @param var2entity_tbl Edge table containing dataset variable names in first
#'   column and entities in ontologies to which they are mapped in the second
#'   column.
#' @inheritParams node_IC_zhou
#' @family semantic enrichment functions
#' @seealso node_IC_zhou
#' @importFrom tidygraph as_tbl_graph graph_join mutate
#' @importFrom dplyr if_else
#' @importFrom forcats as_factor fct_relevel
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A \code{\link{tidygraph}} resulting from the joining of \code{var2entity_tbl}
#'   and \code{ontol_graph}.
#' @export
#' @examples data(example_ontology)
#' join_vars_to_ontol(example_ontology, example_mapping_file, root = "root", mode = "in")
join_vars_to_ontol <- function(ontol_graph, var2entity_tbl, mode = "in", root, k = 0.5) {
  
  var2entity_tbl %>%
    tidygraph::as_tbl_graph() ->
    v2e_tbl_g

  ontol_graph %>%
    node_IC_zhou(root = root, mode = mode, k = k) %>%
    tidygraph::graph_join(v2e_tbl_g, by = "name") %>%
    tidygraph::mutate(node_category = dplyr::if_else(.data$name %in% dplyr::pull(var2entity_tbl,2),
                              "Annotation",
                              dplyr::if_else(.data$name %in% dplyr::pull(var2entity_tbl,1),
                                      "Dataset Variable",
                                      "Annotation Ancestor"))) %>%
    tidygraph::mutate(node_category = forcats::as_factor(.data$node_category)) %>%
    tidygraph::mutate(node_category = forcats::fct_relevel(.data$node_category,
                                                c("Dataset Variable",
                                                  "Annotation",
                                                  "Annotation Ancestor")
                                                )
    )
}


#' Compute Metavariable Information
#'
#' Calculates attributes for each node in a graph object pertaining to their
#' suitability and rank as metavariables; primarily if they are the most
#' informative common ancestor (see \code{\link{node_IC_zhou}}) of a set of
#' nodes representing a dataset variable.
#'
#' The added attributes are:
#' \describe{\item{min_dist_to_var}{Integer. The minimum distance of an ontology
#' node in the graph to a node representing a dataset variable.}
#' \item{is_metavariable}{Logical. If the node has at least two descendants in
#' the graph which represent dataset variables.}
#' \item{variable_descendants}{List. The names of variables of which a node is
#' an ancestor.}
#' \item{variable_set}{Integer. An identifier for the unique set of descendants
#' in the graph which represent dataset variables. The assigned
#' number corresponds to the order in which a unique set was identified when
#' scanning through the node table.}
#' \item{highest_IC}{Logical. If the node possesses the highest information
#' content of all other nodes which are common ancestors of the same variable
#' set. Information content is expected to have been calculated in
#' \code{\link{join_vars_to_ontol}}.}
#' }
#' @seealso \code{\link{node_IC_zhou}}
#' @param graph Graph containing ontological and dataset nodes. Must be in
#' \code{\link{tidygraph}} format or coercible to this format.
#' @param mode Character constant specifying the directionality of the edges.
#'   One of: "in" or "out".
#' @family semantic enrichment functions
#' @importFrom tidygraph mutate map_local_int as_tibble filter graph_order
#'   select pull map_local_lgl map_local arrange group_by ungroup
#' @importFrom igraph distances
#' @importFrom dplyr cur_data cur_group_id
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A modified graph object with additional node attributes pertaining to their
#'   status as a metavariable.
#' @export
#'
#' @examples
#' data(example_ontology)
#' require(magrittr)
#' example_ontology %>%
#' join_vars_to_ontol(example_mapping_file, root = "root") -> joined_ontol
#' 
#' metavariable_info(joined_ontol)
metavariable_info <- function(graph, mode = "in") {
  message("Identifying semantic commonalities through metavariables...")
  start_time <- Sys.time()
  graph %>%
    # Minimum distance of each node to a variable node
    tidygraph::mutate(min_dist_to_var = tidygraph::map_local_int(tidygraph::graph_order(),
                                                                 mode = mode,
                                                                 mindist = 0,
                                                                 .f = function(neighborhood, ...) {

                                             tidygraph::as_tibble(neighborhood) %>%
                                               tidygraph::filter(.data$.central_node) %>%
                                               tidygraph::pull(.data$name) ->
                                               source_node

                                             tidygraph::as_tibble(neighborhood) %>%
                                               tidygraph::select(.data$name, .data$node_category) %>%
                                               tidygraph::filter(.data$node_category == "Dataset Variable") %>%
                                               tidygraph::pull(.data$name) ->
                                               target_nodes

                                             if(length(target_nodes) > 0) {
                                               igraph::distances(neighborhood,
                                                                 v = source_node,
                                                                 to = target_nodes,
                                                                 mode = mode) %>%
                                                 apply(1,min) %>% # min taken
                                                 as.integer()
                                             } else {NULL}

                                           })) %>%
    # remove nodes which don't connect to variables
    tidygraph::filter(!is.na(.data$min_dist_to_var)) %>%

    # is node a metavariable? (>1 descendants which are dataset variables)
    tidygraph::mutate(is_metavariable = tidygraph::map_local_lgl(tidygraph::graph_order(),
                                                                 mode = mode,
                                                                 mindist = 1,
                                                                 .f = function(neighborhood, ...) {
                                             tidygraph::as_tibble(neighborhood,active ='nodes') %>%
                                               tidygraph::filter(.data$node_category == "Dataset Variable") %>%
                                               nrow() > 1
                                           })) %>%

    # list of variables which are a descendant of each node
    tidygraph::mutate(variable_descendants = tidygraph::map_local(tidygraph::graph_order(), mode = mode, mindist = 1,
                                            .f = function(neighborhood, ...) {
                                              tidygraph::as_tibble(neighborhood,active ='nodes') %>%
                                                tidygraph::filter(.data$node_category == "Dataset Variable") %>%
                                                tidygraph::select(.data$name) %>%
                                                tidygraph::arrange(.data$name)
                                            })) %>%

    # Sets of variable descendant (1 set = unique group of variable descendants)
    # note: this also includes non metavariables
    tidygraph::group_by(.data$variable_descendants) %>%
    tidygraph::mutate(variable_set = dplyr::cur_group_id()) %>%
    tidygraph::mutate(highest_IC = 
                        .data$information_content == max(dplyr::cur_data()$information_content)) %>%
    tidygraph::ungroup() ->
    res

  nrow(filter(res, .data$highest_IC & .data$is_metavariable))
  message("Complete. Duration: ", as.character(round(as.numeric(Sys.time()-start_time),2)), " secs.\n",
          nrow(as_tibble(filter(res, .data$highest_IC & .data$is_metavariable))),
          " semantic commonalities found (via most informative common ancestors).")

  res
}

#' Aggregate Data by Metavariable
#'
#' Variables in a numeric data frame are aggregated into metavariables via
#' their most informative common ancestors identified in an ontological graph
#' object (see \code{\link{metavariable_info}}). Metavariables are appended to
#' the data frame.
#' 
#' Metavariables are created from the aggregation of data variables via their
#' most informative common ancestor (expected to have been calculated in
#' \code{\link{metavariable_info}}). Metavariables are labelled using the
#' syntax: \code{MV_[label_attr]_[Aggregation function]}. The data variables are
#' aggregated row-wise by their maximum, minimum, mean, sum, and product.
#' Metavariables with zero entropy (no information) are not appended to the
#' data. See examples for where this function should be applied in the semantic
#' enrichment workflow.
#' @note A warning may be shown regarding the '.add' argument being deprecated, this is
#'   belived to be an issue with tidygraph which may be resolved in a future release: 
#'   <https://github.com/thomasp85/tidygraph/issues/131>. Another warning may be shown regarding the 'neimode' argument being deprecated, this is
#'   belived to be an issue with tidygraph which may be resolved in a future release: 
#'   <https://github.com/thomasp85/tidygraph/issues/156>. These warning messages are not believed to have
#'   an effect on the functionality of eHDPrep.
#'  
#' @param graph Graph containing ontological and dataset nodes. Must be in
#'   \code{\link{tidygraph}} format or coercible to this format. Must have been
#'   processed using \code{\link{metavariable_info}}.
#' @param data Numeric data frame or matrix containing variables which are also
#'   in \code{graph}.
#' @param label_attr Node attribute containing labels used for column names when
#'   creating metavariable aggregations. Default: "name"
#' @param normalize_vals Should values be normalized before aggregation?
#'   Default: TRUE
#' @family semantic enrichment functions
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr slice select pull as_tibble rowwise transmute ungroup
#' @importFrom tidygraph filter
#' @return \code{data} with semantic aggregations derived from common
#'   ontological ancestry (metavariables) appended as new columns, each
#'   prefixed with "MV_" and suffixed by their aggregation function (e.g. "_SUM").
#' @export
#' @examples
#' require(magrittr)
#' require(dplyr)
#' data(example_ontology)
#' data(example_mapping_file)
#' data(example_data)
#' 
#' #' # define datatypes
#' tibble::tribble(~"var", ~"datatype",
#' "patient_id", "id",
#' "tumoursize", "numeric",
#' "t_stage", "ordinal_tstage",
#' "n_stage", "ordinal_nstage",
#' "diabetes_merged", "character",
#' "hypertension", "factor",
#' "rural_urban", "factor",
#' "marital_status", "factor",
#' "SNP_a", "genotype",
#' "SNP_b", "genotype",
#' "free_text", "freetext") -> data_types
#' 
#' # create post-QC data
#' example_data %>%
#'   merge_cols(diabetes_type, diabetes, "diabetes_merged", rm_in_vars = TRUE) %>%
#'   apply_quality_ctrl(patient_id, data_types,
#'                      bin_cats =c("No" = "Yes", "rural" = "urban"),
#'                      to_numeric_matrix = TRUE) %>%
#'                      suppressMessages() ->
#'                      post_qc_data
#' 
#' # minimal example on first four coloums of example data:
#' dplyr::slice(example_ontology, 1:7,24) %>%
#'    join_vars_to_ontol(example_mapping_file[1:3,], root = "root") %>%
#'    metavariable_info() %>%
#'    metavariable_agg(post_qc_data[1:10,1:4]) -> res
#' # see Note section of documentation for information on possible warnings.
#'
#' # summary of result:
#' tibble::glimpse(res)
#'
#' \dontrun{
#' # full example:
#' example_ontology %>%
#'    join_vars_to_ontol(example_mapping_file, root = "root") %>%
#'    metavariable_info() %>%
#'    metavariable_agg(post_qc_data) -> res
#'  # see Note section of documentation for information on possible warnings.
#'
#' # summary of result:
#' tibble::glimpse(res)
#' }
metavariable_agg <- function(graph, data, label_attr ="name", normalize_vals = TRUE) {
  start_time <- Sys.time()
  start_ncol <- ncol(data)
  
  message("Aggregating variables by semantic commonalities and appending to `data`...")
  
  # filter graph to only Most Informative Common Ancestors (MICAs)
  graph %>%
    tidygraph::filter(.data$highest_IC & .data$is_metavariable) %>%
    dplyr::as_tibble() ->
    graph_MICAs
  
  # initialise vector for # vars omitted due to zero entropy
  zev <- 0
  
  for(i in 1:nrow(graph_MICAs)) {
    
    # slice graph to include ith MICA
    dplyr::slice(graph_MICAs,i) %>%
      dplyr::select(.data$variable_descendants, label_attr) ->
      slice_i
    
    # get string of variables to aggregate
    slice_i %>%
      dplyr::pull(.data$variable_descendants) %>%
      unlist() %>%
      unique() ->
      cols2aggregate
    
    # create data frame of cols2aggregate from data
    cols2agg <- dplyr::select(data, dplyr::all_of(cols2aggregate))
    
    # TODO: check for all NAs in row. Then aggregate safely
    # these values can be normalised to be between 0 and 1
    if(normalize_vals) {cols2agg <- purrr::map_dfr(cols2agg, normalize)} else {}

    # create prefix
    prefix <- paste0("MV_", gsub('[[:punct:] ]+','_', dplyr::pull(slice_i, label_attr) ))
    
    # aggregate
    cols2agg %>%
      dplyr::rowwise() %>%
      dplyr::transmute("{prefix}_SUM" := sum_catchNAs(dplyr::c_across(everything())),
                       "{prefix}_AVG" := mean_catchNAs(dplyr::c_across(everything())),
                       "{prefix}_MAX" := max_catchNAs(dplyr::c_across(everything())),
                       "{prefix}_MIN" := min_catchNAs(dplyr::c_across(everything())),
                       "{prefix}_MUL" := prod_catchNAs(dplyr::c_across(everything()))
                       ) %>%
      dplyr::ungroup() -> 
      aggs
    
    aggs_ncol <- ncol(aggs)
    # omit aggregations with zero entropy
    aggs <- dplyr::select(aggs, where(~ entropy(.x) != 0)) ->
      aggs
    aggs_ncol2 <- ncol(aggs)
    
    
    zev <- zev + (aggs_ncol - aggs_ncol2)
    # add to dataset
    data <- cbind(data,aggs)
  }
  
  message("Complete. Duration: ", as.character(round(as.numeric(Sys.time()-start_time),2)), " secs.\n",
          "The dataset has been enriched with ", ncol(data) - start_ncol, " new variables\n(",
          zev, " new variables were not appended as they had zero entropy).")
  
  return(data)
}

#' Min max normalization
#'
#' Normalizes values in \code{x} to be between 0 and 1 using min-max
#' normalization.
#' 
#' @param x numeric vector
#' @param na.rm a logical indicating whether missing values should be removed. Default = TRUE.
#' @return normalised \code{x}
normalize <- function(x, na.rm = TRUE) {
  stopifnot(is.numeric(x))
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}

#' Sum vector safely for semantic enrichment
#'
#' sums values in x (ignoring NAs). If all values in x are \code{NA}, returns
#' \code{NA} (rather than 0),
#' 
#' @param x numeric vector
#' @return sum of \code{x}
#' @examples
#' x <- c(1,2,3)
#' eHDPrep:::sum_catchNAs(x)

sum_catchNAs <- function(x) {
  if(all(is.na(x))) {
    return(as.numeric(NA))
  } else{sum(x, na.rm = TRUE)}
}

#' Find mean of vector safely
#'
#' This low-level function is deployed as part of the semantic enrichment
#' process. Averages values in numeric vector (ignoring NAs). If all values in
#' numeric vector are \code{NA}, returns \code{NA} (rather than NaN),
#' 
#' @param x numeric vector
#' @return mean of \code{x}
#' @examples
#' x <- c(1,2,3)
#' eHDPrep:::mean_catchNAs(x)
#' 
#' # with NA present:
#' x <- c(1,2,3, NA)
#' eHDPrep:::mean_catchNAs(x)
mean_catchNAs <- function(x) {
  if(all(is.na(x))) {
    return(as.numeric(NA))
  } else{mean(x, na.rm = TRUE)}
}

#' Find maximum of vector safely
#'
#' This low-level function is deployed as part of the semantic enrichment
#' process.Calculates maximum of values in numeric vector (ignoring NAs). If all
#' values in input vector are \code{NA}, returns \code{NA} (rather than -Inf),
#'
#' @param x numeric vector
#' @return maximum value of \code{x}
#' @examples
#' x <- c(1,2,3)
#' eHDPrep:::max_catchNAs(x)
max_catchNAs <- function(x) {
  if(all(is.na(x))) {
    return(as.numeric(NA))
  } else{max(x, na.rm = TRUE)}
}

#' Find minimum of vector safely
#'
#' This low-level function is deployed as part of the semantic enrichment
#' process. Calculates minimum of values in numeric vector (ignoring NAs). If
#' all values in numeric vector are \code{NA}, returns \code{NA} (rather than
#' Inf),
#'
#' @param x numeric vector
#' @return minimum value of \code{x}
#' @examples
#' x <- c(1,2,3)
#' eHDPrep:::min_catchNAs(x)
min_catchNAs <- function(x) {
  if(all(is.na(x))) {
    return(as.numeric(NA))
  } else{min(x, na.rm = TRUE)}
}

#' Find product of vector safely
#'
#' This low-level function is deployed as part of the semantic enrichment
#' process. Calculates product of values in numeric vector (ignoring NAs). If
#' all values in numeric vector are \code{NA}, returns \code{NA} (rather than
#' Inf),
#' 
#' @param x numeric vector
#' @return product of \code{x}
#' @examples
#' x <- c(1,2,3)
#' eHDPrep:::prod_catchNAs(x)
prod_catchNAs <- function(x) {
  if(all(is.na(x))) {
    return(as.numeric(NA))
  } else{prod(x, na.rm = TRUE)}
}

#' import ggraph (used in vignette)
#' @keywords internal
#' @importFrom 





