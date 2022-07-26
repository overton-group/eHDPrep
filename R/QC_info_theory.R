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
#    along with eHDPrep.  If not, see <http://www.gnu.org/licenses/>.

####
##### Entropy #####
####

##' Calculate Entropy of a Vector
##'
##' Calculates Shannon Entropy of a vector in bits (default) or natural units.
##' Missing values are omitted from the calculation.
##'
##' @param x Input vector
##' @param unit Unit to measure entropy. Either "bits" (default) or "nats".
##' @return Entropy of input variable
##' @references Shannon, C. E. A mathematical theory of communication. The Bell
##'   System Technical Journal 27, 379–423 (1948).
##' @export
##' @examples
##' # no entropy:
##' vec <- c(1,1,1,1,1,1)
##' entropy(vec)
##' 
##' # entropy
##' vec <- c(1,2,3,4,5,6)
##' entropy(vec)
entropy <- function(x, unit = c("bits")) {
  if(!is.vector(x)) {stop("`x` must be a vector.", call. = FALSE)} else{}

  # remove NAs from vector
  x <- x[!is.na(x)]

  # probabilities per category
  p <- as.numeric(table(x) / length(x))
  # remove categories with zero probability (would return NaN from log2)
  p <- p[p > 0]
  # calculate Shannon entropy
  if(unit == "bits") {
    -sum(p * log2(p), na.rm = T)
  } else if (unit == "nats") {
    -sum(p * log10(p), na.rm = T)
  } else {stop("Units not recognised: enter 'bits' or 'nats'", call. = FALSE)}

}

#' Calculate Entropy of Each Variable in Data Frame
#'
#' Calculates Shannon entropy of all variables in a data frame in bits (default) or
#' natural units. Missing values are omitted from the calculation.
#'
#' @param data Data Frame to compute on
#' @inheritParams entropy
#' @return Named numeric vector containing entropy values
#' @references Shannon, C. E. A mathematical theory of communication. The Bell
#'   System Technical Journal 27, 379–423 (1948).
#' @export
#' @examples
#' a <- matrix(c(c(1,1,1,1,1,1, 1,2,3,4,5,6)),ncol = 2, dimnames =
#' list(seq(1,6), c("no_entropy","entropy")))
#' variable_entropy(as.data.frame(a))
variable_entropy <- function(data, unit = "bits") {
  if(!is.data.frame(data)) {stop("`data` must be a data frame.", call. = FALSE)} else{}
  apply(data, 2, function(x) entropy(x, unit = unit))
}

#' Identify variables with zero entropy
#'
#' Calculates Shannon entropy of variables in a data frame in bits (default) or
#' natural units. Missing values are omitted from the calculation.
#' Names of variables with zero entropy are returned.
#'
#' @inheritParams variable_entropy
#' @return Character vector of variable names with zero entropy
#' @references Shannon, C. E. A mathematical theory of communication. The Bell
#'   System Technical Journal 27, 379–423 (1948).
#' @export
#' @examples
#' data(example_data)
#' zero_entropy_variables(example_data)
zero_entropy_variables <- function(data, unit = "bits") {
  if(!is.data.frame(data)) {stop("`data` must be a data frame.", call. = FALSE)} else{}
  en <- variable_entropy(data, unit = unit)
  names(en[which(en == 0)])
}

####
##### Information Content #####
####

##' Calculate Information Content (Discrete Variable)
##'
##' Calculates information content of a discrete (categorical or ordinal) vector
##' in bits (default) or natural units. Missing values are omitted from the
##' calculation.
##'
##' @param x Input vector
##' @param unit Unit to measure entropy. Either "bits" (default) or "nats".
##' @return Information content of input variable
##' @export
##' @examples
##' data(example_data)
##' information_content_discrete(example_data$marital_status)
information_content_discrete <- function(x, unit = c("bits")) {
  if(!is.vector(x)) {stop("`x` must be a vector.", call. = FALSE)} else{}
  
  # remove NAs from var
  x <- x[!is.na(x)]

  # probabilities per category
  p <- table(x)/length(x)
  # remove categories with zero probability (would return NaN from log2)
  p <- p[p>0]

  # calculate the information content for the input variable
  probs <- as.numeric(p[x])
  
  if(unit == "bits") {
    -sum(log2(probs), na.rm=T)
  } else if (unit == "nats") {
    -sum(log10(probs),na.rm = T)
  } else{stop("Units not recognised: enter 'bits' or 'nats'", call. = FALSE)}

}

##' Calculate Information Content (Continuous Variable)
##'
##' Calculates information content of a continuous (numeric) vector in bits
##' (default) or natural units. Missing values are omitted from the calculation.
##'
##' @param x Input vector
##' @param unit Unit to measure entropy. Either "bits" (default) of "nats".
##' @return Information content of input variable
##' @export
##' @examples
##' data(example_data)
##' information_content_contin(example_data$tumoursize)
information_content_contin <- function(x, unit = c("bits")) {
  if(!is.vector(x)) {stop("`x` must be a vector.", call. = FALSE)} else{}
  
  # variable.bw.kde is from rTMA
  probs <- variable.bw.kde(x, na.rm = T)$y

  # calculate the information content for the input variable
  if (unit == "bits") {
    -sum(log2(probs), na.rm = T)
  } else if (unit == "nats") {
    -sum(log10(probs), na.rm = T)
  } else{stop("Units not recognised: enter 'bits' or 'nats'", call. = FALSE)}

}

##' Calculate Mutual Information Content
##'
##' Calculates mutual information content between two variables in bits. Missing
##' values are omitted from the calculation.
##'
##' @param x First variable
##' @param y Second variable
##' @return Mutual information content of \code{x} and \code{y}
##' @export
##' @examples 
##' data(example_data)
##' mi_content_discrete(example_data$diabetes, example_data$diabetes_type)
mi_content_discrete <- function (x, y) {
  if(!is.vector(x)) {stop("`x` must be a vector.", call. = FALSE)} else{}
  if(!is.vector(y)) {stop("`y` must be a vector.", call. = FALSE)} else{}
  
  # identify the rows with data points in both x and y
  # (i.e. rows with NA values are excluded)
  na_map <- is.na(cbind(x, y))
  complete_rows <- which(((as.integer(na_map[,1]) + as.integer(na_map[,2]))==0))
  total_complete_rows <- length(complete_rows) # count the total
  # calculate mutual information (using function from rTMA)
  x_complete_rows <- x[complete_rows]
  y_complete_rows <- y[complete_rows]
  mi <- discrete.mi(as.matrix(t(cbind(x_complete_rows, y_complete_rows))))

  # calculate mutual information content by multiplying MI by the number of rows
  # where both variables do not have NA values
  mi_content <- total_complete_rows * mi
  as.numeric(mi_content[2])
}

##' Information Content Comparison Table
##'
##' Used to quantify the amount of information loss, if any, which has occurred
##' in a merging procedure between two discrete variables.
##' @details The function requires the two discrete variables which have been
##'   merged (\code{input1} and \code{input2}) and the composite variable
##'   (\code{output}). For each input, information content is calculated using
##'   \code{\link{information_content_discrete}} along with each input's mutual
##'   information content with the composite variable using
##'   \code{\link{mi_content_discrete}}. The function returns a table describing
##'   these measures.
##'   
##'   If the mutual information content between an input variable and the
##'   composite variable is equal to the information content of the input
##'   variable, it is confirmed that all information in the input variable has
##'   been incorporated into the composite variable. However, if one or both
##'   input variables' information content is not equal to their mutual
##'   information with the composite variables, information loss has occurred.
##'   
##' @param input1 Character vector. First variable to compare
##' @param input2 Character vector. Second variable to compare
##' @param composite Character vector. Composite variable, resultant of merging
##'   \code{input1} and \code{input2}.
##' @return Table containing information content for \code{input1} and
##'   \code{input2} and their mutual information content with \code{composite}.
##' @export
##' @seealso \code{\link{compare_info_content_plt}}
##' @examples 
##' data(example_data)
##' require(dplyr)
##' require(magrittr)
##' example_data %>%
##'    mutate(diabetes_merged = coalesce(diabetes_type, diabetes)) %>%
##'    select(starts_with("diabetes")) ->
##'    merged_data
##'    
##' compare_info_content(merged_data$diabetes,
##'                      merged_data$diabetes_type,
##'                      merged_data$diabetes_merged)
compare_info_content <- function(input1, input2, composite) {
  if(!is.vector(input1)) {stop("`input1` must be a vector.", call. = FALSE)} else{}
  if(!is.vector(input2)) {stop("`input2` must be a vector.", call. = FALSE)} else{}
  if(!is.vector(composite)) {stop("`composite` must be a vector.", call. = FALSE)} else{}
  
  i1 <- as.character(input1)
  i2 <- as.character(input2)
  o <- as.character(composite)

  # Information Content
  ic_in1 <- c(information_content_discrete(i1),
              deparse(substitute(input1)),
              "Information Content")
  ic_in2 <- c(information_content_discrete(i2),
              deparse(substitute(input2)),
              "Information Content")
  ic_out <- c(information_content_discrete(o),
              deparse(substitute(output)),
              "Information Content")

  # Mutual Information
  mi_in1_out <- c(mi_content_discrete(o, i1),
                  deparse(substitute(input1)),
                  paste0("Mutual Information Content with\n", deparse(substitute(output))))
  mi_in2_out <- c(mi_content_discrete(o, i2),
                  deparse(substitute(input2)),
                  paste0("Mutual Information Content with\n", deparse(substitute(output))))

  # Results table
  res <- rbind(ic_in1, ic_in2, ic_out, mi_in1_out, mi_in2_out)
  colnames(res) <- c("Information", "Variable", "Measure")
  res <- as_tibble(res)
  return(res)
}

##' Information Content Comparison Plot
##'
##' This function requires the output from \code{\link{compare_info_content}}.
##' It is used to visualise the amount of information loss, if any, which has
##' occurred in a merging procedure between two discrete variables.
##' 
##' @details If the mutual information content between an input variable and the
##'   composite variable is equal to the information content of the input
##'   variable, it is confirmed that all information in the input variable has
##'   been incorporated into the composite variable.
##'   
##' @param compare_info_content_res Output from
##'   \code{\link{compare_info_content}}.
##' @return Plot of measures calculated in \code{\link{compare_info_content}}.
##' @seealso \code{\link{compare_info_content}}
##' @importFrom magrittr %>%
##' @importFrom dplyr mutate
##' @importFrom ggplot2 ggplot geom_bar ylab xlab theme_light theme aes rel
##'   element_text
##' @importFrom stats reorder
##' @importFrom stringr str_wrap
##' @importFrom rlang .data
##' @export
##' @examples
##' data(example_data)
##' require(dplyr)
##' require(magrittr)
##' example_data %>%
##'    mutate(diabetes_merged = coalesce(diabetes_type, diabetes)) %>%
##'    select(starts_with("diabetes")) ->
##'    merged_data
##'
##' compare_info_content(merged_data$diabetes,
##'                      merged_data$diabetes_type,
##'                      merged_data$diabetes_merged) %>%
##'                      compare_info_content_plt()
compare_info_content_plt <- function(compare_info_content_res) {
  
  # truncate long labels
  compare_info_content_res %>%
    dplyr::mutate(Variable = stringr::str_wrap(.data$Variable, 30)) ->
    compare_info_content_res

  compare_info_content_res %>%
    ggplot2::ggplot(
      ggplot2::aes(
        fill = .data$Measure,
        y = as.numeric(.data$Information),
        x = stats::reorder(.data$Variable, as.numeric(.data$Information))
      )
  ) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::ylab("Information (Bits)") +
    ggplot2::xlab("Variable") +
    ggplot2::theme_light() +
    ggplot2::theme(aspect.ratio = 9/16,
                   legend.justification = "left",
                   legend.position = "top",
                   axis.text.x = ggplot2::element_text(size = ggplot2::rel(0.9)))
}
