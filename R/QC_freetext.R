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

#' Identify Neighbouring Words (Skipgrams) in a free-text vector
#'
#' Identifies words which appear near each other in the free-text variable
#' (\code{var}), referred to as "Skipgrams". Supported languages for stop words
#' and stemming are \code{danish}, \code{dutch}, \code{english}, \code{finnish},
#' \code{french}, \code{german}, \code{hungarian}, \code{italian},
#' \code{norwegian}, \code{portuguese}, \code{russian}, \code{spanish}, and
#' \code{swedish}.
#'
#' @param x Free-text character vector to query.
#' @param ids Character vector containing IDs for each element of \code{var}.
#' @param num_of_words Number of words to consider for each returned skipgram.
#'   Default = 2.
#' @param max_interrupt_words Maximum number of words which can interrupt
#'   proximal words. Default = 2.
#' @param words_to_rm Character vector of words which should not be considered.
#' @param lan Language of \code{var}. Default: \code{english}.
#' @importFrom quanteda corpus tokens tokens_tolower tokens_remove
#'   tokens_wordstem tokens_skipgrams
#' @importFrom tm stopwords
#' @importFrom dplyr tibble group_by_all count rename mutate filter arrange desc
#'   ungroup
#' @return Tibble containing skipgrams as variables and patient values as rows.
#' @family free text functions
#' @seealso Principle underlying function: \code{\link[quanteda]{tokens_ngrams}}
#' @references Guthrie, D., Allison, B., Liu, W., Guthrie, L. & Wilks, Y. A
#'   Closer Look at Skip-gram Modelling. in Proceedings of the Fifth
#'   International Conference on Language Resources and Evaluation (LREC’06)
#'   (European Language Resources Association (ELRA), 2006).
#'   
#'   Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018).
#'   “quanteda: An R package for the quantitative analysis of textual data.” _Journal
#'   of Open Source Software_, *3*(30), 774. doi:10.21105/joss.00774
#'   <https://doi.org/10.21105/joss.00774>, <https://quanteda.io>.
#'   
#'   Feinerer I, Hornik K (2020). _tm: Text Mining Package_. R package version 0.7-8,
#'   <https://CRAN.R-project.org/package=tm>.
#'   
#'   Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in
#'   R. Journal of Statistical Software 25(5): 1-54. URL:
#'   https://www.jstatsoft.org/v25/i05/.
#' @export
#' @examples
#' data(example_data)
#' skipgram_identify(x = example_data$free_text,
#'                   ids = example_data$patient_id,
#'                   max_interrupt_words = 5)
skipgram_identify <- function(x, ids, num_of_words = 2, max_interrupt_words = 2,
                          words_to_rm = NULL, lan = "english") {
    #prefix <- deparse(substitute(x))

    quanteda::corpus(x = x, docnames = ids) %>%
    # create tokens. Remove punctuation, numbers, and symbols
    quanteda::tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
    # make tokens lowercase
    quanteda::tokens_tolower() %>%
    # Remove stopwords
    quanteda::tokens_remove(pattern = c(tm::stopwords(lan),words_to_rm), padding = T) %>%
    # Stem words
    quanteda::tokens_wordstem(language = lan) %>% # used, e.g., for brothers -> brother
    # generate skipgrams
    quanteda::tokens_skipgrams(n = num_of_words, skip = 0:max_interrupt_words, concatenator = "_") %>%
    # convert to tbl
    quanteda::dfm() %>%
    quanteda::convert("data.frame") %>%
    dplyr::as_tibble() ->
    res
  
  if (ncol(res) <2) {
    stop("No skipgrams were found in ", deparse(substitute(x)), call. = FALSE)
  } else {return(res)}
  
}

#' Report Skipgram Frequency
#' 
#' Measures the frequency of skipgrams (non-contiguous words in free text), reported in a
#' tibble. Frequency is reported as both counts and percentages.
#'
#' @param skipgram_tokens Output of \code{\link{skipgram_identify}}.
#' @param min_freq Minimum skipgram percentage frequency of occurrence to
#'   retain. Default = 1.
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename group_by summarise mutate filter arrange desc
#' @importFrom rlang .data
#' @return  Data frame containing frequency of skipgrams in absolute count and
#'   relative to the length of input variable.
#' @family free text functions
#' @export
#' @seealso Principle underlying function: \code{\link[quanteda]{tokens_ngrams}}
#' @references Guthrie, D., Allison, B., Liu, W., Guthrie, L. & Wilks, Y. A
#'   Closer Look at Skip-gram Modelling. in Proceedings of the Fifth
#'   International Conference on Language Resources and Evaluation (LREC’06)
#'   (European Language Resources Association (ELRA), 2006).
#'   
#'   Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018).
#'   “quanteda: An R package for the quantitative analysis of textual data.” _Journal
#'   of Open Source Software_, *3*(30), 774. doi:10.21105/joss.00774
#'   <https://doi.org/10.21105/joss.00774>, <https://quanteda.io>.
#'   
#'   Feinerer I, Hornik K (2020). _tm: Text Mining Package_. R package version 0.7-8,
#'   <https://CRAN.R-project.org/package=tm>.
#'   
#'   Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in
#'   R. Journal of Statistical Software 25(5): 1-54. URL:
#'   https://www.jstatsoft.org/v25/i05/.
#' @examples
#' data(example_data)
#' toks_m <- skipgram_identify(x = example_data$free_text,
#'                             ids = example_data$patient_id,
#'                             max_interrupt_words = 5)
#' skipgram_freq(toks_m, min_freq = 0.5)
skipgram_freq <- function(skipgram_tokens, min_freq = 1) {

  skipgram_tokens %>%
    tidyr::pivot_longer(!1) %>%
    dplyr::rename(skipgram = "name") %>%
    dplyr::group_by(.data$skipgram) %>%
    dplyr::summarise(count = sum(.data$value)) %>%
    dplyr::arrange(dplyr::desc(.data$count)) %>%
    dplyr::mutate(percentage = (count/nrow(skipgram_tokens))*100) %>%
    dplyr::filter(.data$percentage >= min_freq)

}

#' Append Skipgram Presence Variables to Dataset
#'
#' Adds new variables to \code{data} which report the presence of skipgrams
#' (either those specified in \code{skipgrams2append} or, if not specified,
#' skipgrams with a minimum frequency (\code{min_freq}, default = 1)).
#'
#' @param skipgram_tokens Output of \code{\link{skipgram_identify}}.
#' @param skipgrams2append Which skipgrams in \code{skipgram_tokens} to append
#'   to dataset.
#' @param data Data frame to append skipgram variables to.
#' @param min_freq Minimum percentage frequency of skipgram occurrence to
#'   return. Default = 1.
#' @inheritParams encode_as_num_mat
#'
#' @importFrom quanteda dfm convert
#' @importFrom dplyr select mutate across left_join all_of
#' @importFrom rlang .data
#' @return \code{data} with additional variables describing presence of
#'   skipgrams
#' @family free text functions
#' @seealso Principle underlying function: \code{\link[quanteda]{tokens_ngrams}}
#' @references Guthrie, D., Allison, B., Liu, W., Guthrie, L. & Wilks, Y. A
#'   Closer Look at Skip-gram Modelling. in Proceedings of the Fifth
#'   International Conference on Language Resources and Evaluation (LREC’06)
#'   (European Language Resources Association (ELRA), 2006).
#'   
#'   Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018).
#'   “quanteda: An R package for the quantitative analysis of textual data.” _Journal
#'   of Open Source Software_, *3*(30), 774. doi:10.21105/joss.00774
#'   <https://doi.org/10.21105/joss.00774>, <https://quanteda.io>.
#'   
#'   Feinerer I, Hornik K (2020). _tm: Text Mining Package_. R package version 0.7-8,
#'   <https://CRAN.R-project.org/package=tm>.
#'   
#'   Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in
#'   R. Journal of Statistical Software 25(5): 1-54. URL:
#'   https://www.jstatsoft.org/v25/i05/.
#' @export
#'
#' @examples
#' data(example_data)
#' # identify skipgrams
#' toks_m <- skipgram_identify(x = example_data$free_text,
#'                             ids = example_data$patient_id,
#'                             max_interrupt_words = 5)
#' # add skipgrams by minimum frequency
#' skipgram_append(toks_m,
#'                 id_var = patient_id,
#'                 min_freq = 0.6,
#'                 data = example_data)
#' # add specific skipgrams
#' skipgram_append(toks_m,
#'                 id_var = patient_id,
#'                 skipgrams2append = c("sixteen_week", "bad_strain"),
#'                 data = example_data)
skipgram_append <- function(skipgram_tokens, skipgrams2append, data, id_var, min_freq = 1) {
  
  id_var <- dplyr::enquo(id_var)
  
  if(!(is.numeric(min_freq)) | min_freq < 0 | min_freq > 100) {
    stop("`min_freq` must be a numeric value between 0 and 100 (inclusive)", call. = FALSE)
  } else
  
  # if id_var in data is numeric, make skipgram_tokens id_var numeric. Needed for join
  if(dplyr::summarise(data, dplyr::across(!! id_var, is.numeric)) %>% dplyr::pull()) {
    skipgram_tokens %>%
      dplyr::mutate(dplyr::across(1, as.numeric)) -> 
      skipgram_tokens
  } else {}
  
  # if skipgrams2append is missing, find skipgrams which match the minimum freq and use those
  if(missing(skipgrams2append)) {
    message("`skipgrams2append` not provided. Searching for skipgrams with a `min_freq` of ",
            min_freq, "%")
  
    skipgram_freq(skipgram_tokens, min_freq = min_freq) %>%
      pull(.data$skipgram) %>%
      unname() ->
      skipgrams2append

  } else{}
  
  # subset skipgram tokens to those which need to be added
  skipgram_tokens %>%
    dplyr::select(1, !! dplyr::all_of(skipgrams2append)) ->
    sg_tbl

 # join selected columns to original data frame
  data %>%
   dplyr::left_join(y = sg_tbl, by = setNames("doc_id", dplyr::as_label(id_var))) ->
    data

  if(length(skipgrams2append) == 0) {
    message("No variables have been appended.\n
            No valid skipgrams were specified and no skipgrams met the frequency threshold (",min_freq,"%)")
  } else{message(length(skipgrams2append), " skipgrams have been appended the data.")}

  data
}

# needed for "Reduce" function
utils::globalVariables(".")

#' Extract information from free text
#'
#' Extracts information from specified free text variables (\code{...}) which
#' occur in a minimum amount of rows (\code{min_freq}) and appends new variables
#' to \code{data}.
#'
#' New variables report the presence of skipgrams (proximal words in the text)
#' with a minimum frequency (\code{min_freq}, default = 1\%)).
#'
#' @param ... Unquoted expressions of free text variable names from which to
#'   extract information.
#' @inheritParams skipgram_append
#' @importFrom purrr map
#' @importFrom dplyr select as_tibble left_join
#' @importFrom rlang .data
#' @return \code{data} with additional Boolean variables describing skipgrams in
#'   \code{...}
#' @family free text functions
#' @export
#' @seealso Principle underlying function: \code{\link[quanteda]{tokens_ngrams}}
#' @references Guthrie, D., Allison, B., Liu, W., Guthrie, L. & Wilks, Y. A
#'   Closer Look at Skip-gram Modelling. in Proceedings of the Fifth
#'   International Conference on Language Resources and Evaluation (LREC’06)
#'   (European Language Resources Association (ELRA), 2006).
#'   
#'   Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018).
#'   “quanteda: An R package for the quantitative analysis of textual data.” _Journal
#'   of Open Source Software_, *3*(30), 774. doi:10.21105/joss.00774
#'   <https://doi.org/10.21105/joss.00774>, <https://quanteda.io>.
#'   
#'   Feinerer I, Hornik K (2020). _tm: Text Mining Package_. R package version 0.7-8,
#'   <https://CRAN.R-project.org/package=tm>.
#'   
#'   Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in
#'   R. Journal of Statistical Software 25(5): 1-54. URL:
#'   https://www.jstatsoft.org/v25/i05/.
#' @examples
#' data(example_data)
#' extract_freetext(example_data, patient_id, min_freq = 0.6, free_text)
extract_freetext <- function(data, id_var, min_freq = 1, ...) {

  id_var <- dplyr::enquo(id_var)
  vars <- dplyr::enquos(...)

  ids <- pull(data, !! id_var)

  data %>%
    dplyr::select(!!! vars) %>%
    purrr::map(function(x) skipgram_identify(x, ids)) %>%
    Reduce(function(...) dplyr::left_join(..., by='doc_id'), .) %>%
    dplyr::as_tibble() %>%
    skipgram_append(data = data, min_freq = min_freq, id_var = !! id_var) %>%
    dplyr::select(-(!!! vars))

  # TODO: add source variable prefix
}
