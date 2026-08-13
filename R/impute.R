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

##' Most frequent (mode) value of a vector
##'
##' Internal. Returns the most frequently occurring non-\code{NA} value in a
##' vector. Ties are resolved by returning the value which appears first in the
##' data. Returns \code{NA} (of the same type as \code{x}) if all values are
##' \code{NA}.
##'
##' @param x input vector
##' @return single value of the same type as \code{x}
##' @noRd
##' @examples
##' statistical_mode(c("a","a","b",NA))
statistical_mode <- function(x) {
  x_nona <- x[!is.na(x)]
  if (length(x_nona) == 0L) return(x[NA_integer_][1])
  # tabulate in order of first appearance so ties resolve to the first-seen value
  ux <- unique(x_nona)
  ux[which.max(tabulate(match(x_nona, ux)))]
}

##' Fill value for imputing a vector
##'
##' Internal. Computes the single value with which \code{NA}s in \code{x} will
##' be replaced. Numeric vectors support "median", "mean", and "mode";
##' non-numeric vectors are always imputed with the mode (the most frequent
##' value) as means and medians are undefined for them.
##'
##' @param x input vector
##' @param method one of "median", "mean", "mode", or "constant"
##' @param constant value used when \code{method == "constant"}
##' @return single value
##' @noRd
impute_fill_value <- function(x, method = "median", constant = NULL) {
  if (method == "constant") {
    constant
  } else if (is.numeric(x)) {
    switch(method,
           median = stats::median(x, na.rm = TRUE),
           mean   = mean(x, na.rm = TRUE),
           mode   = statistical_mode(x),
           stop("Unsupported `method`: ", method, call. = FALSE))
  } else {
    # mean / median are undefined for non-numeric vectors: fall back to mode
    statistical_mode(x)
  }
}

##' Impute a single vector with a simple summary statistic
##'
##' Internal. Replaces \code{NA} values in \code{x} with a single imputed value
##' derived from the non-missing values (see \code{impute_fill_value()}).
##'
##' The class of \code{x} is always preserved. Assigning into a vector coerces
##' it to the widest common type, which would silently convert a variable (e.g.
##' filling a numeric variable with a character \code{constant} turns the whole
##' variable into character), so three safeguards are applied:
##' \itemize{
##' \item integer vectors receive a rounded integer fill, as medians and means
##'   of integers are frequently fractional;
##' \item factors gain the fill as a level when it is not already one, rather
##'   than the fill being dropped to \code{NA};
##' \item any remaining class change is an error naming the variable.
##' }
##'
##' @param x input vector
##' @param method one of "median", "mean", "mode", or "constant"
##' @param constant value used when \code{method == "constant"}
##' @param var name of the variable, used in messages and errors
##' @return \code{x} with \code{NA} values replaced, of the same class as
##'   \code{x}
##' @noRd
impute_vector <- function(x, method = "median", constant = NULL, var = NULL) {
  if (!anyNA(x)) return(x)

  # a variable with no observed values provides no statistic to impute from
  if (all(is.na(x)) && method != "constant") return(x)

  label <- if (is.null(var)) "" else paste0("`", var, "`: ")
  fill <- impute_fill_value(x, method = method, constant = constant)

  if (length(fill) != 1L) {
    stop(label, "the imputation value must be a single value, not length ",
         length(fill), ".", call. = FALSE)
  }
  if (is.na(fill)) return(x)

  # extend the levels so a fill absent from `x` is not dropped to NA
  if (is.factor(x)) {
    fill <- as.character(fill)
    if (!fill %in% levels(x)) levels(x) <- c(levels(x), fill)
    x[is.na(x)] <- fill
    return(x)
  }

  # a fractional fill would both promote an integer variable to double and
  # introduce a value the variable cannot take (e.g. 1.5 for a count)
  if (is.integer(x) && is.double(fill)) {
    if (method == "constant" && !isTRUE(fill == round(fill))) {
      stop(label, "`constant` must be a whole number for an integer variable ",
           "(got ", format(fill), ").", call. = FALSE)
    }
    rounded <- as.integer(round(fill))
    if (method != "constant" && !isTRUE(fill == rounded)) {
      message(label, "integer variable, so the ", method, " (", format(fill),
              ") was rounded to ", rounded, ".")
    }
    fill <- rounded
  }

  out <- x
  out[is.na(out)] <- fill

  # an all-NA variable is logical by default and so carries no type of its own:
  # the fill is allowed to determine it
  untyped <- is.logical(x) && all(is.na(x))

  # imputation must never silently change the class of a variable
  if (!untyped && !identical(class(out), class(x))) {
    stop(label, "imputing with ", format(fill), " would change the variable ",
         "from <", paste(class(x), collapse = "/"), "> to <",
         paste(class(out), collapse = "/"), ">.\n",
         "\u2716 Supply a value of the same type as the variable.",
         call. = FALSE)
  }

  out
}

#' Impute missing values in a dataset
#'
#' Replaces missing (\code{NA}) values in a dataset using either simple summary
#' statistics or k-nearest-neighbours (kNN) imputation.
#'
#' @details
#' Two families of imputation are provided:
#' \describe{
#' \item{Simple imputation}{Each selected variable (column) is imputed
#'   independently using a summary statistic of its own non-missing values.
#'   \itemize{
#'   \item \code{"auto"} (default): numeric variables are imputed with their
#'     median and non-numeric variables with their mode (most frequent value).
#'   \item \code{"median"} / \code{"mean"}: numeric variables are imputed with
#'     the chosen statistic. Non-numeric variables fall back to the mode (a
#'     message is emitted) as means and medians are undefined for them.
#'   \item \code{"mode"}: all selected variables are imputed with their mode.
#'   \item \code{"constant"}: all selected variables have missing values
#'     replaced with \code{constant}.
#'   }}
#' \item{kNN imputation (\code{method = "knn"})}{Missing values are imputed from
#'   the \code{k} most similar rows (samples), using all other variables as
#'   predictors. Similarity is measured with Gower distance, which handles mixed
#'   numeric and categorical data. This method uses \code{\link[VIM]{kNN}} from
#'   the \pkg{VIM} package, which is only required (\code{Suggests}) when this
#'   method is used. Unlike the simple methods, imputed values depend on each
#'   row's own profile, better preserving relationships between variables.}
#' }
#'
#' Only variables (columns) selected via \code{...} are imputed; if none are
#' supplied all variables are eligible. For \code{method = "knn"}, all variables
#' (except any supplied to \code{ignore}) are still used to compute distances,
#' even if they are not themselves imputed.
#'
#' Imputation never changes the class of a variable:
#' \itemize{
#' \item Integer variables are imputed with a rounded integer, as the median or
#'   mean of a set of integers is frequently fractional. The rounding is
#'   reported via a message.
#' \item Factors gain the imputed value as an additional level if it is not
#'   already one.
#' \item Any other type mismatch (e.g. a character \code{constant} for a
#'   numeric variable) is an error rather than a silent conversion of the whole
#'   variable.
#' }
#'
#' Variables which contain no non-missing values are skipped (there is no
#' statistic to impute from) and reported as such, except under
#' \code{method = "constant"} where \code{constant} does not depend on the
#' observed values.
#'
#' Imputation is deterministic: the same input always yields the same output.
#' The number of values imputed per variable is reported via a message (silence
#' with \code{\link[base]{suppressMessages}}). Value-level changes can be
#' reviewed with \code{\link{review_quality_ctrl}}.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param ... <\code{\link[dplyr]{dplyr_tidy_select}}> Variables (columns) to
#'   impute. If none are supplied, all variables are imputed.
#' @param method Imputation method. One of \code{"auto"} (default),
#'   \code{"median"}, \code{"mean"}, \code{"mode"}, \code{"constant"}, or
#'   \code{"knn"}. See Details.
#' @param constant Value used to replace missing values when
#'   \code{method = "constant"}. Must be of the same type as the variables being
#'   imputed so that their class is preserved (a whole number for integer
#'   variables); an incompatible value is an error. For factors, a value which
#'   is not an existing level is added as one.
#' @param k Number of nearest neighbours to use when \code{method = "knn"}
#'   (Default: 5).
#' @param ignore <\code{\link[dplyr]{dplyr_tidy_select}}> Variables (columns) to
#'   exclude from being used as predictors when \code{method = "knn"} (e.g. a
#'   row identifier). Ignored for the simple methods.
#' @importFrom rlang enquos expr
#' @importFrom magrittr %>%
#' @family high level functionality
#' @return \code{data} with missing values in the selected variables imputed.
#' @references Kowarik, A. & Templ, M. Imputation with the R Package VIM.
#'   \emph{Journal of Statistical Software} \strong{74}, 1-16 (2016).
#'   \doi{10.18637/jss.v074.i07}
#'
#'   Gower, J. C. A General Coefficient of Similarity and Some of Its
#'   Properties. \emph{Biometrics} \strong{27}, 857-871 (1971).
#'   \doi{10.2307/2528823}
#'
#'   Little, R. J. A. & Rubin, D. B. \emph{Statistical Analysis with Missing
#'   Data}. (John Wiley & Sons, 2019).
#' @export
#'
#' @examples
#' data(example_data)
#'
#' # median (numeric) / mode (categorical) imputation of all variables:
#' res <- impute_missing_values(example_data)
#'
#' # impute only specific variables:
#' res <- impute_missing_values(example_data, tumoursize, diabetes,
#'                              method = "median")
#'
#' # k-nearest-neighbours imputation (requires the 'VIM' package),
#' # excluding the identifier from the distance calculation:
#' \donttest{
#' if (requireNamespace("VIM", quietly = TRUE)) {
#'   res <- impute_missing_values(example_data, method = "knn",
#'                                ignore = patient_id)
#' }
#' }
impute_missing_values <- function(data, ..., method = "auto", constant = NULL,
                                  k = 5, ignore = NULL) {
  method <- match.arg(method, c("auto", "median", "mean", "mode",
                                "constant", "knn"))

  if (method == "constant" && is.null(constant)) {
    stop("`constant` must be supplied when `method = \"constant\"`.",
         call. = FALSE)
  }

  # resolve selected columns (default: all)
  vars <- rlang::enquos(...)
  if (length(vars) == 0L) {
    selected <- seq_along(data)
  } else {
    selected <- unname(tidyselect::eval_select(rlang::expr(c(!!!vars)), data))
  }
  names(selected) <- names(data)[selected]

  if (method == "knn") {
    return(impute_knn(data, selected, k = k, ignore = rlang::enquo(ignore)))
  }

  # simple imputation, column by column
  for (i in selected) {
    x <- data[[i]]
    n_missing <- sum(is.na(x))
    if (n_missing == 0L) next

    nm <- names(data)[i]

    # a variable with no observed values has no statistic to impute from, so
    # report it as skipped rather than claiming values were imputed
    if (all(is.na(x)) && method != "constant") {
      message("`", nm, "` contains no non-missing values: not imputed.")
      next
    }

    # inform user when a mean/median request is downgraded to mode
    if (method %in% c("mean", "median") && !is.numeric(x)) {
      message("`", nm, "` is not numeric: imputing with mode ",
              "instead of ", method, ".")
    }

    # effective per-column method (auto dispatches by type; mean/median on a
    # non-numeric column falls back to mode within impute_vector())
    m <- if (method == "auto") {
      if (is.numeric(x)) "median" else "mode"
    } else if (method %in% c("mean", "median") && !is.numeric(x)) {
      "mode"
    } else method

    data[[i]] <- impute_vector(x, method = m, constant = constant, var = nm)
    message("`", nm, "`: ", n_missing, " value(s) imputed (", m, ").")
  }

  data
}

##' k-nearest-neighbours imputation
##'
##' Internal. Wraps \code{\link[VIM]{kNN}} to impute the selected columns of
##' \code{data}. Column order, names and (where possible) tibble class are
##' preserved. VIM's indicator columns are suppressed.
##'
##' @param data data frame to impute
##' @param selected named integer vector of column positions to impute
##' @param k number of neighbours
##' @param ignore quosure of columns to exclude from distance predictors
##' @return \code{data} with the selected columns imputed
##' @importFrom rlang expr as_label quo_is_null
##' @noRd
impute_knn <- function(data, selected, k = 5, ignore = NULL) {
  if (!requireNamespace("VIM", quietly = TRUE)) {
    stop("`method = \"knn\"` requires the 'VIM' package.\n",
         "\u2716 Install it with install.packages(\"VIM\").", call. = FALSE)
  }

  impute_names <- names(data)[selected]

  # predictors are all columns except any the user asked to ignore
  dist_vars <- names(data)
  if (!rlang::quo_is_null(ignore)) {
    ignore_sel <- tidyselect::eval_select(rlang::expr(!!ignore), data)
    dist_vars <- setdiff(dist_vars, names(data)[ignore_sel])
  }

  was_tibble <- inherits(data, "tbl_df")

  # VIM::kNN appends logical "_imp" indicator columns unless imp_var = FALSE
  res <- VIM::kNN(as.data.frame(data),
                  variable = impute_names,
                  dist_var = dist_vars,
                  k = k,
                  imp_var = FALSE)

  # restore original column order and class
  res <- res[names(data)]
  if (was_tibble) res <- dplyr::as_tibble(res)

  for (nm in impute_names) {
    n_missing <- sum(is.na(data[[nm]]))
    if (n_missing > 0L) {
      message("`", nm, "`: ", n_missing, " value(s) imputed (knn, k = ", k, ").")
    }
  }

  res
}
