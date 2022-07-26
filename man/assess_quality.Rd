% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/high_level.R
\name{assess_quality}
\alias{assess_quality}
\title{Assess quality of a dataset}
\usage{
assess_quality(data, id_var, consis_tbl)
}
\arguments{
\item{data}{A data frame, data frame extension (e.g. a tibble), or a lazy
data frame (e.g. from dbplyr or dtplyr).}

\item{id_var}{An unquoted  expression which corresponds to a variable (column) in
\code{data} which identifies each row (sample).}

\item{consis_tbl}{data frame or tibble containing information on internal
consistency rules (see "Consistency Table Requirements" section)}
}
\value{
Nested list of quality measurements
}
\description{
Provides information on the quality of a dataset. Assesses dataset's
completeness, internal consistency, and entropy.
}
\details{
Wraps several quality assessment functions from \code{eHDPrep}
  and returns a nested list with the following structure:
  \describe{
  \item{completeness}{- A list of completeness assessments:
  \enumerate{
  \item Tibble of variable (column) completeness (via \code{\link{variable_completeness}})
  \item Tibble of row (sample) completeness (via \code{\link{row_completeness}})
  \item Plot of row and variable completeness (via \code{\link{plot_completeness}})
  \item Completeness heatmap (via \code{\link{completeness_heatmap}})
  \item A function which creates a clean canvas before  plotting the completeness heatmap.
  }
  }
  \item{internal_inconsistency}{- Tibble of internal inconsistencies, if any
  are present and if a consistency table is supplied (via
  \code{\link{identify_inconsistency}}).}
  \item{vars_with_zero_entropy}{- Names of variables (columns) with zero entropy (via
  \code{\link{zero_entropy_variables}})}
  
}
}
\section{Consistency Table Requirements}{
 Table must have exactly five character columns.
  The columns should be ordered according to the list below which describes the
  values of each column:
  \enumerate{
  \item First column name of data values that will be subject to 
  consistency checking. String. Required.
  \item Second column name of data values that will be subject to 
  consistency checking. String. Required.
  \item Logical test to compare columns one and two. One of: ">",">=",
  "<","<=","==", "!=". String. Optional if columns 4 and 5 have non-\code{NA} values.
  \item Either a single character string or a colon-separated range of
  numbers which should only appear in column A. Optional if column 3 has a
  non-\code{NA} value.
  \item Either a single character string or a colon-separated range of
  numbers which should only appear in column B given the value/range
  specified in column 4. Optional if column 3 has a non-\code{NA} value.
  }
  Each row should detail one test to make.
  Therefore, either column 3 or columns 4 and 5 must contain non-\code{NA}
  values.
}

\examples{
# general example
data(example_data)
res <- assess_quality(example_data, patient_id)

# example of internal consistency checks on more simple dataset
# describing bean counts
require(tibble)

# creating `data`:
beans <- tibble::tibble(red_beans = 1:15,
blue_beans = 1:15,
total_beans = 1:15*2,
red_bean_summary = c(rep("few_beans",9), rep("many_beans",6)))

# creating `consis_tbl`
bean_rules <- tibble::tribble(~varA, ~varB, ~lgl_test, ~varA_boundaries, ~varB_boundaries,
"red_beans", "blue_beans", "==", NA, NA,
"red_beans", "total_beans", "<=", NA,NA,
"red_beans", "red_bean_summary", NA, "1:9", "few_beans",
"red_beans", "red_bean_summary", NA, "10:15", "many_beans")

# add some inconsistencies
beans[1, "red_bean_summary"] <- "many_beans"
beans[1, "red_beans"] <- 10

res <- assess_quality(beans, consis_tbl = bean_rules)

# variable completeness table
res$completeness$variable_completeness

# row completeness table
res$completeness$row_completeness

# show completeness of rows and variables as a bar plot
res$completeness$completeness_plot

# show dataset completeness in a clustered heatmap
res$completeness$plot_completeness_heatmap(res$completeness)

# show any internal inconsistencies
res$internal_inconsistency

# show any variables with zero entropy
res$vars_with_zero_entropy
}
\seealso{
Other high level functionality: 
\code{\link{apply_quality_ctrl}()},
\code{\link{review_quality_ctrl}()},
\code{\link{semantic_enrichment}()}
}
\concept{high level functionality}
