% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC_completeness.R
\name{row_completeness}
\alias{row_completeness}
\title{Calculate Row Completeness in a Data Frame}
\usage{
row_completeness(data, id_var)
}
\arguments{
\item{data}{Data frame.}

\item{id_var}{Row identifier variable.}
}
\value{
Tibble detailing completeness statistics for each row in input data.
}
\description{
Calculates the completeness of each row/observation in a data frame.
}
\details{
Row completeness is measured by comparing the number of \code{NA} to
non-\code{NA} values. Returns the count of \code{NA} as well as the
percentage of \code{NA} values and the percentage completeness.
}
\examples{
data(example_data)
row_completeness(example_data, patient_id)
}
\seealso{
Other measures of completeness: 
\code{\link{assess_completeness}()},
\code{\link{compare_completeness}()},
\code{\link{completeness_heatmap}()},
\code{\link{plot_completeness}()},
\code{\link{variable_completeness}()}
}
\concept{measures of completeness}
