% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_export.R
\name{export_dataset}
\alias{export_dataset}
\title{Export data to delimited file}
\usage{
export_dataset(x, file, format = "csv", ...)
}
\arguments{
\item{x}{A data frame or tibble to write to disk.}

\item{file}{File or connection to write to.}

\item{format}{Character constant. "csv" (default) or "tsv"}

\item{...}{parameters to pass to \code{\link[readr]{write_csv}} or \code{\link[readr]{write_tsv}}.}
}
\value{
\code{x} saved to \code{file} in selected \code{format}
}
\description{
Save dataset in .csv or .tsv format. A wrapper function for \code{readr}'s
\code{\link[readr]{write_csv}} and \code{\link[readr]{write_tsv}}.
}
\examples{
data(example_data)
tmp = tempfile(fileext = ".csv")
export_dataset(example_data, tmp)
}
\seealso{
\code{\link[readr]{write_csv}} and \code{\link[readr]{write_tsv}}

Other import to/export from 'R' functions: 
\code{\link{import_dataset}()}
}
\concept{import to/export from 'R' functions}
