% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semantic_enrich.R
\name{normalize}
\alias{normalize}
\title{Min max normalization}
\usage{
normalize(x, na.rm = TRUE)
}
\arguments{
\item{x}{numeric vector}

\item{na.rm}{a logical indicating whether missing values should be removed. Default = TRUE.}
}
\value{
normalised \code{x}
}
\description{
Normalizes values in \code{x} to be between 0 and 1 using min-max
normalization.
}
