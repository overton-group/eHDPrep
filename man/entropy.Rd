% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC_info_theory.R
\name{entropy}
\alias{entropy}
\title{Calculate Entropy of a Vector}
\usage{
entropy(x, unit = c("bits"))
}
\arguments{
\item{x}{Input vector}

\item{unit}{Unit to measure entropy. Either "bits" (default) or "nats".}
}
\value{
Entropy of input variable
}
\description{
Calculates Shannon Entropy of a vector in bits (default) or natural units.
Missing values are omitted from the calculation.
}
\examples{
# no entropy:
vec <- c(1,1,1,1,1,1)
entropy(vec)

# entropy
vec <- c(1,2,3,4,5,6)
entropy(vec)
}
\references{
Shannon, C. E. A mathematical theory of communication. The Bell
  System Technical Journal 27, 379–423 (1948).
}
