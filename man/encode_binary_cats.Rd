% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC_clean.R
\name{encode_binary_cats}
\alias{encode_binary_cats}
\title{Encode categorical variables as binary factors}
\usage{
encode_binary_cats(data, ..., values = NULL)
}
\arguments{
\item{data}{A data frame, data frame extension (e.g. a tibble), or a lazy
data frame (e.g. from dbplyr or dtplyr).}

\item{...}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> One or more unquoted
expressions separated by commas. Variable names can be used as if they
were positions in the data frame, so expressions like \code{x:y} can
be used to select a range of variables.}

\item{values}{Optional named vector of user-defined values for binary values
using \code{binary_label_1 = binary_label_2} syntax (e.g. \code{c("No" =
"Yes")} would assign level 1 to "No" and 2 to "Yes").}
}
\value{
dataset with specified binary categories converted to factors.
}
\description{
In a data frame, converts binary categories to factors. Ordering of levels is
standardised to: \code{negative_finding, positive_finding}. This embeds a
standardised numeric relationship between the binary categories while
preserving value labels.
}
\details{
Binary categories to convert can be specified with a named character vector,
specified in \code{values}. The syntax of the named vector is:
\code{negative_finding = positive_finding}. If \code{values} is not
provided, the default list will be used: \code{"No"="Yes", "No/unknown" =
"Yes", "no/unknown" = "Yes", "Non-user" = "User", "Never" = "Ever", "WT" =
"MT"}.
}
\examples{
# use built-in values. Note: rural_urban is not modified
# Note: diabetes is not modified because "missing" is interpreted as a third category.
# strings_to_NA() should be applied first
encode_binary_cats(example_data, hypertension, rural_urban)

# use custom values. Note: rural_urban is now modified as well.
encoded_data <- encode_binary_cats(example_data, hypertension, rural_urban,
                   values = c("No"= "Yes", "rural" = "urban"))

# to demonstrate the new numeric encoding:
dplyr::mutate(encoded_data, hypertension_num = as.numeric(hypertension), .keep = "used") 
}
