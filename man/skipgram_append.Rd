% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QC_freetext.R
\name{skipgram_append}
\alias{skipgram_append}
\title{Append Skipgram Presence Variables to Dataset}
\usage{
skipgram_append(skipgram_tokens, skipgrams2append, data, id_var, min_freq = 1)
}
\arguments{
\item{skipgram_tokens}{Output of \code{\link{skipgram_identify}}.}

\item{skipgrams2append}{Which skipgrams in \code{skipgram_tokens} to append
to dataset.}

\item{data}{Data frame to append skipgram variables to.}

\item{id_var}{An unquoted  expression which corresponds to a variable in
\code{data} which identifies each row.}

\item{min_freq}{Minimum percentage frequency of skipgram occurrence to
return. Default = 1.}
}
\value{
\code{data} with additional variables describing presence of
  skipgrams
}
\description{
Adds new variables to \code{data} which report the presence of skipgrams
(either those specified in \code{skipgrams2append} or, if not specified,
skipgrams with a minimum frequency (\code{min_freq}, default = 1)).
}
\examples{
data(example_data)
# identify skipgrams
toks_m <- skipgram_identify(x = example_data$free_text,
                            ids = example_data$patient_id,
                            max_interrupt_words = 5)
# add skipgrams by minimum frequency
skipgram_append(toks_m,
                id_var = patient_id,
                min_freq = 0.6,
                data = example_data)
# add specific skipgrams
skipgram_append(toks_m,
                id_var = patient_id,
                skipgrams2append = c("sixteen_week", "bad_strain"),
                data = example_data)
}
\references{
Guthrie, D., Allison, B., Liu, W., Guthrie, L. & Wilks, Y. A
  Closer Look at Skip-gram Modelling. in Proceedings of the Fifth
  International Conference on Language Resources and Evaluation (LREC’06)
  (European Language Resources Association (ELRA), 2006).
  
  Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018).
  “quanteda: An R package for the quantitative analysis of textual data.” _Journal
  of Open Source Software_, *3*(30), 774. doi:10.21105/joss.00774
  <https://doi.org/10.21105/joss.00774>, <https://quanteda.io>.
  
  Feinerer I, Hornik K (2020). _tm: Text Mining Package_. R package version 0.7-8,
  <https://CRAN.R-project.org/package=tm>.
  
  Ingo Feinerer, Kurt Hornik, and David Meyer (2008). Text Mining Infrastructure in
  R. Journal of Statistical Software 25(5): 1-54. URL:
  https://www.jstatsoft.org/v25/i05/.
}
\seealso{
Principle underlying function: \code{\link[quanteda]{tokens_ngrams}}

Other free text functions: 
\code{\link{extract_freetext}()},
\code{\link{skipgram_freq}()},
\code{\link{skipgram_identify}()}
}
\concept{free text functions}
