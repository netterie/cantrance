\name{summarize_cmprsk}
\alias{summarize_cmprsk}
\title{Summarize multiple crude survival curves by covariate groups}
\description{Compute the crude KM curve for a
distribution with competing risk over multiple
simulations, and return the distribution of medians and
a summary of the crude survival at each of the specified
"xvalues"}
\usage{summarize_cmprsk(cancer, other, xvalues = seq(0, 10, 0.5), covar = NULL, 
    covar.order = NULL)}
\arguments{
  \item{cancer}{Matrix of times to main endpoint. Rows are
individuals}
  \item{other}{Matrix of times to competing endpoint. Rows are
individuals}
  \item{xvalues}{Vector of times to evaluate K-M survival
probabilities }
  \item{covar}{Vector of covariate values for each individual by
which crude survivals should be computed}
  \item{covar.order}{Vector of the unique covariate values in their
preferred order}
}

\value{A list of 1) Crude median survivals for each simulation
(rows) by covariate group (columns) and 2) Median,
lower, and upper bound across simulations of crude
survival probabilities at each timepoint specified in
xvalues, by covariate group (indicated by rownames)}

\author{Jeanette Birnbaum & Leslie Mallinger}





