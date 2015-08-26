\name{summarize_censored}
\alias{summarize_censored}
\title{Summarize multiple Kaplan-Meiers by covariate groups}
\description{Compute the net KM curve for a distribution
with competing risk over multiple simulations, and return
the distribution of medians and a summary of the K-M
curves at each of the specified "xvalues"}
\usage{summarize_censored(cancer, other, xvalues = seq(0, 10, 0.5), 
    covar, covar.order = NULL)}
\arguments{
  \item{cancer}{Matrix of times to main endpoint. Rows are}
  \item{other}{Matrix of times to competing endpoint. Rows are
individuals}
  \item{xvalues}{Vector of times to evaluate K-M survival
probabilities }
  \item{covar}{Vector of covariate values for each individual by
which K-Ms should be computed}
  \item{covar.order}{Vector of the unique covariate values in their
preferred order}
}

\value{A list of 1) Net median survivals for each simulation
(rows) by covariate group (columns) and 2) Median,
lower, and upper bound across simulations of net
survival probabilities at each timepoint specified in
xvalues, by covariate group (indicated by rownames)}

\author{Jeanette Birnbaum & Leslie Mallinger}





