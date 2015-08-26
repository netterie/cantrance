\name{summarize_surv_types}
\alias{summarize_surv_types}
\title{Summarize mean and 95\% confidence intervals for mean, median, and time_max survival for a given type of survival}
\description{Return the mean and 2.5\% and 97.5\% quantiles
for mean, median, and time_max survival}
\usage{summarize_surv_types(lst, kvals, n_arms = 2)}
\arguments{
  \item{lst}{List of matrices med, mean, and summ to be
summarized. Within each matrix, rows are simulations
and columns are groups for which distinct summaries
will be returned}
  \item{kvals}{time for which to return k-time survival}
  \item{n_arms}{number of study arms. Leave as NULL to 
have the function compute it on based on
data in lst}
}

\value{A matrix of the mean, 2.5\%-ile, and 97.5\%-ile for mean,
median, and k-time survival}

\author{Jeanette Birnbaum & Leslie Mallinger}





