\name{bootstrap_rows}
\alias{bootstrap_rows}
\title{Bootstraps datasets and returns the row numbers (not names) for each sample, instead of the actual datasets}
\description{Takes a dataset, resamples the specified
number of times with replacement, and returns the dataset
along with the resampled row numbers.}
\usage{bootstrap_rows(data, n_row = nrow(data), n_sim, prefix = "sim", 
    return_data = TRUE)}
\arguments{
  \item{data}{A data frame from which to bootstrap}
  \item{n_row}{Number of rows to sample for each simulation}
  \item{n_sim}{Number of simulations for which to bootstrap}
  \item{prefix}{Prefix for the names of each bootstrapped dataset}
  \item{return_data}{If false, only the row indicators will be returned;
default is to also return the data}
}

\value{The input data with additional columns indicating nsim
bootstrapped datasets, where the variable indicated by
prefix contains row numbers for constructing the
bootstrapped datasets}

\author{Jeanette Birnbaum & Leslie Mallinger}





