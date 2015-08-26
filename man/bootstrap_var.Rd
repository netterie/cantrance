\name{bootstrap_var}
\alias{bootstrap_var}
\title{Bootstrap a single variable from a population}
\description{Given a data frame or a list of data frames with 
a specified variable,bootstrap from that variable according to rows}
\usage{bootstrap_var(dset, variable, rows)}
\arguments{
  \item{dset}{Data frame or matrix with column for variable of interest
OR list of data frames of which one has variable of interest}
  \item{variable}{Column name for variable of interest}
  \item{rows}{Matrix/df of row indicators that can be applied to
recover different bootstraps of the data.
Each column is a different bootstrap of the
data
OR a list of such indicators that correspond to the 
list dset}
}

\value{A matrix/df of the same dimensions as dset, containing 
values for the variable bootstrapped from dset}

\author{Jeanette Birnbaum & Leslie Mallinger}





