\name{bootstrap_rows_list}
\alias{bootstrap_rows_list}
\title{Bootstrap rows from a list of data frames}

\usage{bootstrap_rows_list(popdata, nrows, nsims, pfx)}
\arguments{
  \item{popdata}{List of data frames from which rows are to be 
bootstrapped}
  \item{nrows}{Number of rows to bootstrap}
  \item{nsims}{Number of simulations for which to bootstrap}
  \item{pfx}{Prefix for the names of each bootstrapped dataset}
}

\value{A list of of data frames in which each data frame has
nsims columns of bootstrapped rows}

\author{Jeanette Birnbaum & Leslie Mallinger}





