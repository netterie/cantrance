\name{calc_ac_lifespan_pop}
\alias{calc_ac_lifespan_pop}
\title{Use the individual calc_ac_lifespan function to estimate lifespans of a population}
\description{Given a population with birth years and
ages, returns random draws of their ages at other-cause
death}
\usage{calc_ac_lifespan_pop(popdata, bootrows, results_as_matrix = FALSE, 
    survHR = 1)}
\arguments{
  \item{popdata}{Data frame where individuals are rows, with columns 
birth_year, age, and male}
  \item{bootrows}{Matrix/df of row indicators that can be applied to
the data to recover different bootstraps of the
data. Each column is a different bootstrap of the
data}
  \item{results_as_matrix}{Convert the results from a data frame to a matrix?}
  \item{survHR}{Hazard ratio for survival as a modification of the
life table in use}
}

\value{A data frame or matrix with nsim columns of randomly
drawn ages at other-cause death for individuals (rows)}

\author{Jeanette Birnbaum & Leslie Mallinger}





