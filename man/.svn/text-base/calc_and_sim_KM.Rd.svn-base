\name{calc_and_sim_KM}
\alias{calc_and_sim_KM}
\title{Estimates KM curves from a dataset and simulates from them}
\description{Estimates KM curves on each 
bootstrapped dataset, and simulates a time to event
for each individual}
\usage{calc_and_sim_KM(data, bootrows, covars = 1, extrapolate = FALSE, 
    extrapolation_distr = NULL, extrapolation_years = NULL, prefix = NULL)}
\arguments{
  \item{data}{Data frame of times to event and (optionally) covariates
Required variables are "time" and "status"}
  \item{bootrows}{Matrix/df with row indicators that can be applied to
data to recover different bootstraps of edata. Each
column is a different bootstrap (sim) of the data.}
  \item{covars}{1 (no covariates) or vector of covariate names}
  \item{extrapolate}{If FALSE, will return NA for simulated values beyond 
the maximum event time. If TRUE, requires the 
remaining parameters to be specified}
  \item{extrapolation_distr}{One of the survival regression distributions 
(see help for survreg.distributions).
Will be used to fit a parametric model to the data
for extrapolating beyond the maximum event time}
  \item{extrapolation_years}{Number of years, counting backwards from the 
maximum event time, to use in modeling the parametric
curve for extrapolating beyond the maximum event
time}
  \item{prefix}{Optional prefix for colnames of output}
}

\value{Matrix of times to event of the same dimensions as 
bootrows}

\author{Jeanette Birnbaum & Leslie Mallinger}





