\name{sim_parametric_survreg}
\alias{sim_parametric_survreg}
\title{Simulate from a parametric survival model run using the survreg command}
\description{Given a survreg object and a vector or matrix of linear 
predictors, returns random deviates from the distribution used
to estimate the survreg model}
\usage{sim_parametric_survreg(survreg_object, linear_predictor, distribution, 
    n_deviates, time_lower_bound = 0)}
\arguments{
  \item{survreg_object}{The survreg model object}
  \item{linear_predictor}{Vector or matrix of linear predictors. Note that these may 
be derived from the data used to estimate the survreg object
OR from a new dataset}
  \item{distribution}{String with the name of the distribution used to estimate survreg
Currently, only exponential and weibull are supported }
  \item{n_deviates}{Number of deviates to return}
  \item{time_lower_bound}{If a conditional simulation is desired, enter the lower 
bound for time}
}



\author{Jeanette Birnbaum & Leslie Mallinger}





