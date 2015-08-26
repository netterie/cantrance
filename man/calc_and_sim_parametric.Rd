\name{calc_and_sim_parametric}
\alias{calc_and_sim_parametric}
\title{Function to calculate a parametric survival model and then simulate from it}
\description{Estimates a parametric survival model and then returns random }
\usage{calc_and_sim_parametric(data, distribution, model_call = 1, bootrows = NULL, 
    newdata = NULL, prefix = NULL, time_lower_bound = 0)}
\arguments{
  \item{data}{Data frame on which to fit the model}
  \item{distribution}{One of the survreg.distributions (see corresponding help file)}
  \item{model_call}{The right hand side of a model formula, i.e. if your
model is Surv(time,status)~rx+as.factor(surg)*age, 
input 'rx+as.factor(surg)*age'}
  \item{bootrows}{Optional matrix of row indicators defining bootstrapped samples
of the data. Leave as null if you want to run the model
only once, rather than on multiple bootstrapped
samples of the data}
  \item{newdata}{Optional data frame for which simulated values will 
be returned. If not null, the process will be
1) Model is fit to 'data' (or a bootstrapped sample of 'data')
2) 'newdata' is used to define the value of the linear
predictor for each person
3) A time to event is simulated for each person using the
appropriate linear predictor}
  \item{prefix}{Optional prefix for colnames of output}
  \item{time_lower_bound}{Optional lower bound for simulated times to event, 
i.e. you may simulate from a conditional distribution, 
conditional on surviving to time_lower_bound}
}

\value{Simulated times to event in the following form:
1) If newdata is NULL and bootrows is not, 
a matrix of same dimension as bootrows
2) If newdata and bootrows are NULL, a nrow(data)x1 matrix
3) If newdata is not NULL, a nrow(newdata)x1 matrix}

\author{Jeanette Birnbaum & Leslie Mallinger}





