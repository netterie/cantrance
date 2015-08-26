\name{rx_logit}
\alias{rx_logit}
\title{Estimates the probability of receiving treatment conditional on specified covariates and simulates treatment assignment}
\description{Runs a logistic regression of treatment
assignent on each bootstrapped dataset, predicts
probability of treatment assignment in the population,
and then simulates a treatment assignment for each person
based on how the predicted probability compares to a draw
from the uniform distribution}
\usage{rx_logit(data, bootrows, rxvar = "rx", covars, isfactor)}
\arguments{
  \item{data}{Data frame of treatment assignments and covariates, 
along with bootstrapped row numbers for nsim
datasets}
  \item{bootrows}{Matrix/df with row indicators that can be applied to
data to recover different bootstraps of edata. Each
column is a different bootstrap (sim) of the data.}
  \item{rxvar}{Treatment assignment variable}
  \item{covars}{Vector of covariate names}
  \item{isfactor}{A vector of TRUE/FALSE values that indicate whether
each covariate should be considered a factor variable}
}

\value{Data frame of nsim treatment assignments, where the
variable "sim" is the intermediate outcome, in the form
of "testresult-binarytreatmentindicator"}

\author{Jeanette Birnbaum & Leslie Mallinger}





