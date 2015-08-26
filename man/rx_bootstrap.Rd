\name{rx_bootstrap}
\alias{rx_bootstrap}
\title{Bootstraps treatment from a dataset of treatment assignments}
\description{Takes a dataset on treatment assignment and
bootstraps from it by covariate combinations to assign
treatment in a population}
\usage{rx_bootstrap(data_bootrows, rx_var = "rx", n_sim, prefix = "sim")}
\arguments{
  \item{data_bootrows}{A data frame that has columns sim1...simX indicating
rows for X bootstrapped datasets}
  \item{rx_var}{Variable that will be returned from the bootstrapped
datasets}
  \item{n_sim}{Number of bootstrapped datasets/simulations}
  \item{prefix}{Prefix for the names of each bootstrapped dataset}
}

\value{Data frame of nsim treatment assignments, where the
variable "sim" is the intermediate outcome, in the form
of "testresult-binarytreatmentindicator"}

\author{Jeanette Birnbaum & Leslie Mallinger}
\note{This function really needs revamping. It's
ridiculously specific to the exact usage, and that itself
seems illogical!}




