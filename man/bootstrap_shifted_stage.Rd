\name{bootstrap_shifted_stage}
\alias{bootstrap_shifted_stage}
\title{Bootstrap a stage-shifted population for multiple simulations}
\description{For each simulation, implement a stage shift
within a starting population, then boostrap rows to 
sample}
\usage{bootstrap_shifted_stage(stgs, base_pop, new_cts, ordr, bootrows, 
    prefix = "sim")}
\arguments{
  \item{stgs}{Vector of stages, in the order denoted by ordr}
  \item{base_pop}{Vector representing the starting population. Each 
entry represents an individual and denotes the stage
in which they start.}
  \item{new_cts}{Number of individuals to be assigned to each stage
in the new, stage-shifted population}
  \item{ordr}{Numeric vector denoting the order in which stage
progression occurs, where 1=least invasive stage
and max(ordr)=most invasive stage. Individuals will
be shifted to a stage that is lower than or equal
to their starting stage.}
  \item{bootrows}{Matrix/df of row indicators that can be applied to
the data to recover different bootstraps of the
data. Each column is a different bootstrap of the
data.}
  \item{prefix}{Prefix for the names of each bootstrapped dataset}
}

\value{A matrix of stages in which rows denote individuals and
columns denote simulations}

\author{Jeanette Birnbaum & Leslie Mallinger}





