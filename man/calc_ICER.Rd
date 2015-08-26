\name{calc_ICER}
\alias{calc_ICER}
\title{Calculate ICER across sims}
\description{Takes in data frames from calc_costs_QALYs
that represent populations to be compared, calculates the
ICERs for each sim, and returns summary statistics on the
ICER as well as on costs and QALYs for each population}
\usage{calc_ICER(dfNew, dfCompare, thresholds = NULL, perperson = NULL)}
\arguments{
  \item{dfNew}{A data frame returned by calc_costs_QALYs of
TotCost, QALY, and Sim, for the population with the
new intervention. If there are multiple
interventions,this can be a list of data frames}
  \item{dfCompare}{The corresponding data frame for the comparison
group}
  \item{thresholds}{If data for a cost-effectiveness acceptability curve
is desired, a vector of thresholds to evaluate}
  \item{perperson}{To scale the costs and QALYs to be per person, 
specify the number of people in the pop rather than
NULL}
}

\value{A list of 1) The cost, LY, and QALY differences and
ICERs, 2) a data frame of summary statistics for those
measures, and 3) if thresholds is not NULL,
cost-effectiveness acceptability}

\author{Jeanette Birnbaum & Leslie Mallinger}





