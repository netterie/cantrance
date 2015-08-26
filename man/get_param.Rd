\name{get_param}
\alias{get_param}
\title{Calculate parameters for specified distribution, given mean and/or median}
\description{Take in data and return lognorm or exponential parameters}
\usage{get_param(df, distr, anchor = NULL)}
\arguments{
  \item{df}{Data frame with columns mean and median}
  \item{distr}{Specify distribution: "lognorm" or "exponential" 
or "truncnorm"}
  \item{anchor}{If choosing exponential, specify whether 
its one parameter should be calculated
based off the mean or median. If no anchor
is specified, the default will be to 
use whichever column has non-NA entries}
}



\author{Jeanette Birnbaum & Leslie Mallinger}





