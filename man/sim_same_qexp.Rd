\name{sim_same_qexp}
\alias{sim_same_qexp}
\title{Simulate new time to event using quantile from old time to event}

\usage{sim_same_qexp(oldtime, oldrate, newrate, prefix)}
\arguments{
  \item{oldtime}{Vector of time to event using old estimation method}
  \item{oldrate}{Vector of rates for exponential distribution used to
estimate oldtime}
  \item{newrate}{Vector of rates for exponential distribution to be
used to estimate new times to event}
  \item{prefix}{Prefix for column names in results vector/matrix}
}

\value{Vector of time to event using same quantile as old time
to event}

\author{Jeanette Birnbaum & Leslie Mallinger}





