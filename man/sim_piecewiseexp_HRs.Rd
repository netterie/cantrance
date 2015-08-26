\name{sim_piecewiseexp_HRs}
\alias{sim_piecewiseexp_HRs}
\title{Returns random deviates from a regular or piecewise exponential distribution for a population with varying rates, specified by HRs}
\description{Rates are specified as baseline hazard(s)
and hazard ratios}
\usage{sim_piecewiseexp_HRs(baseline_rates, HRs, prefix = NA)}
\arguments{
  \item{baseline_rates}{Data frame of rates specifying the baseline hazards
(column name "rate") and changepoints (column name
"times"). Nrow = 1 if using a regular exponential}
  \item{HRs}{Matrix/df of HRs to be applied }
  \item{prefix}{Prefix to apply to the sim number for column names}
}

\value{Matrix of random deviates of the same dimension as the
HRs matrix}

\author{Jeanette Birnbaum & Leslie Mallinger}





