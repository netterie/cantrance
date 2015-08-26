\name{sim_piecewiseexp_HRs_restrictedrange}
\alias{sim_piecewiseexp_HRs_restrictedrange}
\title{Returns random deviates from a regular or piecewise exponential distribution for a population with varying rates, specified by HRs. Values can be simulated over a restricted range rather over [0,1]}
\description{Rates are specified as baseline hazard(s)
and hazard ratios}
\usage{sim_piecewiseexp_HRs_restrictedrange(baseline_rates, HRs, lower_time_bounds = NULL, 
    upper_time_bounds = NULL, time_max, prefix = NA)}
\arguments{
  \item{baseline_rates}{Data frame of rates specifying the baseline hazards
(column name "rate") and changepoints (column name
"times"). Nrow = 1 if using a regular exponential}
  \item{HRs}{Matrix/df of HRs to be applied }
  \item{lower_time_bounds}{Matrix/df of lower time bounds to be applied}
  \item{upper_time_bounds}{Matrix/df of upper time bounds to be applied}
  \item{time_max}{Maximum time allowed. The rate of the terminal interval
is assumed to be constant until time_max}
  \item{prefix}{Prefix to apply to the sim number for column names}
}

\value{Matrix of random deviates of the same dimension as the
HRs matrix}

\author{Jeanette Birnbaum & Leslie Mallinger}





