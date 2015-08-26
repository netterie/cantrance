\name{recreate_pinsky_calc}
\alias{recreate_pinsky_calc}
\title{Re-calculate death rate using an approach comparable to that used by Pinsky et al. for REDUCE study}
\description{Matches incidence status and cause-specific
survival status to each individual's covariate of
intereset, then calculates the corresponding death rate.
Expects multiple simulations and returns mean, median, 
2.5\%, and 97.5\% quantiles across simulations.}
\usage{recreate_pinsky_calc(data, bootrows, extra_data = NULL, extra_data_as_is = NULL, 
    extra_data_name = NULL, extra_data_bootrows = NULL, covar_tab, 
    inc_status, css_status)}
\arguments{
  \item{data}{Matrix/df of data with observations as rows, and 
columns containing covariate data}
  \item{bootrows}{Matrix/df of row indicators that can be applied to
the data to recover different bootstraps of the
data. Each column is a different bootstrap of the
data}
  \item{extra_data}{
}
  \item{extra_data_as_is}{
}
  \item{extra_data_name}{Name of the data described in extra_data}
  \item{extra_data_bootrows}{
}
  \item{covar_tab}{Data frame containing covariate strata and desired
order.}
  \item{inc_status}{Matrix/df of same dimensions as bootrows. Contains
binary data on incidence status}
  \item{css_status}{Matrix/df of same dimensions as bootrows. Contains
binary data on cause-specific death status}
}

\value{A data frame with summary statistics for the observed
death rate across simulations, calculated using the same
approach as was used by Pinsky et al.}

\author{Jeanette Birnbaum & Leslie Mallinger}





