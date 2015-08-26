\name{calc_surv_stat}
\alias{calc_surv_stat}
\title{Calculate survival statistic based on simulated data}
\description{Matches person-years lived and event status
to each individual's covariates, then calculates the
corresponding survival statistic. Expects multiple
simulations and returns mean, median, 2.5\%, and 97.5\%
quantiles across simulations.}
\usage{calc_surv_stat(data, bootrows, extra_data = NULL, extra_data_as_is = NULL, 
    extra_data_name = NULL, extra_data_bootrows = NULL, covar_list, 
    ptime, status, statistic, k = NA, t_units = NA, stage_shift = FALSE, 
    rx_name = "Treatment")}
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
  \item{covar_list}{List of which each element is a data frame
containing covariate combinations and the 
corresponding HR}
  \item{ptime}{Matrix/df of same dimensions as bootrows. Contains
data on the person-time at risk of the event of
interest}
  \item{status}{Matrix/df of same dimensions as bootrows. Contains
binary data on event status}
  \item{statistic}{String specifying the type of survival statistic to
be reported}
  \item{k}{If "statistic" is "ksurv", give time for k-time
survival}
  \item{t_units}{If "statistic" is "ksurv", give time units for
k-time survival}
  \item{stage_shift}{If stage needs to be shifted to scr_stage among 
screen-detected individuals. Very specific to
the screening model for lack of better options
right now.}
  \item{rx_name}{Name for "rx" variable in output table.}
}

\value{A data frame summarizing the desired survival statistic
for all individuals, individuals grouped by treatment
status,and individuals grouped by covariates as
designated in "covar_list". }

\author{Jeanette Birnbaum & Leslie Mallinger}





