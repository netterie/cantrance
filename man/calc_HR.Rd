\name{calc_HR}
\alias{calc_HR}
\title{Calculate total HR based on covariate HRs}
\description{Matches covariate HRs to each person's
covariates, and multiples to calculate the total HR. The
input data structures are a bit particular. Expects
multiple simulations.}
\usage{calc_HR(data, bootrows, extra_data = NULL, extra_data_as_is = NULL, 
    extra_data_name = NULL, extra_data_bootrows = NULL, covar_list, 
    age_in_data_not_extra_data = TRUE)}
\arguments{
  \item{data}{Matrix/df of data with observations as rows, and 
columns containing covariate data}
  \item{bootrows}{Matrix/df of row indicators that can be applied to
the data to recover different bootstraps of the
data. Each column is a different bootstrap of the
data.}
  \item{extra_data}{
}
  \item{extra_data_as_is}{
}
  \item{extra_data_name}{Name of the data described in extra_data
Usually, age will be in 'data'. In special
cases it may be contained in 'extra_data' instead, 
in which case set extra_data_name to 'age'. This
is very important for when covar_list contains 
agegroups.}
  \item{extra_data_bootrows}{
}
  \item{covar_list}{List of which each element is a data frame
containing covariate combinations and the 
corresponding HR}
  \item{age_in_data_not_extra_data}{
}
}

\value{A matrix of total HRs for each observation under each
simulation. Thus, has the same dimensions as the input
matrix "bootrows"}

\author{Jeanette Birnbaum & Leslie Mallinger}





