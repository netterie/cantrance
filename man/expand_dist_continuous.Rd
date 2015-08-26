\name{expand_dist_continuous}
\alias{expand_dist_continuous}
\title{Expand a distribution of continuous covariates into a full data frame}
\description{Expand a data frame with continuous
distribution of factor covariates into a data frame with
the desired number of rows, with covariates distributed 
according to a truncated normal with the mean, SD, }
\usage{expand_dist_continuous(name, varmean, varsd, varmin, varmax, 
    rows)}
\arguments{
  \item{name}{Name of covariate(s) to be expanded}
  \item{varmean}{Mean value for covariate distribution}
  \item{varsd}{Standard deviation for covariate distribution }
  \item{varmin}{Minimum value for covariate distribution}
  \item{varmax}{Maximum value for covariate distribution}
  \item{rows}{Number of rows in output data frame}
}

\value{A data frame}

\author{Jeanette Birnbaum & Leslie Mallinger}





