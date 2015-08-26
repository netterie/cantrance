\name{bootstrap_pop}
\alias{bootstrap_pop}
\title{Boostrap a population from a data set}
\description{Creates a population by bootstrapping from
the data. For covars_specified, requires a covar_table
data frame with columns "var1", "var2"..."varN", "prop",
where prop indicates the proportion of observations to be
returned for each combination of var1...varN.}
\usage{bootstrap_pop(data = userdat, size = pop_size, how = create_pop_method, 
    covar_table = NULL)}
\arguments{
  \item{data}{Data frame}
  \item{size}{Size of boostrapped population}
  \item{how}{One of "simple_boostrap" (resamples rows randomly)
or "covars_specified," which resamples rows
according to the proportions indicated in 
covar_table}
  \item{covar_table}{Data frame with columns var1...varN, and prop,
where prop indicates the proportion of observations 
to be returned for the row's combination of variable
values.}
}

\value{A data frame of size pop_size}

\author{Jeanette Birnbaum & Leslie Mallinger}





