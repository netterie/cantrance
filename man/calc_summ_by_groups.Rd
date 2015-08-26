\name{calc_summ_by_groups}
\alias{calc_summ_by_groups}
\title{Summarize data by groups}

\usage{calc_summ_by_groups(data, bootrows, extra_data = NULL, extra_data_as_is = NULL, 
    extra_data_name = NULL, extra_data_bootrows = NULL, summ_group, 
    summ_varname)}
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
  \item{extra_data_name}{Name of the data described in extra_data}
  \item{extra_data_bootrows}{
}
  \item{summ_group}{String containing the column name with groups by
which to summarize}
  \item{summ_varname}{String containing the column name for the variable
to summarize}
}

\value{A table summarizing mean, 2.5\%, and 97.5\% quantiles of
summ_varname across simulations, grouped by summ_group}

\author{Jeanette Birnbaum & Leslie Mallinger}





