\name{aggregate_by_indicator}
\alias{aggregate_by_indicator}
\title{Aggregate a matrix by levels of an indicator matrix}
\description{Summarize matrix columns by levels of an 
indicator variable that is also specified in a matrix,
because its levels are different for different columns}
\usage{aggregate_by_indicator(data, indicator, fun, data_name = "x", 
    column_name = "Column", indicator_name = "Indicator", reshape_indicator = FALSE)}
\arguments{
  \item{data}{Matrix of values to be summarized}
  \item{indicator}{Matrix of indicator values. Can be character strings}
  \item{fun}{Summary function, e.g. "sum", "mean"}
  \item{data_name}{What do the data represent?}
  \item{column_name}{What do the columns of the data represent?
Set to 'REMOVE' to remove this column before returning
results}
  \item{indicator_name}{What is the indicator?}
  \item{reshape_indicator}{Set to TRUE to reshape the data such that 
the indicator values are wide}
}

\value{A data frame of the summary statistic, reported by indicator 
level and by column #}

\author{Jeanette Birnbaum & Leslie Mallinger}





