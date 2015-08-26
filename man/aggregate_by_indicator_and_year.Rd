\name{aggregate_by_indicator_and_year}
\alias{aggregate_by_indicator_and_year}
\title{Aggregate a matrix by levels of an indicator matrix separately by year}
\description{Summarize matrix columns by levels of an
indicator variable that is also specified in a matrix,
because its levels are different for different columns,
but do so BY YEAR. This assumes that the values in the
data matrix are life-years, and that the user wants to
separate out the summary by year}
\usage{aggregate_by_indicator_and_year(data, column_name = "Column", 
    indicator, indicator_name = "Indicator", fun, timehorizon)}
\arguments{
  \item{data}{Matrix of values to be summarized}
  \item{column_name}{What do the columns of the data represent?}
  \item{indicator}{Matrix of indicator values. Can be character strings}
  \item{indicator_name}{What is the indicator?}
  \item{fun}{Summary function, e.g. "sum", "mean"}
  \item{timehorizon}{What is the time horizon? Only years between time=0
and this number will be summarized; years beyond
will be discarded}
}

\value{A data frame where years are columns and values are the
summary statistic, by indicator and column # (in the
original data)}

\author{Jeanette Birnbaum & Leslie Mallinger}





