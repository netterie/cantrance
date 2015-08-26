\name{calc_one_future_cost_indicator}
\alias{calc_one_future_cost_indicator}
\title{Apply discounting to calculate a one-time future cost}
\description{Calculate the discounted value of a one-time
future cost by specifying its present value, the time in
the future it will occur, and the discount rate. Cost can
differ by indicator}
\usage{calc_one_future_cost_indicator(times, zero_is_zero_cost = FALSE, 
    indicator, timehorizon, costs, cost_name = "Cost", discount)}
\arguments{
  \item{times}{Matrix of times at which the event occurs}
  \item{zero_is_zero_cost}{Are zeros in the times matrices meant to mean
discount = 0? If so, set to false. If zeros are
meant to mean zero cost, set to TRUE}
  \item{indicator}{Matrix of indicator values}
  \item{timehorizon}{Specify time past which cost does not matter}
  \item{costs}{Data frame with columns of 1)indicator 2)costs }
  \item{cost_name}{Describe what cost is being calculated}
  \item{discount}{Discount rate specified as a proportion, e.g. 3\% is
0.03}
}

\value{Data frame of discounted costs; each row is the column
sum of discounted costs in the original matrix}

\author{Jeanette Birnbaum & Leslie Mallinger}





