\name{calc_one_future_cost}
\alias{calc_one_future_cost}
\title{Apply discounting to calculate a one-time future cost}
\description{Calculate the discounted value of a one-time
future cost by specifying its present value, the time in
the future it will occur, and the discount rate. }
\usage{calc_one_future_cost(times, timehorizon, cost, cost_name = "Cost", 
    discount)}
\arguments{
  \item{times}{Matrix of times at which the event occurs}
  \item{timehorizon}{Specify time past which cost does not matter}
  \item{cost}{Cost of event}
  \item{cost_name}{Describe what cost is being calculated}
  \item{discount}{Discount rate specified as a proportion, e.g. 3\% is
0.03}
}

\value{Data frame of discounted costs; each row is the column
sum of discounted costs in the original matrix}

\author{Jeanette Birnbaum & Leslie Mallinger}





