\name{calc_costs_QALYs}
\alias{calc_costs_QALYs}
\title{Calculate the cost of treatment in a population}
\description{Takes in additional cost of treatment per
year and computes total cost of treatment based on
life-years on treatment. Discounts if desired.}
\usage{calc_costs_QALYs(lifeyears, timetorecur, treatment, cancerdeath, 
    costutil, costoftest = NULL, discount = 0, timehorizon = NULL)}
\arguments{
  \item{lifeyears}{Matrix of life-years lived after treatment. Rows are
individuals and columns are different simulations}
  \item{timetorecur}{Matrix of times to disease recurrence (net). Rows
are individuals and columns are different simulations}
  \item{treatment}{Matrix of treatment indicators, for each person in
each sim}
  \item{cancerdeath}{Matrix of indicators whether the death was from
cancer (1) or other (0)}
  \item{costutil}{Data frame where treatment indicators (as strings)
are the first column, (relative) costs are the
second column, and utilities are the third, with
column names "Treatment", "Cost", "Utility". If
life-years are preferred to QALYs, use utilities of
1.0.}
  \item{costoftest}{If a test was administered to each individual, input
its cost. It will be multiplied by the number of
individuals and added to the total cost,
undiscounted since it is assumed to be administered
at time 0}
  \item{discount}{Optional discount rate expressed as a PROPORTION not
PERCENT, e.g. input 3\% discount rate as 0.03. Use
discount=0 for no discounting}
  \item{timehorizon}{If discounting, must specify the time frame of the
lifeyears; which should be stored in the variable
time_max_inyears}
}

\value{A data frame of total costs and QALYs for each
simulation}

\author{Jeanette Birnbaum & Leslie Mallinger}





