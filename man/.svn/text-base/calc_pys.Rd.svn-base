\name{calc_pys}
\alias{calc_pys}
\title{Summarize person-years saved across simulations}
\description{Calculate expected life-years for each
person and compare total life years across intervention
groups(s) to determine the distribution of life-years
saved (or lost) by the intervention(s) }
\usage{calc_pys(cancer, other, covar, covar.order = NULL, obs = control_group, 
    calc.time, suppress_negative = TRUE, colname = "")}
\arguments{
  \item{cancer}{Matrix of times to main endpoint. Rows are
individuals}
  \item{other}{Matrix of times to competing endpoint. Rows are
individuals}
  \item{covar}{Vector indicating intervention/control groups}
  \item{covar.order}{
}
  \item{obs}{Character string indicating which group is the
control}
  \item{calc.time}{At what time point after study start should the
person-years-saved be calculated?}
  \item{suppress_negative}{Should negative person-years-saved be suppressed
(i.e. don't allow person-years lost?)}
  \item{colname}{Column name for output matrix}
}

\value{A matrix of mean, lower, and upper-bound person-years
saved by treatment group (columns)}

\author{Jeanette Birnbaum & Leslie Mallinger}





