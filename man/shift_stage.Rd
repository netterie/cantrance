\name{shift_stage}
\alias{shift_stage}
\title{Implement stage shift}
\description{Given a vector with existing stages and
information about the desired distribution within
the 'shifted' population, implement a stage shift}
\usage{shift_stage(stages, start_pop, new_counts, ord)}
\arguments{
  \item{stages}{Vector of stages, in the order denoted by ord}
  \item{start_pop}{Vector in which each entry represents an individual
and denotes the stage in which they started}
  \item{new_counts}{Number of individuals to be assigned to each stage
in the new, stage-shifted population}
  \item{ord}{Numeric vector denoting the order in which stage
progression occurs, where 1=least invasive stage
and max(ord)=most invasive stage. Individuals will
be shifted to a stage that is lower than or equal
to their starting stage}
}

\value{A vector in which each entry represents an individual
and denotes the stage to which they have been shifted}

\author{Jeanette Birnbaum & Leslie Mallinger}





