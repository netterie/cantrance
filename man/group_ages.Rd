\name{group_ages}
\alias{group_ages}
\title{Group ages within a data frame to aggregate them into an agegroup variable}
\description{Expects groups to have adjacent integer bounds, 
e.g. 30-34, 35-39, 40-44. Non-integer ages will be rounded
before matching to groups}
\usage{group_ages(ages, groups)}
\arguments{
  \item{ages}{Vector of ages to be grouped }
  \item{groups}{Vector of desired age groups, with ranges designated 
as "agemin-agemax"}
}

\value{A data frame}

\author{Jeanette Birnbaum & Leslie Mallinger}





