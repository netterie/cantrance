\name{clinical_incidence}
\alias{clinical_incidence}
\title{Generates times to and ages at incidence}
\description{Simulates onset of clinical disease, based on 
age-specific incidence rates assumed to approximate disease-
specific hazards}
\usage{clinical_incidence(age_at_entry, inc, age_max, nsim, prefix = "sim")}
\arguments{
  \item{age_at_entry}{Matrix of ages at entry (i.e., people have survived
incidence-free until this age)}
  \item{inc}{Data frame with an "agegroup" and "rate" variable that
specifies the incidence rates for each age category}
  \item{age_max}{Maximum age at incidence allowed. Will be used as the
upper draw possible for incidence in the oldest 
age group}
  \item{nsim}{Number of simulations}
  \item{prefix}{Prefix for column names of each simulation}
}

\value{Input population with simulated ages at/times to incidence}

\author{Jeanette Birnbaum & Leslie Mallinger}





