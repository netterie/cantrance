\name{cancer_death}
\alias{cancer_death}
\title{Determine time from study start to cancer death by adding times to intermediate outcomes}
\description{Adds times from study segments to calculate
time from start of study to death from cancer}
\usage{cancer_death(data, prefixes_in, prefix_out = "ttcd", units = "years", 
    agevar = "age", time = FALSE)}
\arguments{
  \item{data}{Matrix of times to event, encompassing all
intermediate endpoints between study start and
cancer death, with columns named with prefixes
according to segment}
  \item{prefixes_in}{Vector of prefixes that identify event segments in
input data}
  \item{prefix_out}{Prefix for columns in output matrix}
  \item{units}{Time units: one of "years", "months", "days", or
"hours"}
  \item{agevar}{Name of the age variable OR a matrix of ages}
  \item{time}{If age at death is preferable to time from study
start to death, choose time=FALSE}
}

\value{Matrix of times to death or ages at death}

\author{Jeanette Birnbaum & Leslie Mallinger}





