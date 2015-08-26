\name{sim_KM}
\alias{sim_KM}
\title{Returns random deviates from a survival curve}
\description{Takes in event times and corresponding
survival probs (as from a KM curve) and returns n random
deviates}
\usage{sim_KM(survival, time, smalltimes, bigtimes, nsims, mindraw = 0, 
    maxdraw = 1, draws = NULL)}
\arguments{
  \item{survival}{Vector of survival probabilities, as from a KM curve}
  \item{time}{Vector of event times corresponding to the survival
probabilities}
  \item{smalltimes}{Value to be returned if survival is higher/time is
smaller than any observed. Suggestions: o,
min(times), NA}
  \item{bigtimes}{Value to be returned if survival is lower/time is
higher than any observed. Suggestions: Inf,
max(times), NA}
  \item{nsims}{Number of deviates to return}
  \item{mindraw}{Minimum draw allowed on the survival scale}
  \item{maxdraw}{Maximum draw allowed on the survival scale}
  \item{draws}{If draws on the survival scale have already been
made, specify as a vector here}
}

\value{Times to event representative of the input curve}

\author{Jeanette Birnbaum & Leslie Mallinger}
\note{Uses a linear approximation to interpolate between observed events}




