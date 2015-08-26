\name{calc_ac_lifespan}
\alias{calc_ac_lifespan}
\title{Generates an age at other-cause death from US lifetables}
\description{Given a year of birth and current age, draws 
a random age at other-cause death from US cohort
lifetables. }
\usage{calc_ac_lifespan(age, birth_year, male, n_sim = 100, time = FALSE, 
    haz = 1, lifetable = life_table)}
\arguments{
  \item{age}{Current age}
  \item{birth_year}{Year of birth}
  \item{male}{Sex: male=1 for male, male=0 for female}
  \item{n_sim}{Number of random draws to return}
  \item{time}{Return age at other-cause death, or time from
current age to other-cause death?}
  \item{haz}{Should the US lifetables be modified by a hazard
ratio before drawing from them? If so, the cohort
survival will be raised to this number. }
  \item{lifetable}{Life table dataframe with columns Survival,
BirthCohort, Age, and Male}
}

\value{A vector of length n_sim containing ages at other-cause
death (or times to other-cause death, if time=TRUE)}

\author{Jeanette Birnbaum & Leslie Mallinger}





