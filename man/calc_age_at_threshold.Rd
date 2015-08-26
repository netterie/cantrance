\name{calc_age_at_threshold}
\alias{calc_age_at_threshold}
\title{Calculate age at which biomarker reaches threshold value}
\description{Uses the specified growth model to calculate age at which biomarker levels reach the given threshold}
\usage{calc_age_at_threshold(age_at_threshold, marker_pre, marker_post, 
    age_at_onset, age_at_inc, marker_method, threshold)}
\arguments{
  \item{age_at_threshold}{Matrix to record ages at threshold}
  \item{marker_pre}{Matrix of pre-onset marker values}
  \item{marker_post}{Matrix of post-onset marker values}
  \item{age_at_onset}{Matrix of ages at disease onset (preclinical incidence)}
  \item{age_at_inc}{Matrix of ages at clinical incidence}
  \item{marker_method}{"stepfxn" 
- marker jumps up to post-onset value 
immediately at onset
"annual_exp" 
- apply an annual exponential growth between 
pre-onset and post-onset values, anchoring the 
post-onset value at clinical incidence
"annual_linear" 
- apply an annual linear growth rate between
pre-onset and post-onset values, anchoring the
post-onset value at clinical incidence}
  \item{threshold}{Biomarker threshold value}
}

\value{Matrix of ages at which biomarker threshold is reached}

\author{Jeanette Birnbaum & Leslie Mallinger}





