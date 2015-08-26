\name{data_info}
\alias{data_info}
\title{Collects some basic information about the user data}
\description{Tailored to examples (e) and (d) and needs
to be generalized}
\usage{data_info(data, covars = NULL, tabvars = NULL, rx.order = unique(survival_specs$rx))}
\arguments{
  \item{data}{Data frame}
  \item{covars}{Names of continuous variables in the data frame.
Default used to be ttr_covars}
  \item{tabvars}{Names of categorical variables }
  \item{rx.order}{Order of comparison (treatment) groups}
}

\value{List of: 1) person-time or contingency table 2) N/mean}

\author{Jeanette Birnbaum & Leslie Mallinger}





