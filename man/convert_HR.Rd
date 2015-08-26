\name{convert_HR}
\alias{convert_HR}
\title{Convert list of covariate-specific survival statistics into HRs in relation to baseline survival curve}

\usage{convert_HR(lst = NULL, surv_param, surv_k = NA, baseline_rate)}
\arguments{
  \item{lst}{List of which each element is a data frame
containing covariate combinations and the
corresponding survival statistics or HRs}
  \item{surv_param}{String specifying the type of survival parameter
provided in lst. May be "rate", "median", "mean",
or "ksurv".}
  \item{surv_k}{If "surv_param" is "ksurv", give time for k-time survival}
  \item{baseline_rate}{Rate against which to compare to determine hazard ratio}
}

\value{List of which each element is a data frame containing
covariate combinations and the corresponding HR}

\author{Jeanette Birnbaum & Leslie Mallinger}





