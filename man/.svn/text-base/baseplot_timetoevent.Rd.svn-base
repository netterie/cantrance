\name{baseplot_timetoevent}
\alias{baseplot_timetoevent}
\title{Plot parametric fit or KM curve of time to recurrence data}
\description{Fits the standard parametric models to the
data based on covariate and return the plots of the fit.
If no covariates desired, just leave covariates=1. If you
just want to plot the data and leave out the parametric
fit, use distribution=NULL}
\usage{baseplot_timetoevent(data, distribution, covar = 1, group_var = "rx", 
    time_max = NULL, prob_surv_min = 0, covar.order = NULL, empirical_curve = NULL, 
    empirical_curve_color = "red", labels = NULL)}
\arguments{
  \item{data}{Time to event data with covariates "time" and
"status"}
  \item{distribution}{Parametric distribution to overlay. One of
"weibull", "exponential", "gaussian", "logistic",
"lognormal", "loglogistic" OR NULL -- if NULL, will
plot the KM curve }
  \item{covar}{Covariates in the model. Set to 1 if no covariates
desired. ***I'm not sure I tested it with real
covariates; it may not work}
  \item{group_var}{Single covariate in data by which separate KM curves
can be plotted on top of the full KM curve and/or
parametric curve. Set to NULL if there is no grouping
variable of interest}
  \item{time_max}{Maximum time to plot. If NULL, defaults to
max(data$time)}
  \item{prob_surv_min}{What should be the minimum survival probability
for the plot?}
  \item{covar.order}{If covariates are specified, should there be an
order they are displayed in the legend?}
  \item{empirical_curve}{summarize_ecdf 'summ' data frame 
Make sure to set time_max appropriately
given that you are overlaying this curve}
  \item{empirical_curve_color}{Applies if empirical_curve is not NULL}
  \item{labels}{Optional vector of title, yaxis and xaxis labels
IN THAT ORDER}
}

\value{A base package plot}

\author{Jeanette Birnbaum & Leslie Mallinger}





