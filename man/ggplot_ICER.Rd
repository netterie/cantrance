\name{ggplot_ICER}
\alias{ggplot_ICER}
\title{Plot ICERs on a 4-quadrant cost-effectiveness plane}
\description{Takes the results of calc_ICER and plots the
ICERs on a 4-quadrant cost-effectiveness plane,
emphasizing the quadrants where the data lie}
\usage{ggplot_ICER(icerdata, xaxis = "QALY", thresholds = c(0, 50000, 
    1e+05))}
\arguments{
  \item{icerdata}{Data frame with columns "CostDiff" and either
"LYDiff" or "QALYDiff"}
  \item{xaxis}{Choose either "QALY" or "LY" for the x-axis.
Corresponding variable must be in the icerdata data
frame}
  \item{thresholds}{Thresholds to indicate in Quad 1 on the
cost-effectiveness plane}
}

\value{A ggplot of the cost-effectiveness plane}

\author{Jeanette Birnbaum & Leslie Mallinger}





