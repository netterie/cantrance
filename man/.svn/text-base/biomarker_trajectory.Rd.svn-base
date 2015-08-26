\name{biomarker_trajectory}
\alias{biomarker_trajectory}
\title{Generate biomarker trajectories }
\description{Take in a mean and variance for the starting 
level and proportion change in a biomarker, and 
simulates those values for a population using a specified
distributional assumption normals (can choose regular normals by setting limits of }
\usage{biomarker_trajectory(biodata, nval, nsim = 1, distr = "truncnorm")}
\arguments{
  \item{biodata}{Vector (or data frame) of biomarker properties, with 
(column) names:
"mu" for the distribution mean, 
"sigma" for standard deviation, 
"lower" for the lowest value allowed, and 
"upper" for the highest value allowed. 
"lower" can be -Inf and "upper" can be Inf }
  \item{nval}{Number of simulated values to return
If biodata is a data frame with columns 
storing the parameters for each row/simulation,
nval should be set equal to nrow(biodata)}
  \item{nsim}{If there are multiple simulations being 
generated at once, setting nsim to a larger
number will result in a matrix of size 
nval x nsim being returned, rather than 
a vector of length nval}
  \item{distr}{String specifying the distribution: "truncnorm" for
truncated normal (for a regular normal, choose 
"truncnorm" and set the lower and upper limits to -Inf
and Inf respectively), "lognorm", or "exponential"}
}

\value{Simulated biomarker levels}

\author{Jeanette Birnbaum & Leslie Mallinger}





