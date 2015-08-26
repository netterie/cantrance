\name{merge_preserve_sort}
\alias{merge_preserve_sort}
\title{Merges two data frames, preserving the original sort of the 1st data frame specified}
\description{This seems silly, but the base merge function 
in R cannot consistently preserve the sort order, even with 
sort=TRUE (see top R help tickets when you Google this 
for more detail). Uses merge() with all.x=TRUE}
\usage{merge_preserve_sort(x, y, mergevars = NA, byx = NA, byy = NA)}
\arguments{
  \item{x}{Data frame in order wished to be preserved. All rows will be kept}
  \item{y}{Data frame to merge in. Non-merged rows will be deleted}
  \item{mergevars}{If merge variables are common to both datasets, vector of column names}
  \item{byx}{If merge variables are not named the same in both datasets, variable to merge on in x}
  \item{byy}{If merge variables are not named the same in both datasets, variable to merge on in y}
}

\value{Merged data frame with same number of rows and sort as x}

\author{Jeanette Birnbaum & Leslie Mallinger}





