\name{rbind_list}
\alias{rbind_list}
\title{rbinds a list of lists and row indicators}
\description{Takes in a list (length i) OR a list (length i) 
of lists (length j) and returns a data frame or a list of size j with the i elements rbinded. 
Does the same for corresponding row indicators, such that
the row indicators correctly correspond to the newly rbinded
list elements. 
Assumes, of course, that each of the j elements are data frames or 
matrices who have the same columns across i elements}
\usage{rbind_list(lst, rows = FALSE, nesting = FALSE)}
\arguments{
  \item{lst}{List OR list of lists}
  \item{rows}{Does lst contain row indicators?}
  \item{nesting}{TRUE if lst is a list of lists, FALSE if it is one list}
}



\author{Jeanette Birnbaum & Leslie Mallinger}





