\name{extract_treatment}
\alias{extract_treatment}
\title{Return treatment assignment from composite variable}
\description{Given a string variable of form
"testresult-treatmentindicator", extract and return the
treatment indicator}
\usage{extract_treatment(matrix, as.numeric = FALSE)}
\arguments{
  \item{matrix}{Matrix of the string composite indicators}
  \item{as.numeric}{If the treatment indicator is numeric, return a
numeric matrix?}
}

\value{A matrix of treatment indicators}

\author{Jeanette Birnbaum & Leslie Mallinger}





