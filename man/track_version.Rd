\name{track_version}
\alias{track_version}
\title{Creates or appends to a "version info" file}
\description{Takes in a small data frame containing
version information and either writes it as a new file or
appends to existing file. }
\usage{track_version(version_info, filename)}
\arguments{
  \item{version_info}{A data frame that must have a column called "version"
in which a unique identifier is stored (can be
alphanumeric). Example:
vinfo = data.frame(version="edrn_1", description=
"Linear hazard, varying rules")}
  \item{filename}{Name of .csv file to write or append to}
}

\value{No return value}

\author{Jeanette Birnbaum & Leslie Mallinger}





