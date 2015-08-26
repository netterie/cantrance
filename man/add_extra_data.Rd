\name{add_extra_data}
\alias{add_extra_data}
\title{Add extra columns to a data frame within a simulation environment}

\usage{add_extra_data(dset, edata = NULL, edata_as_is = NULL, edata_name = NULL, 
    edata_bootrows = NULL, sim_num = NULL)}
\arguments{
  \item{dset}{Data frame with data to be added to}
  \item{edata}{Matrix/df to be added to dset}
  \item{edata_as_is}{Logical. If TRUE, columns from edata will be added 
as columns to dset without modification. If FALSE,
edata will be bootstrapped according to rows 
specified in bootrows}
  \item{edata_name}{Column name for edata once it's added to dset}
  \item{edata_bootrows}{Matrix/df wiht row indicators that can be applied to
edata to recover different bootstraps of edata. Each
column is a different bootstrap (sim) of the data.}
  \item{sim_num}{Number of simulation (column) to recreate}
}



\author{Jeanette Birnbaum & Leslie Mallinger}





