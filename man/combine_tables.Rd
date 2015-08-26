\name{combine_tables}
\alias{combine_tables}
\title{Combine tables into one using superrows}
\description{Combine separate tables with the same number
of columns into one using superrow indicators}
\usage{combine_tables(table_list, table_IDs = NULL, row_names = NULL)}
\arguments{
  \item{table_list}{List of data frames that have the same number
of columns and column names}
  \item{table_IDs}{Optional vector of string IDs that will become
the superrow indicators for each table. Must 
have the same length as table_list.
If NULL, the names of the list elements will be used}
  \item{row_names}{If some of the tables have meaningful row names,
enter a vector of IDs for the row names, one per
table. Entries should be '' if a table doesn't
have meaningful row names. The other entries MUST
ALL BE THE SAME, AS THIS WILL BECOME A COLUMN HEAD}
}

\value{Data frame of combined tables}

\author{Jeanette Birnbaum & Leslie Mallinger}





