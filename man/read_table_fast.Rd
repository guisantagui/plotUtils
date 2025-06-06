% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_write_fast.R
\name{read_table_fast}
\alias{read_table_fast}
\title{Read table faster}
\usage{
read_table_fast(f, row.names = NULL, sep = "auto", sep2 = "auto")
}
\arguments{
\item{f}{A path to the file to be loaded}

\item{row.names}{Integer. The column number where row names are.}

\item{sep}{Separator between columns. Defaults to \code{"auto"} (automatic).}

\item{sep2}{Separator within columns. If each cell is a vector of values a
list column will be returned. Defaults to \code{"auto"} (automatic).}
}
\value{
A \code{data.frame} with the contents of the file \code{f}.
}
\description{
Wrapper function to data.table::fread, to work as read.table, but faster
}
