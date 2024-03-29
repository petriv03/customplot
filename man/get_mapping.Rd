% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customplot.R
\name{get_mapping}
\alias{get_mapping}
\title{Get Mapping}
\usage{
get_mapping(x, y, color, shape, size, label)
}
\arguments{
\item{x}{name of the column containing x-coordinates; character string.
The column must be factor or numeric.}

\item{y}{name of the column containing y-coordinates; character string.
The column must be factor or numeric.}

\item{color}{name of the column for point color values; character string.
The column must be factor or numeric.}

\item{shape}{name of the column for point shape values; character string.
The column must be numeric.}

\item{size}{name of the column for point size values; character string.
The column must be numeric.}

\item{label}{name of the column for point labels; character string. The
column must be numeric.}
}
\description{
Returns gg aes() containing the proportions of a ggplot.
}
\keyword{internal}
