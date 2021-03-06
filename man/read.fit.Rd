% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.fit.R
\name{read.fit}
\alias{read.fit}
\title{Read a ADMB fit file}
\usage{
read.fit(ifile)
}
\arguments{
\item{ifile}{list of ADMB files after convergence}
}
\value{
list of ADMB fits
}
\description{
\code{read.fit} returns the sum of all the values present in its arguments.
}
\details{
This is a generic function: methods can be defined for it directly or via the
\code{\link{read.psv}} group generic. For this to work properly, the
arguments \code{...} should be unnamed, and dispatch is on the first
argument.
}
\seealso{
\code{\link{prod}} for products, \code{\link{cumsum}} for cumulative
  sums, and \code{\link{colSums}}/\code{\link{rowSums}} marginal sums over 
  high-dimensional arrays.

Other aggregate functions: \code{\link{read.admb}},
  \code{\link{read.psv}}, \code{\link{read.rep}}
}
