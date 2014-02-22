\name{bb}
\alias{bb}
\title{API Query}
\description{Execute an API query}
\usage{
bb(baseurl = "http://www.broadbandmap.gov/broadbandmap/", api, args = NULL, ...)
}
\arguments{
  \item{baseurl}{The base URL for conducting the API query.}
  \item{api}{A character string containing an API operation.}
  \item{args}{A named character string or list containing API query arguments.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{\url{http://www.broadbandmap.gov/developer/}}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}
