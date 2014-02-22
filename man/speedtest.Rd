\name{speedtest}
\alias{speedtest}
\title{Broadband Speed test codes by name or ID}
\description{Return speed test information by geography name or ID}
\usage{
speedtest(geographyType, geographyId=NULL, geographyName=NULL, testType=NULL, ...)
}
\arguments{
  \item{geographyType}{}
  \item{geographyId}{}
  \item{geographyName}{}
  \item{testType}{One of \dQuote{any}, \dQuote{oomla}, and \dQuote{mlab}.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/speed-test-api-nation}{Broadband speed test, national}
\href{http://www.broadbandmap.gov/developer/api/speed-test-api-minimum-and-maximum-quartile-speeds-by-geography-type}{Broadband speed test by geography type}
\href{http://www.broadbandmap.gov/developer/api/speed-test-api-by-geography-type-and-geography-id}{Broadband speed test by geography ID}
\href{http://www.broadbandmap.gov/developer/api/speed-test-api-by-geography-type-and-geography-name}{Broadband speed test by geography name}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}



