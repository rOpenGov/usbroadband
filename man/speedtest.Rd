\name{speedtest}
\alias{speedtest}
\title{Broadband Speed test codes by name or ID}
\description{Return speed test information by geography name or ID}
\usage{
speedtest(geographyType, geographyId=NULL, geographyName=NULL, type=NULL, ...)
}
\arguments{
  \item{geographyType}{A character string containing an allowed geography type: \dQuote{censusplace}, \dQuote{county}, \dQuote{msa}, \dQuote{usf}, \dQuote{statesenate}, \dQuote{statehouse}, \dQuote{congdistrict}, \dQuote{tribalnation}.}
  \item{geographyId}{A numeric or character value specifying a geography ID for the specified \code{geographyType}, possibly as returned by \code{\link{geography}}.}
  \item{geographyName}{A character string containing a place name for the specified \code{geographyType}.}
  \item{type}{One of \dQuote{any}, \dQuote{oomla}, and \dQuote{mlab}.}
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
\examples{
speedtest('county', geographyName='Hennepin')
}

