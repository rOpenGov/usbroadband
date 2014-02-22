\name{county_availability}
\alias{county_availability}
\title{County Broadband Availability}
\description{Return data about county broadband availability}
\usage{
county_availability(state=NULL, county=NULL, version=NULL, ...)
}
\arguments{
  \item{state}{}
  \item{county}{}
  \item{version}{A data version, currently one of \dQuote{dec2012}, \dQuote{jun2012}, \dQuote{dec2011}, and \dQuote{jun2011}.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/county-broadband-availability-api-nation}{Broadband availability data, national}
\href{http://www.broadbandmap.gov/developer/api/county-broadband-availability-api-search-by-county-fips-code}{Broadband availability data by county FIPS code}
\href{http://www.broadbandmap.gov/developer/api/county-broadband-availability-api-search-by-county-name}{Broadband availability data by county name}
\href{http://www.broadbandmap.gov/developer/api/county-broadband-availability-api-search-by-state-fips-code}{Broadband availability data by state FIPS code}
\href{http://www.broadbandmap.gov/developer/api/county-broadband-availability-api-search-by-state-name}{Broadband availability data by state name}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}
