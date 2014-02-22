\name{institutions}
\alias{institutions}
\title{Community Anchor Institutions}
\description{Return data about community anchor institutions by a long/lat, state, or other geography name}
\usage{
institutions(geographyType=NULL, geographyId=NULL, geographyName=NULL, 
             lat=NULL, long=NULL, version=NULL, ...)
}
\arguments{
  \item{geographyType}{}
  \item{geographyId}{}
  \item{geographyName}{}
  \item{lat}{A latitude}
  \item{long}{A longitude}
  \item{version}{A data version, currently one of \dQuote{dec2012}, \dQuote{jun2012}, \dQuote{dec2011}, and \dQuote{jun2011}.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/community-anchor-institutions-closest-by-latitude-and-longitude}{Institutions by Lat/Long}
\href{http://www.broadbandmap.gov/developer/api/community-anchor-institutions-api-by-geography-type-and-geography-id}{Institutions by Geography ID}
\href{http://www.broadbandmap.gov/developer/api/community-anchor-institutions-api-by-geography-type-and-geography-name}{Institutions by Geography name}
\href{http://www.broadbandmap.gov/developer/api/community-anchor-institutions-api-nation}{Institutions for the nation}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}
