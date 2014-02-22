\name{census}
\alias{census}
\title{Geography information}
\description{Return geography information by a long/lat, state, or other geography name}
\usage{
census(geographyType=NULL, geographyId=NULL, geographyName=NULL, lat=NULL, long=NULL, version=NULL, all=TRUE, ...)
}
\arguments{
  \item{geographyType}{A character string containing an allowed geography type: \dQuote{censusplace}, \dQuote{county}, \dQuote{msa}, \dQuote{usf}, \dQuote{statesenate}, \dQuote{statehouse}, \dQuote{congdistrict}, \dQuote{tribalnation}.}
  \item{geographyId}{A numeric or character value specifying a geography ID for the specified \code{geographyType}, possibly as returned by \code{\link{geography}}.}
  \item{geographyName}{A character string containing a place name for the specified \code{geographyType}.}
  \item{lat}{A latitude}
  \item{long}{A longitude}
  \item{version}{A character string containing a data version, currently one of \dQuote{dec2012}, \dQuote{jun2012}, \dQuote{dec2011}, and \dQuote{jun2011}.}
  \item{all}{A logical indicating whether, if a place borders two geographies, to return information about all geographies.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
\details{This returns basic identifying information about a place, based on \code{geographyType} and either \code{geographyName} or \code{geographyId}, or alternatively by both \code{lat} and \code{long} alone. The response is similar to what is returned by \code{\link{geography}}.}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{Geography data by coordinates}{http://www.broadbandmap.gov/developer/api/census-api-by-coordinates}
\href{Geography data by state FIPS code}{http://www.broadbandmap.gov/developer/api/census-api-by-fips-code}
\href{Geography data by geography name}{http://www.broadbandmap.gov/developer/api/census-api-by-geography-name}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}



