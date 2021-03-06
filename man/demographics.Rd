\name{demographics}
\alias{demographics}
\title{Demographics information}
\description{Return demographic information by a long/lat, state, or other geography name}
\usage{
demographics(geographyType=NULL, geographyId=NULL, geographyName=NULL, lat=NULL, long=NULL, version=NULL, all=FALSE, ...)
}
\arguments{
  \item{geographyType}{A character string containing an allowed geography type: \dQuote{censusplace}, \dQuote{county}, \dQuote{msa}, \dQuote{usf}, \dQuote{statesenate}, \dQuote{statehouse}, \dQuote{congdistrict}, \dQuote{tribalnation}.}
  \item{geographyId}{A numeric or character value specifying a geography ID for the specified \code{geographyType}, possibly as returned by \code{\link{geography}}.}
  \item{geographyName}{A character string containing a place name for the specified \code{geographyType}.}
  \item{lat}{A latitude}
  \item{long}{A longitude}
  \item{version}{A data version, currently one of \dQuote{dec2012}, \dQuote{jun2012}, \dQuote{dec2011}, and \dQuote{jun2011}.}
  \item{all}{A boolean indicating whether, if a place borders two geographies, to return information about all geographies.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/demographics-api-by-coordinates}{Demographics data by coordinates}
\href{http://www.broadbandmap.gov/developer/api/demographics-api-by-geography-type-and-geography-id}{Demographics data by geography ID}
\href{http://www.broadbandmap.gov/developer/api/demographics-api-by-geography-type-and-geography-name}{Demographics data by geography name}
\href{http://www.broadbandmap.gov/developer/api/demographics-api-nation}{Demograhpics data, national}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}
