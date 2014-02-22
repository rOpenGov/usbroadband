\name{geography}
\alias{geography}
\title{Geography codes by name or ID}
\description{Return geographical names and codes by geography name or ID}
\usage{
geography(geographyType, geographyId=NULL, geographyName=NULL, fips=NULL, all=NULL, ...)
}
\arguments{
  \item{geographyType}{A character string containing an allowed geography type: \dQuote{censusplace}, \dQuote{county}, \dQuote{msa}, \dQuote{usf}, \dQuote{statesenate}, \dQuote{statehouse}, \dQuote{congdistrict}, \dQuote{tribalnation}.}
  \item{geographyId}{A numeric or character value specifying a geography ID for the specified \code{geographyType}, possibly as returned by \code{\link{geography}}.}
  \item{geographyName}{A character string containing a place name for the specified \code{geographyType}.}
  \item{fips}{A FIPS state code to narrow the search results, possibly as returned by \code{\link{geography}}.}
  \item{all}{A logical indicating whether to return all results.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
\details{This returns basic identifying information about a place, based on \code{geographyType} and either \code{geographyName} or \code{geographyId}, possibly within a state specified by \code{fips}. The response is similar to what is returned by \code{\link{census}}.}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/geography-lookup-api-by-geography-id}{Lookup by geography ID}
\href{http://www.broadbandmap.gov/developer/api/geography-lookup-api-by-geography-type}{Lookup by geography type}
\href{http://www.broadbandmap.gov/developer/api/geography-lookup-api-by-geography-type-and-geography-name}{Lookup by geography name}
\href{http://www.broadbandmap.gov/developer/api/geography-lookup-api-by-geography-type-within-a-state}{Lookup by type, within state}
\href{http://www.broadbandmap.gov/developer/api/geography-lookup-api-by-name-of-specific-geography-type-within-a-state}{Lookup by geography type, within state}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}



