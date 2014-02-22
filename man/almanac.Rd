\name{almanac}
\alias{almanac}
\alias{almanac_parameters}
\title{Geogrpahic Rankings}
\description{Rankings of geographies on requested measures}
\usage{
almanac_parameters(...)

almanac(state, metric, ranking, geographyType, geographyId=NULL,
        properties=NULL, sort='asc', version=NULL, ...)
}
\arguments{
  \item{state}{A character string containing either \dQuote{nation} or one state name xor numerical identification number, for example as returned by \code{\link{geography}}.}
  \item{metric}{One of \dQuote{population} or \dQuote{household}.}
  \item{ranking}{A ranking criterion. The set of available rankings can be retrieved from \code{almanac_parameters}.}
  \item{geographyType}{A character string containing an allowed geography type: \dQuote{censusplace}, \dQuote{county}, \dQuote{msa}, \dQuote{usf}, \dQuote{statesenate}, \dQuote{statehouse}, \dQuote{congdistrict}, \dQuote{tribalnation}.}
  \item{geographyId}{A numeric or character value specifying a geography ID for the specified \code{geographyType}, possibly as returned by \code{\link{geography}}.}
  \item{properties}{Undefined.}
  \item{sort}{One of \dQuote{asc} (the default) or \dQuote{desc}, specifying the ordering of the results.}
  \item{version}{A data version, currently one of \dQuote{dec2012}, \dQuote{jun2012}, \dQuote{dec2011}, and \dQuote{jun2011}.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/almanac-api-parameters}{Almanac API Parameters}
\href{http://www.broadbandmap.gov/developer/api/almanac-api-ranking-by-geography-id-within-a-state}{Almanac rank by geography ID within state}
\href{http://www.broadbandmap.gov/developer/api/almanac-api-ranking-by-geography-id-within-the-nation}{Almanac rank by geography ID within nation}
Almanac rank by geography type only (href{http://www.broadbandmap.gov/developer/api/almanac-api-ranking-by-geography-type-within-a-state}{within state} or \href{http://www.broadbandmap.gov/developer/api/almanac-api-ranking-by-geography-type-within-the-nation}{within nation})
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}



