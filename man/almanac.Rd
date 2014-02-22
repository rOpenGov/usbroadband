\name{almanac}
\alias{almanac}
\alias{almanac_parameters}
\title{Almanac API}
\description{Almanac API}
\usage{
almanac_parameters(...)

almanac(...)
}
\arguments{
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



