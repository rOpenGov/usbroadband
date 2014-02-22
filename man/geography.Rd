\name{geography}
\alias{geography}
\title{Geography codes by name or ID}
\description{Return geographical names and codes by geography name or ID}
\usage{
geography(geographyType, geographyId=NULL, geographyName=NULL, fips=NULL, all=NULL, ...)
}
\arguments{
  \item{geographyType}{}
  \item{geographyId}{}
  \item{geographyName}{}
  \item{fips}{}
  \item{all}{A logical indicating whether to return all results.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
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



