\name{providers}
\alias{providers}
\alias{provider_parameters}
\title{Broadband Provider APIs}
\description{Provider API}
\usage{
providers(name=NULL, geographyType=NULL, geographyId=NULL, version=NULL, all=TRUE, ...)

provider_parameters(...)
}
\arguments{
  \item{name}{}
  \item{geographyType}{}
  \item{geographyId}{}
  \item{version}{A character string containing a data version, currently one of \dQuote{dec2012}, \dQuote{jun2012}, \dQuote{dec2011}, and \dQuote{jun2011}.}
  \item{all}{A logical indicating whether, if a place borders two geographies, to return information about all geographies.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/provider-statistics-api-similar-search-parameters}{Provider API Parameters}

\href{http://www.broadbandmap.gov/developer/api/broadband-provider-api-all-providers}{All providers}
\href{http://www.broadbandmap.gov/developer/api/broadband-provider-api-by-provider-name}{By provider name}
\href{http://www.broadbandmap.gov/developer/api/providers-by-geography-api-by-provider-name-within-a-geography}{Providers by geography name}
\href{http://www.broadbandmap.gov/developer/api/providers-by-geography-api-by-provider-name-within-nation}{Providers by geography and name}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}



