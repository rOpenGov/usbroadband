\name{funding}
\alias{funding}
\title{Funding Data}
\description{Broadband (BIP and BTOP) funding data by state.}
\usage{
funding(states, type='bip', ...)
}
\arguments{
  \item{states}{A character string containing either \dQuote{nation} or up to ten state names xor numerical identification numbers, for example as returned by \code{\link{geography}}.}
  \item{type}{A character string containing either \dQuote{bip} (the default) or \dQuote{btop}.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
\details{This function can retrieve data about state-level funding received from the Broadband Initiatives Program (BIP) and Broadband Technologies Opportunities Program (BTOP).}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/bip-funding-api-by-state-name}{BIP funding by state name}
\href{http://www.broadbandmap.gov/developer/api/bip-funding-api-by-state-id}{BIP funding by state ID}
\href{http://www.broadbandmap.gov/developer/api/btop-funding-api-by-state-name}{BTOP funding by state name}
\href{http://www.broadbandmap.gov/developer/api/btop-funding-api-by-state-id}{BTOP funding by state ID}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}



