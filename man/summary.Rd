\name{bbsummary}
\alias{bbsummary}
\title{Broadband summary}
\description{Broadband summary by geography ID}
\usage{
bbsummary(geographyType, geographyId=NULL, metric, version=NULL, ...)
}
\arguments{
  \item{geographyType}{A character string containing an allowed geography type: \dQuote{censusplace}, \dQuote{county}, \dQuote{msa}, \dQuote{usf}, \dQuote{statesenate}, \dQuote{statehouse}, \dQuote{congdistrict}, \dQuote{tribalnation}.}
  \item{geographyId}{A numeric or character value specifying a geography ID for the specified \code{geographyType}, possibly as returned by \code{\link{geography}}.}
  \item{metric}{One of \dQuote{population} or \dQuote{household}.}
  \item{version}{A character string containing a data version, currently one of \dQuote{dec2012}, \dQuote{jun2012}, \dQuote{dec2011}, and \dQuote{jun2011}.}
  \item{\dots}{Additional arguments passed to \code{curlPerform}.}
}
%\details{}
\value{A list containing the API response, transformed by \code{fromJSON}.}
\references{
\href{http://www.broadbandmap.gov/developer/api/broadband-summary-api-nation}{Broadband summary, national}
\href{http://www.broadbandmap.gov/developer/api/broadband-summary-api-by-geography-type-and-geography-id}{Broadband summary by geography ID}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
%\examples{}



