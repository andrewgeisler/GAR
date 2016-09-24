\name{getSegments}
\alias{getSegments}

\title{
Retrieves Account Segment Data from the Google Analytics Management API.
}
\description{

This function retrieves segment resources, each of which describes one of the user's default advanced segments or custom segments.

}
\usage{
getSegments(token)
}

\arguments{
  \item{token}{Access Token. String format.}
}
\details{
Function returns a dataframe.
}
\value{
  \item{segmentName}{Segment name.}
  \item{segmentId}{Segment ID.}
  \item{definition}{Segment definition.}
  \item{type}{Type for a segment. Possible values are "BUILT_IN" or "CUSTOM".}
}
\references{
https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/segments
}
\author{
Andrew Geisler
}

\seealso{
\code{\link[GAR]{GAR}}
\code{\link[GAR]{gaRequest}}
\code{\link[GAR]{getCode}}
\code{\link[GAR]{getRefresh}}
\code{\link[GAR]{tokenRefresh}}
\code{\link[GAR]{getMeta}}
}
\examples{

getSegments(
  token="ABCDEFG"
)
}

