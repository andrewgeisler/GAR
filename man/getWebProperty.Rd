\name{getWebProperty}
\alias{getWebProperty}

\title{
Retrieves Web Property Data from the Google Analytics Management API.
}
\description{
This function retrieves Webproperty resources, each of which describes a web property available to an authenticated user. 
}
\usage{
getWebProperty(account_id, token)
}

\arguments{
  \item{account_id}{Required. Account Id (string).}
  \item{token}{Access Token. String format.}
}
\details{
Function returns a dataframe.
}
\value{
\item{propertyName}{Web property name}
\item{id}{Web property ID of the form UA-XXXXX-YY.}
\item{accountId}{Account ID to which this web property belongs.}
\item{propertyId}{Internal ID for this web property.}
\item{propertyUrl}{Website url for this web property}
\item{level}{Level for this web property.}
\item{profileCount}{View (Profile) count for this web property.}
\item{defaultProfileId}{Default view (profile) ID.}
\item{created}{Time this web property was created.}
\item{updated}{Time this web property was last modified.}



}
\references{
https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/webproperties
}
\author{
Andrew Geisler
}

\seealso{
\code{\link[GAR]{GAR}}
\code{\link[GAR]{getCode}}
\code{\link[GAR]{getRefresh}}
\code{\link[GAR]{tokenRefresh}}
\code{\link[GAR]{getAccountSummary}}
\code{\link[GAR]{getFilterLinks}}
\code{\link[GAR]{getFilters}}
\code{\link[GAR]{getGoals}}
\code{\link[GAR]{getProfiles}}
\code{\link[GAR]{getWebProperty}}
\code{\link[GAR]{getSegments}}
}
\examples{

getWebProperty(
  account_id = "ABCDEFG",
  token="ABCDEFG"
)
}

