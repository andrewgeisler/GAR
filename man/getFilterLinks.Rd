\name{getFilterLinks}
\alias{getFilterLinks}

\title{
Retrieves filter links data from the Google Analytics Management API.
}
\description{
This function returns a set of Profile Filter Link resources, each of which describes the link between a View (Profile) and a Filter.
}
\usage{
getFilterLinks(account_id, property_id, profile_id, token)
}

\arguments{
  \item{account_id}{Required. Account Id (string).}
  \item{property_id}{Required. Account Id (string).}
  \item{profile_id}{Required. Account Id (string).}
  \item{token}{Require. Access Token (string).}
}
\details{
Function returns a dataframe.
}

\value{
\item{accountId}{Account ID to which this view (profile) belongs.}
\item{propertyId}{Internal ID for the web property to which this view (profile) belongs.}
\item{profileId}{View (Profile) ID.}
\item{profileName}{Name of this view (profile).}
\item{filterRank}{The rank of this profile filter link relative to the other filters linked to the same profile. For readonly (i.e., list and get) operations, the rank always starts at 1.}
\item{filterId}{Filter ID.}
\item{filterName}{Kind value for filter reference.}
}
\references{
https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/profileFilterLinks
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

getFilterLinks(
  account_id = "ABCDEFG", 
  property_id = "ABCDEFG",
  profile_id = "ABCDEFG", 
  token = "ABCDEFG"
)

}

