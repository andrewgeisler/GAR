\name{getProfiles}
\alias{getProfiles}

\title{
Retrieves profile data from the Google Analytics Management API.
}
\description{
The function returns a set of Profile resources, each of which describes the profile of an authenticated user. 
}
\usage{
getProfiles(account_id, property_id, token)
}

\arguments{
  \item{account_id}{Required. Account Id (string).}
  \item{property_id}{Required. Account Id (string).}
  \item{token}{Require. Access Token (string).}
}
\details{
Function returns a dataframe.
}

\value{
\item{profileName}{Name of this view (profile).}
\item{profileId}{View (Profile) ID.	}
\item{accountId}{Account ID to which this view (profile) belongs}
\item{propertyId}{Internal ID for the web property to which this view (profile) belongs.}
\item{propertyUrl}{Website URL for this view (profile).}
\item{currency}{The currency type associated with this view (profile), defaults to USD.}
\item{timezone}{Time zone for which this view (profile) has been configured. }
\item{type}{}
\item{ecommerce}{View (Profile) type. Supported types: WEB or APP.}
\item{siteSearchParameter}{The site search query parameters for this view (profile).}
\item{siteSearchCategoryParameter}{Site search category parameters for this view (profile).}
\item{created}{Time this view (profile) was created.}
\item{updated}{Updates an existing view (profile).}
}

\references{
https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/filters
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

getProfiles(
  account_id = "ABCDEFG",
  property_id = "ABCDEFG",
  token = "ABCDEFG"
)

}

