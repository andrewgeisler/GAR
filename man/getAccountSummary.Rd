\name{getAccountSummary}
\alias{getAccountSummary}

\title{
Retrieves Account Summary Data from the Google Analytics Management API.
}
\description{
This function retrieves account summaries (comprised of accounts/properties/profiles) to which the user has access.

}
\usage{
getAccountSummary(token)
}

\arguments{
  \item{token}{
Access Token. String format.
}
}
\details{
Function returns a dataframe.
}
\value{
\item{accountName}{Account name}
\item{accountId}{Account ID}
\item{propertyName}{Web property name}
\item{propertyId}{Web property ID of the form UA-XXXXX-YY}
\item{propertyUrl}{Website url for this web property}
\item{profileName}{View (profile) name}
\item{profileId}{View (profile) ID}
\item{profileType}{View (Profile) type. Supported types: WEB or APP}
}
\references{
https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/accountSummaries
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

getAccountSummary(
  token="ABCDEFG"
)
}

