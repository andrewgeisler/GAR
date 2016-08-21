\name{getFilters}
\alias{getFilters}

\title{
Retrieves filter data from the Google Analytics Management API.
}
\description{
The function returns a set of filter resources, each of which describes a filter which can be applied to a View (profile).
}
\usage{
getFilters(account_id,token)
}

\arguments{
  \item{account_id}{Required. Account Id (string).}
  \item{token}{Require. Access Token (string).}
}
\details{
Function returns a dataframe.
}

\value{
\item{accountId}{Account ID to which this filter belongs.}
\item{filterName}{Name of this filter.}
\item{filterId}{Filter ID.}
\item{kind}{Resource type for Analytics filter.}
\item{selfLink}{Link for this filter.}
\item{type}{Type of this filter. Possible values are INCLUDE, EXCLUDE, LOWERCASE, UPPERCASE, SEARCH_AND_REPLACE and ADVANCED.}
\item{parentLink}{Link to the account to which this filter belongs.}
\item{created}{Time this filter was created.}
\item{updated}{Time this filter was last modified.}
\item{details}{Filter details}
}
\references{
https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/filters
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

getFilters(
  account_id = "ABCDEFG",
  token = "ABCDEFG"
)

}

