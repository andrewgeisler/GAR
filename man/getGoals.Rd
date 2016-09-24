\name{getGoals}
\alias{getGoals}

\title{
A Goals resource describes a goal for one of a user's profiles.
}
\description{
This function returns a set of Goals for a profile that a user has access too.
}
\usage{
getGoals(account_id, property_id, profile_id, token)
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
  \item{accountId}{Account ID to which this goal belongs.}
  \item{propertyId}{Web property ID to which this goal belongs. The web property ID is of the form UA-XXXXX-YY.}
  \item{profileId}{	View (Profile) ID to which this goal belongs.}
  \item{goalName}{Goal name.}
  \item{goalValue}{Goal value.}
  \item{goalActive}{Determines whether this goal is active.}
  \item{goalType}{Goal type. Possible values are URL_DESTINATION, VISIT_TIME_ON_SITE, VISIT_NUM_PAGES, and EVENT.}
  \item{created}{Time this goal was created.}
  \item{updated}{Time this goal was last modified.}
  \item{urlDestinationUrl}{URL for this goal.}
  \item{caseSensitive}{Determines if the goal URL must exactly match the capitalization of visited URLs.}
  \item{urlDestinationMatchType}{Match type for the goal URL. Possible values are HEAD, EXACT, or REGEX.}
  \item{urlDestinationfirstStepRequired}{Determines if the first step in this goal is required.}
  \item{visitTimeOnSiteComparisonType}{Type of comparison. Possible values are LESS_THAN or GREATER_THAN.}
  \item{visitTimeOnSiteComparisonValue}{Value used for this comparison.	}
  \item{visitNumPagesComparisonType}{Type of comparison. Possible values are LESS_THAN, GREATER_THAN, or EQUAL.}
  \item{visitNumPagesComparisonValue}{Value used for this comparison.	}
  \item{eventDetailsValue}{	Determines if the event value should be used as the value for this goal.}
  \item{eventDetailsType}{Type of this event condition. Possible values are CATEGORY, ACTION, LABEL, or VALUE.}
  \item{eventDetailsMatchType}{Type of the match to be performed. Possible values are REGEXP, BEGINS_WITH, or EXACT.}
  \item{eventDetailsExpression}{Expression used for this match.}
  \item{eventDetailsComparisonValue}{Value used for this comparison.}
}
\references{
https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/goals
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

getGoals(
  account_id = "ABCDEFG", 
  property_id = "ABCDEFG",
  profile_id = "ABCDEFG", 
  token = "ABCDEFG"
)

}

