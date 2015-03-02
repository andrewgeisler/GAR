\name{gaRequest}
\alias{gaRequest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Request data from Google Analytics API 
}
\description{
This function compiles a Google Analytics API request, retrieves the results and outputs a dataframe. Input formats based on documented Google Analytic's Core Reporting API requirements. The function is designed to loop through the same request to multiple GA account ids. The result will still be one data frame. Additionally, the function attemps to output the error message if there is an issue.
}
\usage{
gaRequest(
          id, dimensions, metrics, start, end, 
          token, sort = NA, max = NA, filters = NA
          )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
Required. Profile ID including the ga: prefix. Can be a one or many.
}
  \item{dimensions}{
Required. Parameter can be a single dimension or a string of multiple dimensions separated by commas.
}
  \item{metrics}{
Required. Parameter can be a single metric or a string of multiple metrics separated by commas.
}
  \item{start}{
Required. Start date formated as a string (YYYY-MM-DD).
}
  \item{end}{
Required. End date formated as a string (YYYY-MM-DD).
}
  \item{token}{
Required.
}
  \item{sort}{
Not Required. Can be a single parameter or can be a string of multiple parameters separated by commas.
}
  \item{max}{
Not Required. Google Analytics default is 100 and their max is 10,000.
}
  \item{filters}{
Not Required. Can be a single parameter or can be a string of multiple parameters separated by commas.
}
}
\details{
The function returns a dataframe.
}
\value{
 \item{profileId}{Profile/view ID in request.}
 \item{accountId}{Account ID.}
 \item{webPropertyId}{Property UA number.}
 \item{internalWebPropertyId}{Internal web property ID.}
 \item{profileName}{Name of profile in the request.}
 \item{tableId}{Table ID from Google Analytics.}
 \item{start.date}{Start date of the request.}
 \item{end.date}{End date of the request.}
 \item{dimension(s)}{Each requested dimension will be a separate field in Data Frame.}
 \item{metric(s)}{Each requested metric will be a separate field in Data Frame.}
}
\references{
https://developers.google.com/analytics/devguides/reporting/core/v3/
}
\author{
Andrew Geisler
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[GAR]{GAR}}
\code{\link[GAR]{gaRequest}}
\code{\link[GAR]{getCode}}
\code{\link[GAR]{getRefresh}}
\code{\link[GAR]{tokenRefresh}}
}
\examples{
gaRequest(
        id=c('ga:123456789','ga:987654321'), 
        dimensions='ga:date,ga:month', 
        metrics='ga:sessions,ga:users,ga:pageviews', 
        start='YYYY-MM-DD',
        end='YYYY-MM-DDD',
        sort='-ga:sessions,ga:users',
        max='1000',
        filters='ga:browser==VALUE,ga:city==VALUE',
        token='abcdef'
        )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
