\name{GAR-package}
\alias{GAR-package}
\alias{GAR}
\docType{package}
\title{
Authorize and Request Google Analytics data.
}
\description{
This package has two functions. First, tokenRefresh returns an access token from a provided refresh token. Second, gaRequest builds and requests data from the Google Analytics API using the access token.
}
\details{
\tabular{ll}{
Package: \tab GAR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-02-16\cr
License: \tab R Version 3.1\cr
Depends: \tab httr, jsonlite \cr
}
Package includes two main functions: gaRequest() and tokenRefresh().
}
\author{
Andrew Geisler

Maintainer: Andrew Geisler <andrew.geisler@gmail.com>
}
\references{
https://developers.google.com/analytics/devguides/reporting/core/v3/
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
\code{\link[GAR]{gaRequest}}
\code{\link[GAR]{tokenRefresh}}
}
\examples{
gaRequest(
        id='ga:123456789', 
        dimensions='ga:date,ga:month', 
        metrics='ga:sessions,ga:users,ga:pageviews', 
        start='YYYY-MM-DD',
        end='YYYY-MM-DDD',
        sort='-ga:sessions,ga:users',
        max='1000',
        filters='ga:browser==VALUE,ga:city==VALUE'
        token='abcdef'
        )

tokenRefresh(
            client_id="ABCDEFG",
            client_secret="HIJKLMNOP",
            token="QRSTUVWX"
            )
}
