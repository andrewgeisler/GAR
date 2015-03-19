\name{GAR-package}
\alias{GAR-package}
\alias{GAR}
\docType{package}
\title{
Authorize and Request Google Analytics data.
}
\description{

This package is designed to aid in obtaining initial authentication with Google Analytics using the getCode() and getRefresh() functions. 

Once authenticated you will have an refresh token which can be repeated traded for an access token via the tokenRefresh() function. Once traded for an access token, data can be retreived used the gaRequest() function. 

It is recommended to store the client id and client secret in the following system variables: GAR_CLIENT_ID, GAR_CLIENT_SECRET. The getCode(), getRefresh() and tokenRefresh() functions will reference these variables by default. It is also recommended to store the refresh token retreived by getRefresh() in a GA_REFRESH_TOKEN system variable. The tokenRefresh() function will refer to GA_REFRESH_TOKEN by default. 

Additionally, a package environment (envGAR) is created on load and will store the access token produced by tokenRefresh() for use within the session.

}
\details{
\tabular{ll}{
Package: \tab GAR\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2015-03-10\cr
License: \tab R Version 3.1\cr
Depends: \tab httr, jsonlite \cr
}
Package includes four main functions: getCode(), getRefresh(), gaRequest() and tokenRefresh().
}
\author{
Andrew Geisler

Maintainer: Andrew Geisler <andrew.geisler@gmail.com>
}
\references{
https://developers.google.com/analytics/devguides/reporting/core/v3/
}

\keyword{ package }
\seealso{
\code{\link[GAR]{GAR}}
\code{\link[GAR]{gaRequest}}
\code{\link[GAR]{getCode}}
\code{\link[GAR]{getRefresh}}
\code{\link[GAR]{tokenRefresh}}
}
\examples{


getCode(
        client_id="ABCDEFG"
        )

getRefresh(
            client_id="ABCDEFG",
            client_secret="HIJKLMNOP",
            code="QRSTUVWX"
            )

gaRequest(
        id=c('ga:123456789','ga:987654321'), 
        dimensions='ga:date,ga:month', 
        metrics='ga:sessions,ga:users,ga:pageviews', 
        start='YYYY-MM-DD',
        end='YYYY-MM-DDD',
        sort='-ga:sessions,ga:users',
        max='1000',
        filters='ga:browser==VALUE,ga:city==VALUE',
        segment='abcdef',
        token='abcdef'
        )

tokenRefresh(
            client_id="ABCDEFG",
            client_secret="HIJKLMNOP",
            token="QRSTUVWX"
            )

}
