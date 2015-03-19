\name{getRefresh}
\alias{getRefresh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieves a Google Analytics refresh token using a pre-retrieved and authenciated code. A refresh token can be used to repeating obtain a new access token without additional manual authentication.
}
\description{
This function retrieves a Google Analytics refresh token using a supplied credentials and code. The function will use GAR_CLIENT_ID, GAR_CLIENT_SECRET environmental variables if present. A refresh token can be traded for an access token repeatedly. As a result, this should only be needed one time unless the refresh token is revoked or lost. The refresh token is stored in an 'GAR_REFRESH_TOKEN' environmental variable.
}
\usage{
 getRefresh(client_id, client_secret, code)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{client_id}{
Client ID found in API Console. String format.
}
  \item{client_secret}{
Client Secret found in API Console. String format.
}
  \item{code}{
Code obtained from the redirect URL provided by the getCode() function. String format.
}
}
\details{
Function returns a GA refresh token.
}
\value{
Refresh token is a string to be used in conjunction with tokenRefresh() function.
}
\references{
https://developers.google.com/analytics/devguides/reporting/core/v3/
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
}
\examples{

getRefresh(
            client_id="ABCDEFG",
            client_secret="HIJKLMNOP",
            code="QRSTUVWX"
            )
}


