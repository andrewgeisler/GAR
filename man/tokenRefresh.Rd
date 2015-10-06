\name{tokenRefresh}
\alias{tokenRefresh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieves a Google Analytics access token using a refresh token. 
}
\description{
This function retrieves a Google Analytics access token using a supplied refresh token. Refresh tokens do not expire and can be traded for an access token repeatedly. The function will pull from the following environmental variables by default: GAR_CLIENT_ID, GAR_CLIENT_SECRET, GAR_REFRESH_TOKEN. Additionally, a temporary variable (GAR_ACCESS_TOKEN) will be stored in a package specific environment (envGAR) containing the newly generated access token for use in the getRequest() function. 
}
\usage{
tokenRefresh(client_id, client_secret, token)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{client_id}{
Client ID found in API Console. String format.
}
  \item{client_secret}{
Client Secret found in API Console. String format.
}
  \item{token}{
Refresh token as originally generated manually. String format.
}
}
\details{
Function returns a GA access token.
}
\value{
Access token is a string to be used in conjunction with gaRequest() function.
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
\code{\link[GAR]{getMeta}}
}
\examples{

tokenRefresh(
            client_id="ABCDEFG",
            client_secret="HIJKLMNOP",
            token="QRSTUVWX"
            )

}
