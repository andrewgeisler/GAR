\name{tokenRefresh}
\alias{tokenRefresh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieves a Google Analytics access token using a refresh token. 
}
\description{
This function retrieves a Google Analytics access token using a supplied refresh token. Refresh tokens do not expire and can be traded for an access token repeatedly.  
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


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[GAR]{GAR}}
\code{\link[GAR]{gaRequest}}
\code{\link[GAR]{getCode}}
\code{\link[GAR]{getRefresh}}
\code{\link[GAR]{tokenRefresh}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
