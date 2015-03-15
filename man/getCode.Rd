\name{getCode}
\alias{getCode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Authenticates a Google Analytics. 
}
\description{
This function launches an URL in a broswer for authentication. Once authenticated, the value after code= appearing in the resulting URL needs to be manually copied and stored for later use. This code will allow you to get a refresh token that can be refreshed until revoked. This should be a one-time process unless code is revoked,lost or expires before used to get a refresh token.

}
\usage{
getCode(client_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{client_id}{
Client ID found in API Console. String format.
}
}
\details{
Function opens a webiste URL in a browser for authenication. A code can be retrieved from the redirect URL. 
}
\value{
Code is a string to be used in conjunction with gaRefresh() function.
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
