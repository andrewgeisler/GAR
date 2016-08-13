\name{getMeta}
\alias{getMeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Access Google's Metadata API
}
\description{
This function retrieves a data frame of dimensions and metrics available to use in the Core Reporting API along with descriptions and usage information.
}
\usage{
getMeta()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
No arguments required.
}
\details{
The function returns a dataframe.
}
\value{
  \item{id}{}
  \item{kind}{}
  \item{type}{}
  \item{dataType}{}
  \item{group}{}
  \item{status}{}
  \item{uiName}{}
  \item{description}{}
  \item{allowedinSegments}{}
  \item{replaceBy}{}
  \item{calculation}{}
  \item{minTemplateIndex}{}
  \item{maxTemplateIndex}{}
  \item{premiumMinTemplateIndex}{}
  \item{premiumMaxTemplateIndex}{}
}
\references{
https://developers.google.com/analytics/devguides/reporting/metadata/v3/
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
getMeta()
}