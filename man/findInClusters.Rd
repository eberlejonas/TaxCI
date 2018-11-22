\name{findInClusters}
\alias{findInClusters}
\alias{findSpeciesInClusters}

\title{
Search for clusters matching a given pattern
}
\description{
Functions to search occurances of a genus / species / RegEx in the clusters of a BCCluster-analysis.
}
\usage{
findInClusters(pattern="vulgaris", BCCluster.result, verbose = TRUE)
findSpeciesInClusters(pattern="vulgaris", BCCluster.result, md, verbose = TRUE)
}
\arguments{
  \item{pattern}{A RegEx - pattern that is passed to grep in order to match against the clusters. See details section.}
  \item{BCCluster.result}{The result object of a \code{\link{BCCluster}} analysis.}
  \item{md}{Metadata table that was used for the \code{\link{BCCluster}} analysis.}
  \item{verbose}{Set to 'FALSE' to suppress printing to console.}
}
\details{
\code{findInClusters} directly matches against the specimen ids in \code{BCCluster.result$clusters}.
\code{findSpeciesInClusters} matches the pattern against \code{BCCluster.result$sp.in.clusters} which should equal the species column entry in the metadata. Specimen indices are searched in the respective species column entries in the metadata of the ids in \code{BCCluster.result$clusters}. This is useful if the actual specimens are labelled by their IDs only.
}
\value{
Prints indices of matching clusters and the indices of matching specimens within the cluster on the console and returns a vector of cluster indices invisibly.
}
\author{
Jonas Eberle
}
\seealso{
\code{\link{BCCluster}}
}
\examples{
### Sorry, still no examples.
}
