\name{BCCluster}
\alias{BCCluster}
\alias{print.BCCluster}
\title{
Barcode clustering
}
\description{
Barcode clustering using \code{\link{localMinima}}\{spider\}. The first local minimum is used for clustering.
}
\usage{
BCCluster(data, x, method="spider", sp.col="sp", distmodel="K80", pairwise.deletion=FALSE, fix.threshold=NA)

## S3 method for class 'BCCluster'\cr
print.BCCluster(x, ...)
}
\arguments{
  \item{data}{
If method is "spider" (the default): a matrix or a list containing the DNA sequences; this must be of class "DNAbin" (use as.DNAbin is they are stored as character) (Copied from \code{\link{dist.dna}}).

If method is "external": a data.frame with two colums, the first containing specimen labels as in the taxonomic information and the second with integers describing cluster affiliation.
}
  \item{x}{
A table with taxonomic information (must contain a column with species names which is specified with sp.col). rownames(x) must exactly match rownames(alignment).\cr

For S3 method BCCluster an object of class \code{BCCluster}.
}
  \item{method}{
Either "spider" to use the package spider to infer clusters or "external" if the user supplies a clustering table (see 'data').
}
  \item{sp.col}{
The name or number of the column of x that contains species names
}
  \item{distmodel}{
Copied from \code{\link{dist.dna}}: a character string specifying the evolutionary model to be used; must be one of "raw", "N", "TS", "TV", "JC69", "K80" (the default), "F81", "K81", "F84", "BH87", "T92", "TN93", "GG95", "logdet", "paralin", "indel", or "indelblock".
}
  \item{pairwise.deletion}{passed to \code{\link{dist.dna}}: a logical indicating whether to delete the sites with missing data in a pairwise way. The default is to delete the sites with at least one missing data for all sequences (ignored if model = "indel" or "indelblock").}
  \item{fix.threshold}{
If NA, threshold is estimated my \code{\link{tclust}}. Else distance cutoff for clustering. E.g. 0.01 (1\%).
}
\item{...}{Unused.}
}
\details{
Finds distance based barcode-clusters using spider-functions. Some summarizing and preparing is done for Taxonomic Consistency analyses.
}
\value{
A list with the following elements:
  \item{clusters}{A list of clusters, each containing the respective specimen names.}
  \item{sp.in.clusters}{A list of clusters, each containing the names of species that are in the respective cluster.}
  \item{homogen}{A vector containing the indices of homogeneous clusters. (Only one species contained.)}
  \item{heterogen}{A vector containing the indices of heterogeneous clusters. (More than one species contained.)}
  \item{no.clusters}{The number of clusters inferred.}
  \item{no.homogeneous}{The number of homogeneous clusters.}
  \item{no.heterogeneous}{The number of homogeneous clusters.}
  \item{sp.col}{User-set column of x containing species information.}
  \item{threshold}{The local minimum threshold that was used or the fixed threshold.}
}
\author{
Jonas Eberle
}
\seealso{
\code{\link{localMinima}}, \code{\link{tclust}}, \code{\link{read.dna}}, \code{\link{dist.dna}}
}
\examples{
# See \code{\link{TaxCI}} for example.
}
