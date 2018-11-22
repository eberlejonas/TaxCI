\name{TaxCIsummary}
\alias{TaxCIsummary}
\title{
Statistics of a TaxCI-analysis
}
\description{
Get numbers of species in different kinds of clusters.
}
\usage{
TaxCIsummary(tree, x, sp.col = "binomial", BCCluster.result, plotParamTCI.result, plotParamBCC.result, analysis = "TaxCI.analysis", file=NA)
}
\arguments{
  \item{tree}{
The tree that was used for the previous analyses.
}
  \item{x}{
The table with taxonomic information (must contain a column with species names which is specified with sp.col). Must be the same one that was used in the previous analyses.
}
  \item{sp.col}{
The column of the metadata that contains the species names.
}
  \item{BCCluster.result}{
The list that resulted from a \code{\link{BCCluster}}-analysis.
}
  \item{plotParamTCI.result}{
The output of \code{\link{plotParamTCI}}.
}
  \item{plotParamBCC.result}{
The output of \code{\link{plotParamBCC}}.
}
  \item{analysis}{
Optional. A name for the summarized analysis provided as character string.
}
  \item{file}{
Optional. If a filename is given the table will be printed to it. Defaults to NA (table is returned as data.frame).
  }
}
\value{
A data frame with the following entries:
\item{noMorphospecies}{Number of a priori identified species.}
\item{no.tci.positive.sp}{Number of species with TCI < 1.}
\item{no.cluster}{Number of barcode clusters.}
\item{no.homogen}{Number of homogeneous clusters (Containing only one species).}
\item{no.heterogen}{Number of heterogeneous clusters (Containing multiple species.}
\item{no.species.in.multiple.clusters}{Number of species that appear in more than one cluster.}
\item{no.species.in.multiple.hom.clusters}{Number of species that appear in more than one homogeneous cluster.}
\item{threshold}{The barcode-threshold that was used for clustering. "external" if clustering was done with an external method.}
\item{Q-measure}{A quality measure...}
}
\author{
Jonas Eberle
}
\seealso{
See \code{\link{TaxCI}} for example.
}
