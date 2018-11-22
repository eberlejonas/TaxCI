\name{plotParamBCC}
\alias{plotParamBCC}
\title{
Get indices for barcode-cluster plotting
}
\description{
Summarizes the output from BCCluster and outputs indices for plotting on the tree.
}
\usage{
plotParamBCC(tree, x, BCCluster.result, plotParamTCI.result, cluster.nodes = TRUE)
}
\arguments{
  \item{tree}{
The tree that will be plotted later.
}
  \item{x}{
A table with taxonomic information (must contain a column with species names which is specified with sp.col). Must be the same one that was used in the previous \code{\link{BCCluster}}-analysis.
}
  \item{BCCluster.result}{
The list that resulted from a \code{\link{BCCluster}}-analysis.
}
  \item{plotParamTCI.result}{
The list that resulted from a \code{\link{plotParamTCI}}-analysis.
}
  \item{cluster.nodes}{
Logical. Should the mrca-node number of each cluster containing more than 1 specimen be saved for later visualization with \code{\link{TaxCIplot}}? Will take quiet a while for large trees.
}
}
\details{
This function is usefull for further processing the output of an \code{\link{BCCluster}}-analysis.
}
\value{
A list containing the following elements:
  \item{sp.in.het.clust}{A vector of all species that appear in heterogeneous clusters.}
  \item{spn.in.het.clust}{A vector of all specimens that appear in heterogeneous clusters.}
  \item{species.in.n.clusters}{Named integer vector with numbers of clusters in which a species occur.}
  \item{spn.in.het.clust2}{Character vector of all specimens that appear in heterogeneous clusters AND whichs species appear in another cluster.}
  \item{rel.abundance}{List of relative abundances of species in heterogeneous clusters.}
  \item{rel.abundance.spn}{Named vector of relative abundances of all specimens in heterogeneous clusters.}
  \item{spn.marked.by.rel.abund}{Character vector of specimens in heterogeneous clusters which don't have the lowest relative abundance.}
  \item{splitted.species.homogen}{Species that appear in more than one homogeneous cluster.}
  \item{spn.in.split.hom.clust}{Specimens in splitted.species.homogen.}
  \item{relevant.species}{A vector of all specimens that might be problematic.}
  \item{i}{index for tree$tip.label of specimens that are in heterogeneous clusters}
  \item{j}{index for tree$tip.label of specimens that are in heterogeneous clusters and occur in more than one cluster}
  \item{k}{index for tree$tip.label of specimens that are marked because they didn't have the highest relative abundance in a heterogeneous cluster.}
  \item{l}{index for tree$tip.label of specimens which belong to species that appear in more than one homogeneous cluster}
  \item{m}{index for tree$tip.label of all relevant specimen}
  \item{tips}{a vector 1:number_of_tips_in_tree. Usefull for plottig something on all tips except the indexed (tips[-i])}
  \item{mrca.clusters}{Only available if \code{cluster.nodes = TRUE}. Contains node numbers of \code{tree} that are most recent common ancestors of a cluster.}
}
\author{
Jonas Eberle
}
\seealso{
See \code{\link{TaxCI}} for example.

Related: \code{\link{BCCluster}}
}
