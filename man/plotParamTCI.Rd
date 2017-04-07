\name{plotParamTCI}
\alias{plotParamTCI}
\title{
Get indices for tci plotting.
}
\description{
Get indices for plotting the results of tci-analyses on the phylogeny.
}
\usage{
plotParamTCI(tree, x, tci.result)
}
\arguments{
  \item{tree}{
A fully bifurcating tree (phylo-object). Must be the same one that was used in the previous tci-analysis.
}
  \item{x}{
A table with taxonomic information (e.g. genus and species in columns). Must be the same one that was used in the previous \code{\link{tci}}-analysis.
}
  \item{tci.result}{
Results-List of the \code{\link{tci}}-analysis.
}
}
\details{
Finds the species with Taxonomic Consistency Index < 1 and the specimens belonging to these species in the results of an tci-analysis. The specimens indices in tree$tip.label are saved for plotting the results with plot.phylo.
}
\value{
A list with the following elements:
  \item{species}{species with CI < 1}
  \item{specimen}{specimens belonging to species with CI < 1}
  \item{i}{index of these specimens in tree$tip.label}
  \item{tips}{a vector 1:number_of_tips_in_tree. Usefull for plottig something on all tips except the indexed (tips[-i])}
}
\author{
Jonas Eberle
}
\seealso{
See \code{\link{TaxCI}} for example.

Related: \code{\link{tci}}
}
