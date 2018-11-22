\name{TaxCIplot}
\alias{TaxCIplot}
\title{
TaxCI plotting
}
\description{
Plot the results of a taxonomic consistency analysis with pretty boxes on a tree.
}
\usage{
TaxCIplot(tree, x, plotParamTCI.result, plotParamBCC.result=NA,
      box.placement="auto", adj=.515, step=0.01, label.offset=0.08,
      npages=1, draft=F, vis.clusters=TRUE, info="", legend=FALSE, alternative.tip.labels=NULL, ...)
}
\arguments{
The arguments must be exactly the same that were used in the \code{\link{tci}} and the \code{\link{BCCluster}} analyses:
  \item{tree}{The tree.}
  \item{x}{The table with taxonomic information.}
  \item{plotParamTCI.result}{The output of \code{\link{plotParamTCI}}.}
  \item{plotParamBCC.result}{The output of \code{\link{plotParamBCC}}.}
  \item{box.placement}{If set to "auto", TaxCIplot will try to place the boxes in the correct place automatically. EXPERIMENTAL!}
  \item{adj}{The offset of the boxes from the tips of the tree. Ignored if \code{box.placement = "auto"}.}
  \item{step}{The step-length to go right before the next box is plotted. Ignored if \code{box.placement = "auto"}.}
  \item{label.offset}{Distance from tips to tip labels. Passed to \code{\link{plot.phylo}} and ignored if \code{box.placement = "auto"}.}
  \item{npages}{The number of pages on which the plot should be distributed. Some tips can be plotted again after page-break. Control this with the margin width options of \code{\link{par}}.}
  \item{draft}{Only the first 3 pages are printed. Usefull to adjust spacing in large plots.}
  \item{vis.clusters}{Logical: Should small black dots be plotted on nodes of the inferred clusters from \code{\link{plotParamBCC}}? Or alternatively a numeric vector containing the node numbers to plot the dots on.}
  \item{alternative.tip.labels}{A vector of mode character or expression and the same length as \code{tree$tip.labels}. Defaults to NULL (ignored)}
  \item{info}{A character string to be plotted at the top right corner of each page. No info is plotted if \code{info=NA} (default).}
  \item{legend}{Logical. Should a legend be plotted?}
  \item{...}{Passed to \code{\link{plot.phylo}}. \code{x.lim} usefull for increasing the plot region to the right (only if \code{box.placement != "auto"}). Don't use ylim! It's used for multipage-plotting.}
}
\value{
A phylogenetic plot from \code{\link{plot.phylo}}.
}
\author{
Jonas Eberle
}
\note{
Plotting of \code{\link{tci}}-results alone is not tested.
}
\seealso{
See \code{\link{TaxCI}} for example.

Related: \code{\link{plot.phylo}}\{ape\}, \code{\link{tci}}, \code{\link{BCCluster}}, \code{\link{plotParamTCI}}, \code{\link{plotParamBCC}}
}
