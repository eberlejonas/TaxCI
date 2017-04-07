\name{TaxCI-package}
\alias{TaxCI-package}
\alias{TaxCI}
\docType{package}
\title{
Tools for checking the integrity of DNA-barcode data
}
\description{
The package contains tools for checking the quality of barcoding data prior to submission. It is intended for highlighting for instance contaminations or misidentifications in very large trees in two steps:\cr
1. By reconstructing the Consistency Index (Kluge and Farris 1969) and the Retention Index (Farris 1989) along phylogenetic trees. (Usable with taxonomic data on different levels.)\cr
2. By evaluating individual composition of barcoding-clusters. (On species level.)
}
\details{
\tabular{ll}{
Package: \tab TaxCI\cr
Type: \tab Package\cr
Version: \tab 0.4.4\cr
Date: \tab 2016-12-13\cr
License: \tab AGPL-3\cr
}
Main function: \code{\link{tci}}, \code{\link{BCCluster}}
}
\author{
Jonas Eberle \email{eberle.jonas@gmail.com}
}
\references{
Farris, J.S. (1989). The retention index and rescaled consistency index. Cladistics 5: 417-419.

Kluge, A.G, Farris, J.S. (1969). Quantitative phyletics and the evolution of anurans. Syst. Zool. 18: 1-32.
}
\keyword{CI, Consistency Index, RI, Retention Index, Taxonomic Consistency, barcode, cluster, data quality}
\examples{
library(TaxCI)

#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# load some data
#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#

data(Carabidae)
head(Carabidae$metadata)


#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# analyses
#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#

# taxonomic consistency analysis
ci <- tci(Carabidae$tree, Carabidae$metadata, level="binomial", num_cores=1)

# cluster analysis
cl <- BCCluster(Carabidae$alignment, Carabidae$metadata, sp.col="binomial")

# prepare data for plotting
p1 <- plotParamTCI(Carabidae$tree, Carabidae$metadata, ci)
p2 <- plotParamBCC(Carabidae$tree, Carabidae$metadata, cl, p1)


#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# multipageplot to pdf
# use oma for page bleed margin [inch]
# and mai for controlling overlap of pages [inch]
#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#

npages <- ceiling(length(Carabidae$tree$tip.label) / 130)
pdf("Carabidae.pdf", width=8.3, height=11.7, pointsize=3.5)
par(oma=c(1,1,1,3)+5, mai=c(0,0,0,0)+.03, xaxs="i", yaxs="i", xpd=TRUE)
TaxCIplot(Carabidae$tree, Carabidae$metadata, p1, p2,
       box.placement="auto", npages=npages,
       cex=.7, lwd=.6, edge.width=.6, legend=TRUE,
       info="R-package 'TaxCI'\nExample script 'Carabidae'\npdf output")
dev.off()


#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# write output table
#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
makeTable(p1, p2, "score", "score_table.tsv")
#

result <- TaxCIsummary(Carabidae$tree, Carabidae$metadata, sp.col="binomial", BCCluster.result=cl, plotParamTCI.result=p1, plotParamBCC.result=p2, analysis="example")

###
# you can do another set of analyses and compare the results with:
# cbind(result, result2)
###

}
