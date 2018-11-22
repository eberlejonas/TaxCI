\name{tci}
\alias{tci}
\title{
Calculate the Consistency Index and the Retention Index for taxonomic data
}
\description{
Main function for calculating the Taxonomic Consistency Index.
}
\usage{
tci(tree, x, level)
}
\arguments{
  \item{tree}{
A fully bifurcating tree (phylo-object).
}
  \item{x}{
A table with taxonomic information (e.g. genus and species in columns). rownames(x) must exactly match the tip labels of tree.
}
  \item{level}{
Column-name or number (integer) of x , i.e. the level for which the Taxonomic CI should be calculated.
}
}
\details{
Functions for calculating the Consistency Index (Kluge and Farris 1969) along phylogenetic trees. Especially intended for use with taxonomic data on different levels. Uses \code{\link{phangorn}} (Schliep 2011).
}
\value{
A list with the following entries:
  \item{tax.ci}{A vector of taxonomic consistency indices with taxa as names.}
  \item{level}{the level, i.e. column that was used in the inference}
}
\references{
Kluge, A.G, Farris, J.S. (1969). Quantitative phyletics and the evolution of anurans. Syst. Zool. 18: 1-32.

Schliep K.P. (2011). phangorn: phylogenetic analysis in R. Bioinformatics, 27(4) 592-593.
}
\author{
Jonas Eberle
}
\seealso{
See \code{\link{TaxCI}} for example.

Related: \code{\link{CI}}
}
