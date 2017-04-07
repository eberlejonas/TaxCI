\name{tci}
\alias{tci}
\title{
Calculate the Consistency Index and the Retention Index for taxonomic data
}
\description{
Main function for calculating the Taxonomic Consistency Index.
}
\usage{
tci(tree, x, level, reconstruction = "lower", num_cores = 1)
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
  \item{reconstruction}{
Should the MP-reconstruction return "lower" or "upper" values (check MPR in ape for info).
}
  \item{num_cores}{
Number of cores to use. Speed was approx. doubled with 2 cores in Carabidae test, but there was no further significant increase with 4 cores.}
}
\details{
Functions for calculating the Consistency Index (Kluge and Farris 1969) and the Retention Index (Farris 1989) along phylogenetic trees. Especially intended for use with axonomic data on different levels.
}
\value{
A list with the following entries:
  \item{tax.ci}{A matrix with taxa as rownames and taxonomic consistency and retention indices in 2 colums.}
  \item{level}{the level, i.e. column that was used in the inference}
}
\references{
Farris, J.S. (1989). The retention index and rescaled consistency index. Cladistics 5: 417-419.

Kluge, A.G, Farris, J.S. (1969). Quantitative phyletics and the evolution of anurans. Syst. Zool. 18: 1-32.
}
\author{
Jonas Eberle
}
\seealso{
\code{\link{MPR}}
}
\examples{
# See \code{\link{TaxCI}} for example.
}
