\name{noMorphospecies}
\alias{noMorphospecies}
\title{
Get number of morphospecies in dataset
}
\description{
Counts the number of a priori determined morphospecies/genera/etc. in a dataset.
}
\usage{
noMorphospecies(metadata, column = "binomial", tree = NULL)
}
\arguments{
  \item{metadata}{
A table with taxonomic information (e.g. genus and species in columns). If a tree is given, rownames(x) must exactly match the tip labels of the tree.
}
  \item{column}{
Which column of the metadata should be summarized? (Number or column-name.)
}
  \item{tree}{
If a tree is specified, the data will be subset to match its tips. Defaults to tree=NULL, i.e. all elements in metadata are counted.
}
}
\value{
An integer value (the number of unique elements in the specified column).
}
\author{
Jonas Eberle
}
\examples{
data(Carabidae)
noMorphospecies(Carabidae$metadata, column = "binomial", tree = Carabidae$tree)
}
