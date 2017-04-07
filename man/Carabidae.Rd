\name{Carabidae}
\alias{Carabidae}
\docType{data}
\title{
Example dataset of carabid beetles
}
\description{
Alignment of Cox1 barcode sequences, the corresponding maximum likelihood tree, and metadata of an early stage Carabidae data set from GBOL, illustrating the functions of TaxCI.
}
\usage{data("Carabidae")}
\format{
  A list of three elements:
  \enumerate{
  \item{\bold{alignment:} }{Alignment of 1604 Cox1-sequences of ground beetles ('DNAbin')}
  \item{\bold{tree:} }{Maximum likelihood tree based on the alignment.}
  \item{\bold{metadata:} }{A data.frame containing the metadata to the species. Species information is stored in the 'binomial'-column.}
}}
\source{
Rulik et al. 2015. Using taxonomic consistency for semi-automatized preprocessing to ensure high quality Barcode data submissions -- a case study on German beetles.
}
\seealso{
See \code{\link{TaxCI}} for example.
}
\keyword{datasets}
