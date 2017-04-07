\name{parse_mPTP}
\alias{parse_mPTP}
\title{
Parse mPTP output for use with TaxCI
}
\description{
Parse clustering output from mPTP (Kapli et al. 2016) for use with TaxCI. 
}
\usage{
parse_mPTP(file)
}
\arguments{
  \item{file}{
Path to the mPTP output textfile containing the putative species clusters.
}
}
\value{
A data.frame with two colums, the first containing specimen lables as in the taxonomic information and the second with integers describing cluster affiliation. Intended for use with \code{\link{BCCluster}}.
}
\references{
P. Kapli, S. Lutteropp, J. Zhang, K. Kobert, P. Pavlidis, A. Stamatakis and T. Flouri (2016). Multi-rate Poisson Tree Processes for single-locus species delimitation under Maximum Likelihood and Markov Chain Monte Carlo. bioRxiv preprint first posted online. doi: 10.1101/063875
}
\author{
Jonas Eberle
}
\seealso{
\code{\link{BCCluster}}
}
\examples{
# assuming that "mPTP_result.txt" is your mPTP output, use:
# df <- parse_mPTP("mPTP_result.txt")
}
