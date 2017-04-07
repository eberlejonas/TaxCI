\name{makeTable}
\alias{makeTable}
\title{
Summary tables for TaxCI-analyses
}
\description{
Create a summary table for a taxonomic consistency analysis from \code{\link{tci}} and the \code{\link{BCCluster}}.
}
\usage{
makeTable(plotParamTCI.result, plotParamBCC.result, sort = "score", file = NA)
}
\arguments{
  \item{plotParamTCI.result}{
The output of \code{\link{plotParamTCI}}.
}
  \item{plotParamBCC.result}{
The output of \code{\link{plotParamBCC}}.
}
  \item{sort}{
By which column should the table be sorted? Possible choices are: \"tci\", \"cl.het\", \"sp.split\", \"low.abun\", \"other.homog\", or \"score\". Defaults to \"score\". 
}
  \item{file}{
Optional. If specified, a tab-delimited text file is created instead of a data.frame.
}
}
\details{
No output is created if no problematic specimens were detected.
}
\value{
A data.frame with relevent specimens as rownames or a tab-delimited text file.
}
\author{
Jonas Eberle
}
\seealso{
\code{\link{tci}}, \code{\link{BCCluster}}, \code{\link{plotParamTCI}}, \code{\link{plotParamBCC}}
}
\examples{
# See \code{\link{TaxCI}} for example.
}
