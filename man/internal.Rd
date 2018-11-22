\name{internal}
\alias{Dummies}
\alias{transform_user_table}
\alias{is.wholenumber}
\title{
Internal TaxCI-functions
}
\description{
\code{Dummies}: Create dummy variables.

\code{transform_user_table}

\code{is.wholenumber}
}
\usage{
Dummies(tree)

transform_user_table(data, x)

is.wholenumber(x, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{tree}{
  A phylogenetic tree in phylo-format.
  }
  \item{x}{
  \code{transform_user_table}: A metadata table. row.names() must correspond to data[,1].
  
  \code{an R-object}
  }
  \item{data}{
  A data.frame with two columns: (i) specimen id, (ii) grouping by integers.
  }
  \item{tol}{
  Tolerance.
  }
}
\details{
\code{Dummies}: Creates 0-1-dummy variables for the taxonomic level as labeled at tip of a tree. The outgroup is ignored.

\code{transform_user_table}: Makes cluster object for internal use from a user-provided data.frame and the metadata.
}

\author{
Jonas Eberle
}