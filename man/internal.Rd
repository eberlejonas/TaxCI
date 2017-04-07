\name{internal}
\alias{CI}
\alias{Dummies}
\alias{transform_user_table}
\title{
Internal TaxCI-functions
}
\description{
\code{CI}: Calculates consistency index and retention index of traits along a phylogeny (internally used by TaxCI).\cr
\code{Dummies}: Create dummy variables.\cr
\code{transform_user_table}
}
\usage{
CI(tree, x, outgroup, reconstruction = "lower")

Dummies(tree, outgroup)

transform_user_table(data, x)
}
\arguments{
  \item{tree}{
  A phylogenetic tree in phylo-format.
}
  \item{x}{
  \code{CI}: A vector of integers.
  
  \code{transform_user_table}: A metadata table. row.names() must correspond to data[,1].
}
  \item{outgroup}{
  An integer or a character string giving the tip of 'tree' used as outgroup
  }
  \item{reconstruction}{
  Should the indices be computed for the lower or the upper values of the reconstructed sets?
  }
  \item{data}{
  A data.frame with two columns: (i) specimen id, (ii) grouping by integers.
  }
}
\details{
\code{CI}: Uses MPR from ape to do ancestral charaction reconstruction. MPR finds the sets of values for each node as intervals with lower and upper values (Hanazawa et al. 1995, Narushima and Hanazawa 1997, Farris 1970, and Swofford and Maddison 1987).

\code{Dummies}: Creates 0-1-dummy variables for the taxonomic level as labeled at tip of a tree. The outgroup is ignored.

\code{transform_user_table}: Makes cluster object for internal use from a user-provided data.frame and the metadata.
}

\value{
\code{CI}: A list with 4 elements: 1) observed.steps, 2) minimum.steps, 3) CI = Consistency Index, and 4) RI = Retention Index.
}
\references{
Farris, J. M. (1970) Methods for computing Wagner trees. Systematic Zoology, 19, 83-92.

Hanazawa, M., Narushima, H. and Minaka, N. (1995) Generating most parsimonious reconstructions on a tree: a generalization of the Farris-Swofford-Maddison method. Discrete Applied Mathematics, 56, 245-265.

Narushima, H. and Hanazawa, M. (1997) A more efficient algorithm for MPR problems in phylogeny. Discrete Applied Mathematics, 80, 231-238.

Swofford, D. L. and Maddison, W. P. (1987) Reconstructing ancestral character states under Wagner parsimony. Mathematical Biosciences, 87, 199-229.
}
\author{
Jonas Eberle
}