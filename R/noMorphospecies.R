# How many morphospecies are in dataset?
noMorphospecies <- function(metadata, column="binomial", tree=NULL) {
  if (is.null(tree)) {
    no <- length(unique(metadata[,column]))
  } else {
    if(!(all(tree$tip.label %in% rownames(metadata)))) {stop("Not all tiplabels of the tree are in the rownames of metadata!")}
    no <- length(unique(metadata[tree$tip.label,column]))
  }
  return(no)
}
