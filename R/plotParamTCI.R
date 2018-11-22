plotParamTCI <- function(tree, x, tci.result) {
  level <- tci.result$level
  cis   <- tci.result$tax.ci
  
  # check if tree and x are consistent and re-sort x
  if (!all(tree$tip.label %in% rownames(x))) {
    problems <- paste(tree$tip.label[-which(tree$tip.label %in% rownames(x))], collapse="\n")
    stop("plotParamTCI: some treelabels are not contained in 'x':\n", problems)
  }
  x <- x[tree$tip.label,]
  
  # check if level from tci.result is compatible with x
  if (!all(names(cis) %in% x[,level])) {
    names(cis)[-which(names(cis) %in% x[,level])]
    stop("plotParamTCI: above taxa not in level-column of 'x'")
  }
  
  # find the species with ci < 1
  tci.sp.i <- which(cis < 1)    # index of species with CI < 1
  tci.sp   <- names(tci.sp.i)   # names of species with CI < 1
  # find all specimens of species with ci < 1
  ind.sp    <- as.vector(x[,level])
  tci.ind.i <- which(ind.sp  %in%  tci.sp)
  tci.ind   <- rownames(x)[tci.ind.i]
  
  if (length(tci.ind) == 0) {
    tci.ind <- NULL
    N <- length(tree$tip.label) + tree$Nnode + 1
    tci.ind.i <- N
  }
  
  tips <- 1:length(tree$tip.label)
  
  return(list(species  = tci.sp,    # species with CI < 1
              specimen = tci.ind,   # specimen of species with CI < 1
              i        = tci.ind.i, # index of these individuals in tree$tip.labels
              tips     = tips))      # vector 1:number_of_tips_in_tree
}

