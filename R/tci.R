#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# Calculate taxonomic consistency index #
#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
tci <- function (tree, x, level) {
  if (!inherits(tree, "phylo")) 
    stop("object \"tree\" is not of class \"phylo\"")
  if (!is.binary(tree)) 
    tree <- multi2di(tree)
  if (!all(tree$tip.label %in% rownames(x)))
    stop("not all tree$tip.label are in taxonomic information table rownames")
  if (!is.data.frame(x))
    x <- as.data.frame(x)
  x <- x[tree$tip.label,]
  
  # change the tip labels of the tree to the taxonomic group level
  tree$tip.label <- as.character(x[tree$tip.label, level])
  
  # # Now gererate taxonomic dummy variables...
  d <- Dummies(tree)

  # convert metadata to phangorn format
  data <- phyDat(data=d, type="USER", levels=c(0,1))
  
  # calculate CI and save to output table
  tax.ci <- round(phangorn::CI(tree=tree, data=data, sitewise=TRUE), 2)
  names(tax.ci) <- colnames(d)
  
  return(list(tax.ci=tax.ci,
              level=level))
}
