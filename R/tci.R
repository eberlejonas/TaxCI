#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# Calculate taxonomic consistency index #
#=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=#
tci <- function (tree, x, level, reconstruction="lower", num_cores=1) {
  if (!inherits(tree, "phylo")) 
    stop("object \"tree\" is not of class \"phylo\"")
  # make a root edge with length 1
  if (!is.binary.tree(tree)) 
    tree <- multi2di(tree)
  if (!all(tree$tip.label %in% rownames(x)))
    stop("not all tree$tip.label are in taxonomic information table rownames")
  if (!is.data.frame(x))
    x <- as.data.frame(x)
  x <- x[tree$tip.label,]
  # add an artificial outgroup to the tree...
  outgroup.phy <- list(edge=matrix(c(2,1),1,2), tip.label="outgroup", edge.length=1, Nnode=1); class(outgroup.phy)<-"phylo"
  tree <- multi2di(bind.tree(tree, outgroup.phy))
  # ...and the data
  x[nrow(x)+1,] <- x[nrow(x),]
  rownames(x) <- c(rownames(x)[-nrow(x)], "outgroup")
  for (colname in names(x))
  {
    attr(x[,colname], "levels") <- c(attr(x[,colname], "levels"), "outgroup")
  }
  x[nrow(x),] <- rep("outgroup", ncol(x))
  # change the tip labels of the tree to the taxonomic group level
  tree$tip.label <- as.character(x[tree$tip.label, level])
  # Now gererate taxonomic dummy variables...
  d <- Dummies(tree, outgroup="outgroup")
  
  # ...and calculate the taxomomic consistency index
  # setting up parallel environment
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  
  # run foreach - loop in parallel
  temp <- foreach (i = 1:ncol(d)) %dopar% {
    ci <- CI(tree, d[,i], outgroup="outgroup", reconstruction=reconstruction)
  }
  
  # write result to a table
  tax.ci <- matrix(NA, nrow=ncol(d), ncol=2); rownames(tax.ci) <- colnames(d); colnames(tax.ci) <- c("tax.ci", "tax.ri")
  for (i in 1:ncol(d)) {
      tax.ci[i,1] <- round(temp[[i]]$CI, 2)
      tax.ci[i,2] <- round(temp[[i]]$RI, 2)
  }
  
  # stop parallel environment
  stopCluster(cl)
  
  return(list(tax.ci=tax.ci,
              level=level))
  
}
