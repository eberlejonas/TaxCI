plotParamBCC <- function(tree, x, BCCluster.result, plotParamTCI.result, cluster.nodes = TRUE){
  
  # check if tree and x are consistent and re-sort x
  if (!all(tree$tip.label %in% rownames(x))) {
    print(tree$tip.label[-which(tree$tip.label %in% rownames(x))])
    stop("plotParamTCI: above treelabels not contained in 'x'")
  }
  x <- x[tree$tip.label,]
  
  
  # find specimen in heterogeneous clusters (workflow step 2) ### NOTE: this is probably already done in BCCluster() and should be saved there
  if (length(BCCluster.result$heterogen) == 0) {
    spn.in.het.clust <- NULL
  } else {
    spn.in.het.clust <- unlist(BCCluster.result$clusters[BCCluster.result$heterogen])
  }
  
  # find species in heterogeneous clusters
  if (is.null(spn.in.het.clust)) {
    sp.in.het.clust <- NULL
  } else {
    sp.in.het.clust <- unique(x[spn.in.het.clust,BCCluster.result$sp.col])
  }
  
  # Get species that occur in more than one cluster
  species.in.n.clusters <- table(unlist(BCCluster.result$sp.in.clusters))
  splitted.species      <- names(which (species.in.n.clusters > 1))
  
  
  
  # loop through all heterogeneous clusters:
  spn.in.het.clust2 <- c()         # those will be the specimens in heterogeneous clusters that occur in other clusters, too.
  rel.abundance <- list()
  spn.marked.by.rel.abund <- c()
  rel.abundance.spn <- c()
  cluster_mrca <- c()
  
  for (i in BCCluster.result$heterogen) {
    
    # (workflow step 4)
    # calculate relative abundace of species in all heterogeneous clusters (1-n/N; n=#spnArt, N=#spnCluster)
    t <- table(factor(x[BCCluster.result$clusters[[i]],BCCluster.result$sp.col]))
    N <- sum(t)
    rel.abundance[[i]] <- 1 - t/N
    # Get species that are in heterogeneous clusters and have 'not the lowest' rel. abundance
    low.rel.ab.sp <- names(which(rel.abundance[[i]] > min(rel.abundance[[i]])))
    
    # (workflow step 3)
    # check if species in heterogeneous clusters appear in other clusters, too
    # save specimens which belong to species that appear in more than one cluster
    for (s in BCCluster.result$clusters[[i]]){
      if (x[s,BCCluster.result$sp.col] %in% splitted.species) {
        spn.in.het.clust2 <- c(spn.in.het.clust2, s)
      }
      # (workflow step 4)
      # save specimens which belong to species that appear in heterogeneous clusters and have low relative abundance
      if (x[s,BCCluster.result$sp.col] %in% low.rel.ab.sp) {
        spn.marked.by.rel.abund <- c(spn.marked.by.rel.abund, s)
      }
      # save relative abundances of each specimen
      rel.abundance.spn[s] <- rel.abundance[[i]][ as.character( x[s, BCCluster.result$sp.col] ) ]
      # save the number of the mrca-node (Only heterogeneous will be saved here)
      if (cluster.nodes) {
        cluster_mrca <- c(cluster_mrca, getMRCA(tree, BCCluster.result$clusters[[i]]))
      }
    }
  }
  
  # loop though all homogeneous clusters and save the number of the mrca-node (Only homogeneous will be saved here)
  if (cluster.nodes) {
    for (i in BCCluster.result$homogen) {
      cluster_mrca <- c(cluster_mrca, getMRCA(tree, BCCluster.result$clusters[[i]]))
    }
  }
  
  
  # (workflow step 5)
  species.homogen <- unlist(BCCluster.result$sp.in.clusters[BCCluster.result$homogen]) # species in homogeneous clusters
  splitted.species.homogen <- intersect(species.homogen, splitted.species)               # species in homogeneous clusters and in splitted species
  split.hom.clust          <- NULL # indices of splitted homol. clusters
  for (sp in splitted.species.homogen) {
    ii <- findSpeciesInClusters(sp, BCCluster.result, verbose=F) # ii = cluster in which species does occur
    no.hom <- length(which(ii %in% cl$homogen))
    for (i in ii) {
      if (i %in% BCCluster.result$homogen & no.hom > 1) {           # save cluster index if it's homogeneous and if there are more than 1 homogeneous cluster (putative cryptic diversity)
        split.hom.clust <- c(split.hom.clust, i)
      }
    }
  }
  spn.in.split.hom.clust   <- unlist(BCCluster.result$clusters[split.hom.clust]) # specimens in splitted homol. clusters
  
  relevant.specimen <- unique(c(plotParamTCI.result$specimen,
                                spn.in.het.clust,
                                spn.in.het.clust2,
                                spn.marked.by.rel.abund,
                                spn.in.split.hom.clust))
  if (!is.null(relevant.specimen)) {relevant.specimen <- sort(relevant.specimen)}
  
  # Get indices of filtered specimens (if variable is empty, an index out of range is assigned to facilitate plotting with i and tips[-i])
  N <- length(tree$tip.label) + tree$Nnode + 1
  if (length(spn.in.het.clust) > 0)        {r.i <- which(tree$tip.label %in% spn.in.het.clust)}        else {r.i <- N}
  if (length(spn.in.het.clust2) > 0)       {r.j <- which(tree$tip.label %in% spn.in.het.clust2)}       else {r.j <- N}
  if (length(spn.marked.by.rel.abund) > 0) {r.k <- which(tree$tip.label %in% spn.marked.by.rel.abund)} else {r.k <- N}
  if (length(spn.in.split.hom.clust) > 0)  {r.l <- which(tree$tip.label %in% spn.in.split.hom.clust)}  else {r.l <- N}
  if (length(relevant.specimen) > 0)       {r.m <- which(tree$tip.label %in% relevant.specimen)}       else {r.m <- N}
  
  tips <- 1:length(tree$tip.label)
  
  cluster_mrca <- sort(cluster_mrca)
  
  result <- list(clusters                 = BCCluster.result$clusters,
                 sp.in.het.clust          = sp.in.het.clust,
                 spn.in.het.clust         = spn.in.het.clust,
                 species.in.n.clusters    = species.in.n.clusters,
                 spn.in.het.clust2        = spn.in.het.clust2,
                 rel.abundance            = rel.abundance,
                 rel.abundance.spn        = rel.abundance.spn,
                 spn.marked.by.rel.abund  = spn.marked.by.rel.abund,
                 splitted.species.homogen = splitted.species.homogen,
                 spn.in.split.hom.clust   = spn.in.split.hom.clust,
                 relevant.specimen        = relevant.specimen,
                 i    = r.i,
                 j    = r.j,
                 k    = r.k,
                 l    = r.l,
                 m    = r.m,
                 tips = tips)
  
  if (cluster.nodes) {
    result$mrca.clusters = cluster_mrca
  }
  
  return(result)
}

