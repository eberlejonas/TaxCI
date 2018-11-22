findSpeciesInClusters <- function(pattern="vulgaris", BCCluster.result, md, verbose = TRUE){
  m <- 0;
  cluster <- c()
  for (i in 1:BCCluster.result$no.clusters) {
    t <- grep(pattern, BCCluster.result$sp.in.clusters[[i]])
    if (length(t) > 0) {
      cluster <- c(cluster, i)
      if (m < length(t)) {m <- length(t)}
    }
  }
  if (m == 0) {
    message("Pattern not found.")
  }
  
  # specimen indices in clusters
  if (verbose) {
    for (i in cluster) {
      spn <- BCCluster.result$clusters[[i]]
      t <- grep(pattern, md[spn,BCCluster.result$sp.col])
      cat("Cluster ", i, "Index ", t, "\n")
    }
  }
  
  return(invisible(cluster))
}