findInClusters <- function(pattern="vulgaris", BCCluster.result, verbose = TRUE){
  m <- 0;
  cluster <- c()
  for (i in 1:BCCluster.result$no.clusters) {
    t <- grep(pattern, BCCluster.result$clusters[[i]])
    if (length(t) > 0) {
      if (verbose) {cat("Cluster ", i, "Index ", t, "\n")}
      cluster <- c(cluster, i)
      if (m < length(t)) {m <- length(t)}
    }
  }
  if (m == 0) {
    message("Pattern not found.")
  }
  return(invisible(cluster))
}
