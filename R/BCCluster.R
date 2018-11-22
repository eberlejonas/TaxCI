BCCluster <- function(data, x, method="spider", sp.col="sp", distmodel="K80", pairwise.deletion=FALSE, fix.threshold=NA){
  if (!is.data.frame(x))
    x <- as.data.frame(x)
  if (is.wholenumber(sp.col)) {
    if (!(1 <= sp.col & sp.col <= ncol(x))) stop("BCCluster: sp.col must be in 1:", ncol(x), " or the name of a column of 'x'")
  } else {
    if (!sp.col %in% colnames(x)) stop("BCCluster: 'sp.col' is not in column headers of 'x'")
  }
  
  if (method == "spider") {
    cat("Trying to find clusters with spider: assuming that data is an alignment is of class DNAbin\n")
    
    # check the alignment
    if (!inherits(data, "DNAbin")) {stop("data is not of class DNAbin")}
    if (!is.matrix(data)) {
      cat("Attempting to convert DNA-data to a matrix")
      data <- as.matrix(data)
    }
    
    # check if alignment and x are consistent
    if (!all(rownames(data) %in% rownames(x))) {
      print(rownames(data)[which(!(rownames(data) %in% rownames(x)))])
      stop("BCCluster: above sequences not contained in 'x'")
    }
    
    # calculate dna-distances
    dna.dist <- dist.dna(data, distmodel, pairwise.deletion=pairwise.deletion)
    
    # find barcode threshold
    if (!is.na(fix.threshold)) {
      th <- fix.threshold
    } else {
      th <- localMinima(dna.dist)$localMinima[1]
    }
    
    # find all clusters
    clusters <- tclust(dna.dist, th)
    
    # translate cluster indices to specimen (?) names
    spp <- dimnames(data)[[1]]
    clusters <- lapply(clusters, function(x) spp[x])
  } else {
    cat("Expecting user provided cluster data.")
    # transform user data
    clusters <- transform_user_table(data, x)
    th <- "external"
  }
  
  # number of clusters
  n.cl <- length(clusters)
  
  # loop through all clusters -- How many species are contained?
  sp.in.clusters    <- list()
  no.sp.in.clusters <- list()
  for (i in 1:n.cl){
    specimen <- clusters[[i]]
    species  <- sort(unique(x[specimen,sp.col]))
    n.sp     <- length(species)
    sp.in.clusters[[i]]    <- species
    no.sp.in.clusters[[i]] <- n.sp
  }
  
  
  homogen          <- which(no.sp.in.clusters == 1)
  heterogen        <- which(no.sp.in.clusters != 1)
  no.homogeneous   <- length(homogen)
  no.heterogeneous <- length(heterogen)
  
  # results
  out <- list(clusters=clusters,
              sp.in.clusters=sp.in.clusters,
              homogen=homogen,
              heterogen=heterogen,
              no.clusters=n.cl,
              no.homogeneous=no.homogeneous,
              no.heterogeneous=no.heterogeneous,
              sp.col=sp.col,
              threshold=th)
  class(out) <- "BCCluster"
  return(out)
}


#=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# print method              #
#=~=~=~=~=~=~=~=~=~=~=~=~=~=#
print.BCCluster <- function(x, ...) {
  cat("TaxCI cluster object:\n")
  cat("Number of clusters:", x$no.clusters, "\n")
  cat("Number of homogeneous clusters (of the same species):", x$no.homogeneous, "\n")
  cat("Number of heterogeneous clusters (containing more than one species):", x$no.heterogeneous, "\n\n")
  if (x$threshold == "external") {
    cat("Clustering was provided by the user.\n")
  } else {
    cat("Distance threshold for clustering was:", x$threshold)
  }
}
