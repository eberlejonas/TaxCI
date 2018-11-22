#=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# internally used functions #
#=~=~=~=~=~=~=~=~=~=~=~=~=~=#
Dummies <- function (tree) {
  fac <- as.factor(tree$tip.label)
  Nlevels <- length(levels(fac))
  dummies <- diag(Nlevels)[fac,]
  rownames(dummies) <- fac
  colnames(dummies) <- levels(fac)
  return(dummies)
}

# transform a data.frame of user supplied clusters to the internal format; x = metadata
transform_user_table <- function(data, x) {
  # check the data
  if (!inherits(data, "data.frame")) {stop("data is not a data.frame")}
  if (ncol(data) != 2) {warning("data has more than two colums. Trying to use the first two.")}
  if (!all(is.wholenumber(data[,2]))) {stop("The second column of data is not whole numbers")}
  # check if data and x are consistent
  if (!all(data[,1] %in% rownames(x))) {
    print(data[(!(data[,1] %in% rownames(x))), 1])
    stop("BCCluster: above specimens of 'data' not contained in 'x'")
  }
  # transformation
  cls <- sort(unique(data[,2]))
  custom_clusters <- vector("list",length(cls))
  for (ii in cls) {
    custom_clusters[[ii]] <- as.character(data[data[,2]==ii, 1])
  }
  return(custom_clusters)
}

# check for whole numbers
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  if (is.numeric(x)) {
    abs(x - round(x)) < tol
  } else {
    FALSE
  }
}  
