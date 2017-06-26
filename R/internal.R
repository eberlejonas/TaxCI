#=~=~=~=~=~=~=~=~=~=~=~=~=~=#
# internally used functions #
#=~=~=~=~=~=~=~=~=~=~=~=~=~=#
Dummies <- function (tree, outgroup) {
  tree <- drop.tip(tree, tip=outgroup)
  fac <- as.factor(tree$tip.label)
  Nlevels <- length(levels(fac))
  dummies <- diag(Nlevels)[fac,]
  rownames(dummies) <- fac
  colnames(dummies) <- levels(fac)
  return(dummies)
}

# consistency index and retention index: #
#   loop through all branches and
#   check if character state changed; if yes, increase observed steps (os) by current steps
CI <- function (tree, x, outgroup, reconstruction="lower") {
  # TO DO: check here for correct coding of character states (maybe convert to factor)
  ms <- max(x) - min(x) # always correct?
  # calculate most parsimonious reconstruction and concatenate with tip values
  mpr <- MPR(x, unroot(tree), outgroup=outgroup)
  tree <- drop.tip(tree, tip=outgroup)
  states <- c(x[tree$tip.label], mpr[,reconstruction])
  names(states) <- 1:length(states)
  # calculate observed steps
  os <- 0
  for (i in 1:nrow(tree$edge)) {
    if (states[tree$edge[i,1]] != states[tree$edge[i,2]]) {
      os <- os + abs(states[tree$edge[i,1]] - states[tree$edge[i,2]])
    }
  }
  names(os) <- NULL
  ci <- ms/os
  g <- length(tree$tip.label)*ms # the maximum possible no. of steps/homoplasy
  ri <- (g - os) / (g - ms)
  return(list(observed.steps=os, minimum.steps=ms, CI=ci, RI=ri))
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
