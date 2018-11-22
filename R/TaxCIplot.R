TaxCIplot <- function (tree, x, plotParamTCI.result, plotParamBCC.result=NA, 
                    box.placement="auto", adj=.515, step=0.01, label.offset=0.08,
                    npages=1, draft=F, vis.clusters=TRUE, info="", legend=FALSE, alternative.tip.labels=NULL, ...) {
  
  # check if tree and x are consistent and re-sort x
  if (!all(tree$tip.label %in% rownames(x))) {
    problems <- paste(tree$tip.label[-which(tree$tip.label %in% rownames(x))], collapse="\n")
    stop("TaxCIplot Error: some treelabels are not contained in 'x':\n", problems)
  }
  x <- x[tree$tip.label,]
  
  
  # for multipageplotting
  ntips <- length(tree$tip.label)
  tips.per.page <- as.integer(ntips / npages) +1 # whole number plus 1
  y1 <- ntips - tips.per.page -.2
  y2 <- ntips +.2
  
  
  # cluster nodes
  if (!is.na(plotParamBCC.result[1])) {
    if (is.logical(vis.clusters)) {
      if (vis.clusters) {
        if (!("mrca.clusters" %in% names(plotParamBCC.result) )) {stop("TaxCIplot Error: plotParamBCC.result$mrca.clusters does not exist for use with vis.clusters!")}
        cluster.nodes <- plotParamBCC.result$mrca.clusters
      }
    } else {
      if (!is.numeric(vis.clusters)) {stop("TaxCIplot Error: vis.clusters must be either logical or a numeric vector!")}
      node_range <- (ntips+1) : ((2*ntips)-1)
      if (!(all(vis.clusters %in% node_range))) {stop("TaxCIplot Error: vis.clusters must contain numbers in the range '(ntips+1) : ((2*ntips)-1)'!")}
      cluster.nodes <- vis.clusters
    }
  }
  

  
  
  # set alternative tip labels; original tips.label is not needed anymore
  if (!is.null(alternative.tip.labels)) {
    if (length(alternative.tip.labels) != ntips) {stop("TaxCIplot Error: Please provide a vector of mode character of expression of the same length as tree$tip.label")}
    tree$tip.label <- alternative.tip.labels
  }
  
  
  if (draft) {
    if (npages > 3) {npages <- 3}
  }
  
  if (box.placement == "auto") {
    # plot the tree invisibly to get extends
    plotParameters <- plot.phylo(tree, plot=F, label.offset=0, ...)
    
    adj  <- .5 + strwidth("x") * 1.75
    step <- strwidth("x") * 1.075
    w    <- plotParameters$x.lim
    w[2] <- w[2]*1.4
    label.offset = strwidth("x") * 7.55
    par(new=TRUE)
  }
  
  
  for (i in 1:npages) {
    # plot the tree (partially)
    plot.phylo(tree, x.lim = w, y.lim = c(y1, y2), label.offset=label.offset, ...)
    par(new=FALSE)
    # dots on cluster nodes
    if (exists("cluster.nodes")) {nodelabels(node=cluster.nodes, pch=19, cex=.4, col="black", bg="black")}
    
    
    if (legend){
      text <- c("1. TCI < 1",
                "2. Containing cluster heterogen...",
                "3. ... and species in more than one cluster",
                "4. Species with low abundance in cluster",
                "5. Species in other homogeneous clusters too")
      legend("topright", legend=text, pt.bg=c("#FF5555","#FF5555","#5CACEE","#5CACEE","#B3EE3A"), col="grey84", pch=22, xpd=NA, cex=.8, pt.lwd=.5, bty="n", pt.cex=1.2)
    }
    
    y1 <- y1 - tips.per.page
    y2 <- y2 - tips.per.page
    
    # TaxCIplot marks
    tiplabels(tip=plotParamTCI.result$tips[-plotParamTCI.result$i], adj=adj, pch=22, lwd=.5, bg="white", col="grey84")
    tiplabels(tip=plotParamTCI.result$i,                            adj=adj, pch=22, lwd=.5, bg="#FF5555", col="grey84")
    
    if (typeof(plotParamBCC.result) == "list") {
      # plot cl1 marks
      s <- step
      tiplabels(tip=plotParamBCC.result$tips[-plotParamBCC.result$i], adj=adj+s, pch=22, lwd=.5, bg="white", col="grey84")
      tiplabels(tip=plotParamBCC.result$i,                            adj=adj+s, pch=22, lwd=.5, bg="#FF5555", col="grey84")
      s <- s+step
      
      # plot cl2 marks
      tiplabels(tip=plotParamBCC.result$tips[-plotParamBCC.result$j], adj=adj+s, pch=22, lwd=.5, bg="white", col="grey84")
      tiplabels(tip=plotParamBCC.result$j,                            adj=adj+s, pch=22, lwd=.5, bg="#5CACEE", col="grey84")
      s <- s+step
      
      # plot cl3 marks
      tiplabels(tip=plotParamBCC.result$tips[-plotParamBCC.result$k], adj=adj+s, pch=22, lwd=.5, bg="white", col="grey84")
      tiplabels(tip=plotParamBCC.result$k,                            adj=adj+s, pch=22, lwd=.5, bg="#5CACEE", col="grey84")
      s <- s+step
      
      # plot cl4 marks
      tiplabels(tip=plotParamBCC.result$tips[-plotParamBCC.result$l], adj=adj+s, pch=22, lwd=.5, bg="white", col="grey84")
      tiplabels(tip=plotParamBCC.result$l,                            adj=adj+s, pch=22, lwd=.5, bg="#B3EE3A", col="grey84")
    }
    
    # print analyses informations
    if (is.character(info)) {
      mtext(info, 4, 0, las=3, adj = 0)
      #       legend("topright", info,bty="n", xjust=1, )
    } else {
      stop("TaxCIplot Error: Please provide a character string as argument 'info'.")
    }
  }
}
