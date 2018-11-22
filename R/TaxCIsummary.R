TaxCIsummary <- function (tree, x, sp.col="binomial", BCCluster.result, plotParamTCI.result, plotParamBCC.result, analysis="TaxCI.analysis", file=NA) {
  if (!inherits(BCCluster.result, "BCCluster")) {stop("TaxCIsummary: BCCluster.result is not of class BCCluster\n")}
  
  result <- data.frame()
  
  no.msp <- noMorphospecies(metadata=x, column=sp.col, tree=tree)
  result["noMorphospecies",analysis]    <- no.msp
  result["no.tci.positive.sp",1]  <- length(plotParamTCI.result$species)
  result["no.cluster",1]          <- BCCluster.result$no.clusters
  result["no.homogen",1]          <- BCCluster.result$no.homogeneous
  result["no.heterogen",1]        <- BCCluster.result$no.heterogeneous
  result["no.species.in.multiple.clusters",1]     <- length(which(plotParamBCC.result$species.in.n.clusters > 1))
  result["no.species.in.multiple.hom.clusters",1] <- length(which(plotParamBCC.result$splitted.species.homogen > 1))
  if (BCCluster.result$threshold == "external") {
    result["threshold",1]         <- "external"
  } else {
    result["threshold",1]           <- as.character(round(BCCluster.result$threshold, 4))
  }
  
  m.ratio <- (2*(BCCluster.result$no.homogeneous - length(which(plotParamBCC.result$splitted.species.homogen > 1)))) / 
    (no.msp + BCCluster.result$no.clusters)
  
  result["match ratio",1] <- as.character(round(m.ratio, 2))
  
  if (is.na(file)) {
    return(result)
  } else {
    write.table(result, file, append=F, quote=F, sep="\t", col.names = NA, row.names = TRUE)
  }
}
