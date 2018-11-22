makeTable <- function (plotParamTCI.result, plotParamBCC.result, sort="score", file=NA) {
  
  if (is.null(plotParamBCC.result$relevant.specimen)) {
    warning("makeTable: No suspicious specimens. No output created.")
  } else {
    
    # create output table
    f <- rep(0, length(plotParamBCC.result$relevant.specimen))
    table <- data.frame(tci=f, cl.het=f, sp.split=f, low.abun=f, other.homog=f, score=f, row.names=plotParamBCC.result$relevant.specimen)
    
    # fill the table
    table[plotParamTCI.result$specimen,                 1] <- 1
    table[plotParamBCC.result$spn.in.het.clust,         2] <- 1
    table[plotParamBCC.result$spn.in.het.clust2,        3] <- 1
    table[names(plotParamBCC.result$rel.abundance.spn), 4] <- plotParamBCC.result$rel.abundance.spn
    table[plotParamBCC.result$spn.in.split.hom.clust,   5] <- 1
    
    # calculate score
    for (i in rownames(table)) {
      table[i,"score"] <- sum(table[i,])
    }
    
    # sort and return
    if (sort != "id") {
      print(paste("relevant specimens sorted by", sort))
      table <- table[order(table[sort], decreasing=T),]
    }
    
    if (is.na(file)) {
      return(table)
    } else {
      write.table(table, file, append=F, quote=F, sep="\t", col.names = NA, row.names = TRUE)
    }
  }
}

