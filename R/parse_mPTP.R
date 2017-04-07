# parse output from mPTP version 0.2.1
parse_mPTP <- function(file) {
  df <- data.frame(specimen = vector(mode="character",0),
                   cluster  = vector(mode="integer"), stringsAsFactors=F)
  
  # read lines of file
  lines <- readLines(file)
  i <- match("Species 1:", lines) # species 1
  cluster <- 0
  
  while(i <= length(lines)) {
    if (grepl("Species \\d+:$", lines[i])) {
      cluster <- cluster + 1
      i = i + 1
    } else if (grepl("^$", lines[i])) {
      i = i + 1
    } else {
      rr <- nrow(df)+1
      df[rr,1] <- lines[i]
      df[rr,2] <- cluster
      i = i + 1
    }
  }
  df$cluster <- as.integer(df$cluster)
  return(df)
}
