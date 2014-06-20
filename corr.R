## Require getmonitor and Complete functions

corr <- function(directory = getwd(), threshold = 0) {
  corrsNum <- numeric(0)
  nobsDfr <- complete("specdata")
  nobsDfr <- nobsDfr[nobsDfr$nobs > threshold, ]
  for (cid in nobsDfr$id) {
    monDfr <- getmonitor(cid, directory)
    corrsNum <- c(corrsNum, cor(monDfr$sulfate, monDfr$nitrate, use = "pairwise.complete.obs"))
  }
  return(corrsNum)
}