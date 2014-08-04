## 1 pollutantmeans functions

pollutantmeans <- function(directory = getwd(), pollutant, id = 1:332) {  
  files <- list.files(pattern = ".csv")
  data <- lapply(files, read.csv)
  data = do.call(rbind.data.frame,data)
  selected.files = files[match(id,data)]
  mean(data[, pollutant], na.rm=TRUE)
}

## or

pollutantmean <- function(directory, pollutant, id = 1:332) {
  data = numeric()
  for (i in id) {
    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = ""))
    data = c(data, newRead[[pollutant]])
  }
  return(mean(data, na.rm = TRUE))
}

## 2 complete function

## Requires getmonitor function

complete <- function(directory, id = 1:332) {
  nobsNum <- numeric(0)
  for (cid in id) {
    cDfr <- getmonitor(cid, directory)
    nobsNum <- c(nobsNum, nrow(na.omit(cDfr)))
  }
  data.frame(id = id, nobs = nobsNum)
}

getmonitor <- function(id, directory, summarize = FALSE) {
  fileStr <- paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", sep = "")
  rawDfr <- read.csv(fileStr)
  if (summarize) {
    print(summary(rawDfr))
  }
  return(rawDfr)
}

## 3 function = corr

## Requires getmonitor and Complete functions

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


}
