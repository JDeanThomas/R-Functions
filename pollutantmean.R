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