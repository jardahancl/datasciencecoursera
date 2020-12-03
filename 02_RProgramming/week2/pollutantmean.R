pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
  vector <- NULL
  for (x in id) {
    new_x <- formatC(x, width=3, flag="0")
    file_name <- paste(directory, "/", new_x, ".csv", sep = "")
    dx <- subset(read.csv(file_name))
    vector <- c(vector, dx[[pollutant]])
  }
  m <- mean(vector, na.rm = TRUE)
  m
}




