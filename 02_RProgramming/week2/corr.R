corr <- function(directory, threshold = 0) {
  final <- numeric()
  for (x in 1:332) {
    number <- complete(directory, x)
    num_nobs <- number$nobs
    class(num_nobs) <- "numeric"
    if (num_nobs > threshold) {
      new_x <- formatC(x, width=3, flag="0")
      file_name <- paste(directory, "/", new_x, ".csv", sep = "")
      dx <- read.csv(file_name)
      new_dx <- dx[complete.cases(dx), ]
      sul <- new_dx[["sulfate"]]
      class(sul) <- "numeric"
      nit <- new_dx[["nitrate"]]
      class(nit) <- "numeric"
      my_correlation <- cor(sul, nit)
      final <- c(final, my_correlation) 
    }
  }
  final
}

