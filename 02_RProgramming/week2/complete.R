complete <- function(directory = "specdata", id) {
  final <- c(NULL, NULL)
  for (x in id) {
    new_x <- formatC(x, width=3, flag="0")
    file_name <- paste(directory, "/", new_x, ".csv", sep = "")
    dx <- read.csv(file_name)
    good <- complete.cases(dx)
    if (TRUE %in% good) {
      tab <- table(good)
      cases <- tab[names(tab) == TRUE]
      vector <- c(new_x, cases)
      final <- rbind(final, vector)
    } else {
      final <- rbind(final, c(new_x, 0))
    }
  }
  final <- data.frame(final)
  colnames(final) <- c("id", "nobs")
  final
}
