### code to prepare `legacy2D` dataset goes here
load_legacy2D <- function(show.plot=TRUE, root = ".") {
  d <- read.csv(file.path(root, 'legacy2D.csv'), header = TRUE)
  str(d)
  if (show.plot==TRUE) {
    par(mfrow=c(1,1))
    plot(d$intensity,d$evaluation,type = 'n')
    text(x=d$intensity, y=d$evaluation, labels = d$word)
  }
  return(d)
}
legacy2D <- load_legacy2D(root = "./data-raw/")
usethis::use_data(legacy2D, overwrite = FALSE)
