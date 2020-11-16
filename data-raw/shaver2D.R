## code to prepare `shaver2D` dataset goes here
load_shaver2D <- function(show.plot=TRUE, root = '.') {
  d <- read.csv(file.path(root, 'shaver2D.csv'), header = TRUE)
  str(d)

  if (show.plot==TRUE) {
    par(mfrow=c(1,1))
    plot(d$intensity,d$evaluation,type = 'n')
    text(x=d$intensity, y=d$evaluation, labels = d$word)
  }
  return(d)
}
shaver2D <- load_shaver2D(root = "./data-raw/")
usethis::use_data(shaver2D, overwrite = TRUE)
