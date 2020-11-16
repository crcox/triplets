## code to prepare `legacy3D` dataset goes here
load_legacy3D <- function(show.plot=TRUE, root = ".") {
  d <- read.csv(file.path(root, 'legacy3D.csv'), header = TRUE)
  str(d)
  if (show.plot==TRUE) {
    par(mfrow=c(1,2))
    plot(d$potency,d$evaluation,type = 'n')
    text(x=d$potency, y=d$evaluation, labels = d$word)
    plot(d$activity,d$evaluation,type = 'n')
    text(x=d$activity, y=d$evaluation, labels = d$word)
  }
  return(d)
}
legacy3D <- load_legacy3D(root = "./data-raw/")
usethis::use_data(legacy3D, overwrite = FALSE)
