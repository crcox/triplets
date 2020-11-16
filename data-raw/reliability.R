## code to prepare `reliability` dataset goes here
load_reliability <- function(root = ".") {
  d <- read.csv(file.path(root, 'reliability.csv'))[,c(2:5,8,15:47)]
  names(d)[1:4] <- c('condition','target','opt1','opt2')
  x <- rowMeans(d[,6:ncol(d)])
  d <- d[,c('condition','target','opt1','opt2','MajorityResponse')]
  # The proportion of subjects that comprise the majority
  d$majorityP <- x
  return(d)
}
reliability <- load_reliability("./data-raw/")
usethis::use_data(reliability, overwrite = TRUE)
