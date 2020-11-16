quickplot <- function(X, highlight) {
  cols <- rep('grey', nrow(X))
  cols[highlight] <- 'blue'
  old <- par(mfrow=c(1,2))
  plot(X[,1],X[,2],col=cols)
  text(X[highlight,1],X[highlight,2],labels=rownames(X)[highlight])
  plot(X[,1],X[,3],col=cols)
  par(old)
}
