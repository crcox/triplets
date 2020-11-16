compare_with_majority <- function(reliability, embeddings) {
  reliability$target <- as.character(reliability$target)
  reliability$opt1 <- as.character(reliability$opt1)
  reliability$opt2 <- as.character(reliability$opt2)
  reliability$MajorityResponse <- as.character(reliability$MajorityResponse)
  reliability$majority <- ifelse(reliability$MajorityResponse == reliability$opt1, 1, 2)
  
  M <- matrix(FALSE, nrow=nrow(reliability), ncol = length(embeddings))
  colnames(M) <- names(embeddings)
  for (i in 1:nrow(reliability)) {
    w <- c(reliability$target[i], reliability$opt1[i], reliability$opt2[i])
    for (j in 1:length(embeddings)) {
      E <- embeddings[[j]]
      e <- rbind(
          as.numeric(E[E$word==w[1],2:ncol(E)]),
          as.numeric(E[E$word==w[2],2:ncol(E)]),
          as.numeric(E[E$word==w[3],2:ncol(E)])
      )
      d <- dist(e)
      M[i,j] <- ifelse(d[1] < d[2],1,2) == reliability$majority[i]
    }
  }
  return(M)
}
