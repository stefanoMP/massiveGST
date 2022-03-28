geneSets.sim <-
function(gs, eps = 0.25) {
  m <- length(gs)
  q <- m * (m - 1)/2
  ans <- rep(0, q)
  j <- 1
  for(k in 1:(m-1))
    for(kk in (k+1):m) {
      ans[j] <- length(intersect(gs[[k]], gs[[kk]]))
      d_overlap <- min(length(gs[[k]]), length(gs[[kk]]))
      d_jaccard <- length(union(gs[[k]], gs[[kk]]))
      ans[j] <- eps * ans[j]/d_overlap + (1-eps) * ans[j]/d_jaccard
      j <-  j + 1
      }

  attr(ans, "Size") <- m
  attr(ans, "Labels") <- colnames(gs)
  attr(ans, "Diag") <- FALSE    
  attr(ans, "method") <- "convex combination"
  class(ans) <- "dist"
  return(ans)
}
