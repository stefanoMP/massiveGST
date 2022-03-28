massiveGST <-
function(gene_profile, gene_sets, cols_to_remove = NULL,
                 alternative = c("two.sided", "less", "greater")) {
  
  alternative <- match.arg(alternative)
  
  collection <- unlist(sapply(gene_sets, function(x) attr(x, "collection")))
  if(is.null(collection)) collection <- rep(NA, length(gene_sets))
  link <- unlist(sapply(gene_sets, function(x) attr(x, "link")))
  size <- unlist(sapply(gene_sets, length))
 
  gene_sets <- lapply(gene_sets, function(x) intersect(x, names(gene_profile)))
  actualSize <- unlist(sapply(gene_sets, length))
  result <- data.frame(collection, size, actualSize)
  
  rnk <- rank(gene_profile)
  sumOfRanks <- sapply(gene_sets, function(x) sum(rnk[x]))
  n <- length(rnk)
  nx <- actualSize
  ny <- n - nx
  U_stat <- nx * ny + ny * (ny + 1)/2 + sumOfRanks - n * (n + 1)/2
  
  result$NES <- pod <- U_stat/nx/ny 
  result$odd <- odd <- pod/(1-pod)
  result$logit2NES <- logit2NES <- log2(odd)
  
  if(alternative == "two.sided") result$abs_logit2NES <- abs(logit2NES)
  
  zValue <- U_stat - nx * ny/2
  
  sigma <- sqrt(nx * ny * (n + 1)/12)
  result$zValue <- zValue <- zValue/sigma
  
  pValue <- switch(alternative, 
                   less = pnorm(zValue), 
                   greater = pnorm(zValue, lower.tail = FALSE), 
                   two.sided = 2 * pnorm(abs(zValue), lower.tail = FALSE))
  
  result$p.value  <- pValue
  result$BH.value <- p.adjust(pValue, method = "BH") # fdr
  result$B.value  <- p.adjust(pValue, method = "bonferroni") # fdr
  
  rowsToRemove <- which(result$actualSize == 0)
  if(length(rowsToRemove) > 0) {
    result <- result[-rowsToRemove,]
    link <- link[-rowsToRemove]
  }
  
  positive <- which(result[, "logit2NES"] > 0)
  order_p <- rank(result[positive, "logit2NES"]) + rank(result[positive, "actualSize"]) + rank(1-result[positive, "p.value"])
  names(order_p) <- rownames(result)[positive]
  
  negative <- which(result[, "logit2NES"] <= 0)
  order_n <- rank(-result[negative, "logit2NES"]) + rank(result[negative, "actualSize"]) + rank(1 - result[negative, "p.value"])
  names(order_n) <- rownames(result)[negative]
  
  result$relevance <- relevance <- c(order_p, -order_n)[rownames(result)]
  
  if(alternative == "less") result$relevance <- -result$relevance
  
  result$link <- link
  
  result <- result[order(result$relevance, decreasing = TRUE),]
  
  cols_to_remove <- c(cols_to_remove, "zValue")
   
  if(length(table(result$link)) <= 1) cols_to_remove <- c(cols_to_remove, "link") else {
    result$link <- as.character(result$link)
    class(result$link) <- "hyperlink"
  }

  if(length(table(result$collection)) <= 1) cols_to_remove <- c(cols_to_remove, "collection")
  
  result <- result[, -which(colnames(result) %in% cols_to_remove)]
  
  class(result) <- c("mGST", "data.frame")
  
  attr(result, "alternative") <- alternative
  invisible(result)
}
