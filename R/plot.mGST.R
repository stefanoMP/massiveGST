plot.mGST <-
function(x, gene_sets = NULL, order_by = "logit2NES", top = 30, 
                      eps = 0.25, 
                      as.network = FALSE, 
                      similarity_threshold = 1/3, 
                      manipulation = FALSE, 
                      autoResize = TRUE, ...) {
  
  if(as.network) {
    if(is.null(gene_sets)) stop("The gene-sets collection is not provided.")
    ans <- plot.network(x, gene_sets, eps = eps, similarity_threshold = similarity_threshold, manipulation = manipulation, autoResize = autoResize)
    return(ans)
  }
  
  x <- summary(x, top = top, order_by = order_by)

  x$signed_NES <- 2*(x$NES - 0.5)
  bbreaks <- seq(from = -1, to = 1, length.out = 100)
  ccolors <- colorRampPalette(c("green2", "white", "red2"))(99)
  x$NES.col <- cut(x$signed_NES, breaks = bbreaks, include.lowest = TRUE)
  levels(x$NES.col) <- ccolors
  x$NES.col <- as.character(x$NES.col)
  x$rowname <- rownames(x)
  x$rowname <- paste0(x$rowname, " (", round(x$logit2NES,2), ")")

  x <- x[rev(rownames(x)),]

  howMany <- nrow(x)
  bar_width <- 1/howMany
  spacing <- 1/(5 * howMany)
  from <- cumsum(c(0, rep(bar_width + spacing, howMany)))
  mmax <- max(from)
  from <- (from/mmax)[-(howMany+1)]
  to <- from + bar_width/mmax
#  to <- to/max(to)

  plot(c(-1, 1), c(1, 0), col = "white", main = "Enrichment plot",
       xlab = "signed NES (logit2NES in parentesis)", ylab="", yaxt = "n", bty = "n") 
  rect(0, from, x$signed_NES, to, col = as.character(x$NES.col), border = NA)
  text(0, (from+to)/2, x$rowname, adj = 0.5)
  invisible(NULL)
}
