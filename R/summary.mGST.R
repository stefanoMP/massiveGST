summary.mGST <-
function(object, cols_to_remove = "link", 
                         order_by = c("relevance", "NES", "logit2NES", "p.value", "BH.value", "bonferroni"), 
                         top = NULL, 
                         as.formattable = FALSE, ...) {
  
  alternative <- attr(object, "alternative")
  order_by <- match.arg(order_by)
  
  if(order_by == "relevance") object$order <- order(object$relevance, decreasing = TRUE)
  if(order_by == "NES") object$order <- order(object$NES, decreasing = TRUE)
  if(order_by == "logit2NES") object$order <- order(object$NES, decreasing = TRUE)
  if(order_by == "p.value") object$order <- order(object$p.value)
  if(order_by == "BH.value") object$order <- order(object$BH.value)
  if(order_by == "bonferroni") object$order <- order(object$B.value)
  
#  oorder <- switch (order_by,
#               relevance = order(x$relevance, decreasing = TRUE),
#               NES = order(x$NES, decreasing = TRUE),
#               p.value = order(x$p.value),
#               BH.value = order(x$BH.value),
#               bonferroni = order(x$B.value)
#  )
 
  tmp <- object[object$order,]
  
  tmp$NES <- round(tmp$NES, 4)
  tmp$odd <- round(tmp$odd, 4)
  tmp$logit2NES <- round(tmp$logit2NES, 4)
  if(alternative == "two.sided") tmp$abs_logit2NES <- round(tmp$abs_logit2NES, 2)
  tmp$p.value <- format(tmp$p.value, digits = 2)
  tmp$BH.value <- format(tmp$BH.value, digits = 2)
  tmp$B.value <- format(tmp$B.value, digits = 2)
  
  cols_to_remove <- c(cols_to_remove, "order", "abs_logit2NES", "odd")
  
  if(order_by != "relevance") cols_to_remove <- c(cols_to_remove, "relevance")
  
  if(!is.null(cols_to_remove)) tmp <- tmp[, -which(colnames(tmp) %in% cols_to_remove)]
  
  if(!is.null(top) & is.numeric(top)) {
    top <- as.integer(top)
    above <- min(top, sum(tmp$logit2NES > 0))
    below <- min(top, sum(tmp$logit2NES < 0))
    nnrow <- nrow(tmp)
    if(alternative == "two.sided") {
      top <- trunc(top/2)
      tmp <- tmp[c(1:above, (nnrow-below+1):nnrow),]
      } else 
        if(alternative == "greater") tmp <- tmp[1:above,] else 
          tmp <- tmp[(nnrow-below+1):nnrow,]
  }
  
#  if(!verbose) return(tmp)
  if(as.formattable) formattable(tmp, 
    list(logit2NES = formatter("span", style = ~ style("color" = ifelse(logit2NES >= 0, "green", "red"))),
         NES = color_bar("lightgreen"))) else {
#           class(tmp) <- class(tmp)[-1]
           return(tmp)
         }
}
