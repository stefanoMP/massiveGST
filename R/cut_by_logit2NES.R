cut_by_logit2NES <-
function(ttable, logit2NES_threshold = 0.58) {
  
  if(is.null(ttable)) {message("Empty table."); return(NULL)} 
  if(nrow(ttable) < 1) {message("Empty table."); return(NULL)} 
  
  alternative <- attr(ttable, "alternative")
  
  wwhich <- switch(alternative, 
                   two.sided = which(ttable$abs_logit2NES >= logit2NES_threshold), 
                   greater = which(ttable$logit2NES >= logit2NES_threshold), 
                   less = which(ttable$logit2NES <= -logit2NES_threshold))
  
  ttable <- ttable[wwhich,]
  
  if(nrow(ttable) < 1) {message("No gene-set enriched at level ", logit2NES_threshold); ttable <- NULL} 
  
  return(ttable)
}
