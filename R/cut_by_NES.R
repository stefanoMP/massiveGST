cut_by_NES <-
function(ttable, NES_threshold = 0.6) {
  
  if(is.null(ttable)) {message("Empty table."); return(NULL)} 
  if(nrow(ttable) < 1) {message("Empty table."); return(NULL)} 
  
  alternative <- attr(ttable, "alternative")
  
  wwhich <- switch(alternative, 
                   two.sided = which(ttable$NES >= NES_threshold | ttable$NES <= 1 - NES_threshold ), 
                   greater = which(ttable$NES >= NES_threshold), 
                   less = which(ttable$NES <= 1 - NES_threshold))
  
  ttable <- ttable[wwhich,]
  
#  if(nrow(ttable) < 1  | is.null(nrow(ttable))) {message("No gene-set enriched at level ", NES_threshold); invisible(NULL)} 
  if(length(wwhich) < 1) {message("No gene-set enriched at level ", NES_threshold); ttable <- NULL}
  
  return(ttable)
}
