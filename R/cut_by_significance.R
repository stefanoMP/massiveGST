cut_by_significance <-
function(ttable, level_of_significance = 0.05,
                                where = c("BH.value", "bonferroni", "p.value")) {
  
  if(is.null(ttable)) {message("Empty table."); return(NULL)} 
  if(nrow(ttable) < 1) {message("Empty table."); return(NULL)} 
  
  where <- match.arg(where)
  
  the_significant <- switch(where, 
                   BH.value = which(ttable$BH.value < level_of_significance), 
                   bonferroni = which(ttable$B.value < level_of_significance), 
                   p.value = which(ttable$p.value < level_of_significance))
  
  ttable <- ttable[the_significant,]
  
  if(is.null(ttable)) {message("No significant gene-set enriched."); ttable <- NULL}
  if(nrow(ttable) < 1) {message("No significant gene-set enriched."); ttable <- NULL} 
  
  return(ttable)
}
