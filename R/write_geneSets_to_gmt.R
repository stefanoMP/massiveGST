write_geneSets_to_gmt <-
function(gs, fileName = "gene_sets.gmt") {
  gs_name <- names(gs)
  link  <- unlist(lapply(gs, function(x) attr(x, "link")))
  geneSet <- lapply(gs, function(x) paste0(x, collapse = '\t'))
  tmp <- paste(gs_name, link, geneSet, sep = "\t")
  cat(tmp[1], file = fileName, append = FALSE, sep = "\n")
  for(k in 2:length(tmp)) cat(tmp[k], file = fileName, append = TRUE, sep = "\n")
  invisible(NULL)
}
