get_geneSets_from_msigdbr <-
function(category, what, subcategory = NULL, species = "Homo sapiens") {
  message("msigdbr: ", citation("msigdbr")[[1]]$note)
  link <- citation("msigdbr")[[1]]$url
  
  ddata <- msigdbr(category = category, subcategory = subcategory, species = species)

  gs_name <- unique(ddata$gs_name)
  gs <- vector("list", length(gs_name))
  names(gs) <- gs_name
  k <- 1
  for(k in 1:length(gs)) {
    wwhich <- which(ddata$gs_name == gs_name[k])
    gs[[k]] <- unique(as.list(ddata[wwhich, what])[[1]])
    attr(gs[[k]],"link") <- link
    attr(gs[[k]],"collection") <- ifelse(!is.null(subcategory), paste(category, subcategory), category)
  }
  invisible(gs)
}
