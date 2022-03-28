get_geneSets_from_local_files <-
function (ffiles) 
{
  ffunction <- function(x) {
    geneSets_tmp <- readLines(x)
    geneSets_tmp <- strsplit(geneSets_tmp, "\t")
    nnames <- sapply(geneSets_tmp, function(x) x[1])
    link <- sapply(geneSets_tmp, function(x) x[2])
    geneSets_tmp <- lapply(geneSets_tmp, function(x) x[3:length(x)])
    for (k in 1:length(geneSets_tmp)) {
      attr(geneSets_tmp[[k]], "link") <- link[k]
      attr(geneSets_tmp[[k]], "collection") <- x
    }
    names(geneSets_tmp) <- nnames
    return(geneSets_tmp)
  }
  geneSets <- ffunction(ffiles[1])
  if (length(ffiles) > 1) 
    for (k in 2:length(ffiles)) geneSets <- c(geneSets, ffunction(ffiles[k]))
  invisible(geneSets)
}
