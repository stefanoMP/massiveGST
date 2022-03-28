get_geneProfile <-
function (ffile) {
  tmp <- read.delim(ffile, header = FALSE, row.names = 1)
  ans <- tmp[, 1]
  names(ans) <- rownames(tmp)
  invisible(ans)
}
