save_as_tsv <-
function(x, file_name = "massiveGST.tsv", sep = "\t", ...) {
  x$gene_set <- rownames(x)
  x <- cbind(x[,ncol(x)], x[, -ncol(x)])
  colnames(x)[1] <- "gene set"
  write.table(x, file = file_name, sep = sep, quote = FALSE, row.names = FALSE)
  invisible(NULL)
}
