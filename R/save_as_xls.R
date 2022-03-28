save_as_xls <-
function(x, file_name = "massiveGST.xls", ...) {
  x$gene_set <- rownames(x)
  x <- cbind(x[,ncol(x)], x[, -ncol(x)])
  colnames(x)[1] <- "gene set"
  WriteXLS(x, ExcelFileName = file_name, AdjWidth = TRUE, BoldHeaderRow = TRUE, SheetNames = "massiveGST")
  invisible(NULL)
}
