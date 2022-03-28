\name{summary.mGST}
\alias{summary.mGST}

\title{Generate summary tables}
\description{This method handles the result of massiveGST function, to provide views of the table.}
\usage{
\method{summary}{mGST}(object, 
  cols_to_remove = "link", 
  order_by = c("relevance", "NES", "logit2NES", "p.value", "BH.value", "bonferroni"), 
  top = NULL, 
  as.formattable = FALSE, 
  ...
)
}

\arguments{
  \item{object}{a data structure coming from the massiveGST function}
  \item{cols_to_remove}{A character list of the columns to remove from the output.}
  \item{order_by}{a character string specifying which marginal ordering has to be applied to the output;  must be one of "relevance" (default), "NES", "logit2NES", "p.value", "BH.value", and "bonferroni"}
  \item{top}{an integer to trim the table to the first 'top' rows.}
  \item{as.formattable}{a logical value (default = FALSE) to provide a formatted output with the help of formattable package.}
  \item{...}{Arguments to be passed to methods}
}

\value{A data frame.}
\author{Stefano M. Pagnotta}

\seealso{\link{massiveGST}}
\examples{
library(massiveGST)

# get the gene profile
fname <- system.file("extdata", package="massiveGST")
fname <- file.path(fname, "pre_ranked_list.txt")
geneProfile <- get_geneProfile(fname)

# get the gene-sets
geneSets <- get_geneSets_from_msigdbr(category = "H", what = "gene_symbol")

# run the function
ans <- massiveGST(geneProfile, geneSets, alternative = "two.sided")

summary(ans)
summary(ans, as.formattable = TRUE, order_by = "NES", top = 10)

}