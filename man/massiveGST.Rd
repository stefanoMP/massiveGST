\name{massiveGST}
\alias{massiveGST}

\title{massive Gene-Sets Test with Mann-Whitney-Wilcoxon statistics.}
\description{Perform a competitive gene set enrichment analysis by applying the Mann-Withney-Wilcoxon test.
}
\usage{
massiveGST(gene_profile, gene_sets, 
  cols_to_remove = NULL, 
  alternative = c("two.sided", "less", "greater")
  )
}

\arguments{
  \item{gene_profile}{a named list of values; the names have to match the names fo genes in the gene-set.}
  \item{gene_sets}{a character vector of gene-sets}
  \item{cols_to_remove}{a list of colnames to eventually remove from the output}
  \item{alternative}{a character string specifying the alternative hypothesis of the MWW test;  must be one of "two.sided" (default), "greater" or "less".}
}

\value{A data frame with columns
\item{size}{Original size of the gene-set}
\item{actualSize}{Size of the gene-set after the match with the gene-profile}
\item{NES}{(Normalized Enrichment Score) the strength of the association of the gene-set with the gene profile; also the percentile rank of the gene-set in the universe of the genes ouside the gene-set.}
\item{odd}{odd transformation of the NES}
\item{logit2NES}{logit transformation of the NES}
\item{abs_logit2NES}{absolute value of the logit2NES in the case of "two.sided" alternative}
\item{p.value}{p-values associated with the gene-set}
\item{BH.value}{Benijamini and Hockberg adjustment of the p.values}
\item{B.value}{Bonferroni adjustment of the p.values}
\item{relevance}{marginal ordering of the table}
}
\references{Cerulo, Pagnotta (2021) \doi{10.1101/2021.02.15.431228}}
\author{Stefano M. Pagnotta}

\seealso{\link{summary.mGST}, \link{plot.mGST}, \link{cut_by_logit2NES}, \link{cut_by_NES}, \link{cut_by_significance}}
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

ans

}