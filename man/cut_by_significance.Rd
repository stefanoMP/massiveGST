\name{cut_by_significance}

\alias{cut_by_significance}

\title{Trim the table of results.}

\description{This function trims the table of results from massiveGST function according to the significance required.}
\usage{cut_by_significance(ttable, 
  level_of_significance = 0.05, 
  where = c("BH.value", "bonferroni", "p.value")
)
}

\arguments{
  \item{ttable}{a data frame of "mGST" class coming from massiveGST function. }
  \item{level_of_significance}{a real value between 0.0 and 1.}
  \item{where}{a character string specifying where the level_of_significance has to be applied to the output;  must be one of "p.value", "BH.value" (default), and "bonferroni"}
}

\details{BH.value is the adjustment of p-values according to Benijamini and Hockberg's method;  B.value is the adjustment of p-values according to Bonferroni's method.}

\value{A data frame.}

\references{Cerulo, Pagnotta (2021) \doi{10.1101/2021.02.15.431228}}

\author{Stefano M. Pagnotta}

\note{the functions cut_by_NES, cut_by_logit2NES, and cut_by_significance can be nested.}

\seealso{\link{massiveGST}, \link{cut_by_logit2NES}, \link{cut_by_NES},  
\link{summary.mGST}, \link{plot.mGST}}

\examples{library(massiveGST)

# get the gene profile
fname <- system.file("extdata", package="massiveGST")
fname <- file.path(fname, "pre_ranked_list.txt")
geneProfile <- get_geneProfile(fname)

# get the gene-sets
geneSets <- get_geneSets_from_msigdbr(category = "H", what = "gene_symbol")

# run the function
ans <- massiveGST(geneProfile, geneSets, alternative = "two.sided")

head(ans)
cut_by_significance(ans)

cut_by_significance(ans, level_of_significance = 0.05, where = "p")
cut_by_logit2NES(cut_by_significance(ans))

summary(cut_by_significance(ans, level_of_significance = 0.05, where = "bonferroni"))

plot(cut_by_significance(ans, level_of_significance = 0.05, where = "bonferroni"))

}