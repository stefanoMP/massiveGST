\name{cut_by_NES}

\alias{cut_by_NES}

\title{Trim the table of results.}

\description{This function trims the table of results from massiveGST function retaining the rows with a NES below the specified threshold.}
\usage{cut_by_NES(ttable, NES_threshold = 0.6)}

\arguments{
  \item{ttable}{a data frame of 'mGST' class coming from massiveGST function. }
  \item{NES_threshold}{a real value between 0.0 and 1.}
}

\value{A data frame.}

\references{Cerulo, Pagnotta (2021) \doi{10.1101/2021.02.15.431228}}

\author{Stefano M. Pagnotta}

\note{the functions cut_by_NES, cut_by_logit2NES, and cut_by_significance can be nested. In the case the test has alternative = 'two.sided', it is better to use cut_by_logit2NES for a symmetric trim of both directions.}

\seealso{\link{massiveGST}, \link{cut_by_logit2NES},  \link{cut_by_significance}, 
\link{summary.mGST}, \link{plot.mGST}}

\examples{
library(massiveGST)

# get the gene profile
fname <- system.file("extdata", package="massiveGST")
fname <- file.path(fname, "pre_ranked_list.txt")
geneProfile <- get_geneProfile(fname)

# get the gene-sets
geneSets <- get_geneSets_from_msigdbr(category = "H", what = "gene_symbol")

# run the function
ans <- massiveGST(geneProfile, geneSets, alternative = "greater")

head(ans)
cut_by_NES(ans, NES_threshold = .65)
summary(cut_by_NES(ans, NES_threshold = .65))

}