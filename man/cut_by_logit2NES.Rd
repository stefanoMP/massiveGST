\name{cut_by_logit2NES}

\alias{cut_by_logit2NES}

\title{Trim the table of results.}

\description{This function trims the table of results from massiveGST function retaining the rows with a logit2NES below the specified threshold.}

\usage{cut_by_logit2NES(ttable, logit2NES_threshold = 0.58)}

\arguments{
  \item{ttable}{a data frame of "mGST" class coming from massiveGST function. }
  \item{logit2NES_threshold}{a real value}
}

\value{A data frame.}

\references{Cerulo, Pagnotta (2021) \doi{10.1101/2021.02.15.431228}}

\author{Stefano M. Pagnotta}

\note{the functions cut_by_NES, cut_by_logit2NES, and cut_by_significance can be nested.}

\seealso{\link{massiveGST},  \link{cut_by_NES}, \link{cut_by_significance}, 

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
ans <- massiveGST(geneProfile, geneSets, alternative = "two.sided")

head(ans)

cut_by_logit2NES(ans)
cut_by_logit2NES(cut_by_significance(ans))

plot(cut_by_logit2NES(ans))

}
