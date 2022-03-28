\name{get_geneProfile}

\alias{get_geneProfile}

\title{Load a gene-profile from a txt file.}

\description{Load a gene-profile from a txt file.}

\usage{
get_geneProfile(ffile)
}

\arguments{
  \item{ffile}{a character string or a list of a character pointing to a local file}
}

\details{
The txt file contains two columuns separated by a tabulation. The first column is the gene name ( or entrez, ensembl, etc); the second column are the numeric values associated with each gene. The profile do not need to be sorted.

As an example, see the file in /massiveGST/extdata/pre_ranked_list.txt

See the path in the example below.
}

\value{A named list of numeric values.}

\author{Stefano M. Pagnotta}

\seealso{\link{pre_ranked_list}}

\examples{
fname <- system.file("extdata", package="massiveGST")
fname <- file.path(fname, "pre_ranked_list.txt")
fname
geneProfile <- get_geneProfile(fname)
class(geneProfile)
head(geneProfile)
tail(geneProfile)
}
