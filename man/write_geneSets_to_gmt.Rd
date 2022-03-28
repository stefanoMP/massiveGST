\name{write_geneSets_to_gmt}
\alias{write_geneSets_to_gmt}

\title{Save a collection of gene-sets in a .gmt file format.}

\description{Write a collection of gene sets as arranged in this package in a gmt file format.
}

\usage{
write_geneSets_to_gmt(gs, fileName)
}

\arguments{
  \item{gs}{a character vector of gene-sets}
  \item{fileName}{a character value; "gene_sets.gmt" (default)}
}

\value{No return value.}

\author{Stefano M. Pagnotta}

\seealso{\link{get_geneSets_from_msigdbr}, \link{get_geneSets_from_local_files}}

\examples{
library(massiveGST)

# get the gene-sets
geneSets <- get_geneSets_from_msigdbr(category = "H", what = "gene_symbol")

# save the gene-sets
fname <- file.path(tempdir(), "hallmarks.gmt")
write_geneSets_to_gmt(geneSets, fileName = fname)

}