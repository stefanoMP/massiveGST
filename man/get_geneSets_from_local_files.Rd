\name{get_geneSets_from_local_files}

\alias{get_geneSets_from_local_files}

\title{Load the gene-sets collection from local gmt files}

\description{Load the gene-sets collection from local gmt files}

\usage{get_geneSets_from_local_files(ffiles)}

\arguments{
  \item{ffiles}{a character string or a list of a character pointing to local files}
}

\value{A vector list of gene-sets}

\author{Stefano M. Pagnotta}

\seealso{\link{get_geneSets_from_msigdbr}, \link{write_geneSets_to_gmt}
}

\examples{library(massiveGST)

tmp <- get_geneSets_from_msigdbr(category = "H", what = "gene_symbol")

fname1 <- file.path(tempdir(), "h1.gmt")
write_geneSets_to_gmt(tmp, fileName = fname1)

fname2 <- file.path(tempdir(), "h2.gmt")
write_geneSets_to_gmt(tmp, fileName = fname2)

# getting one collection
geneSets <- get_geneSets_from_local_files(fname1)
length(geneSets)

# getting two collections
geneSets <- get_geneSets_from_local_files(c(fname1, fname2))
length(geneSets)

}
