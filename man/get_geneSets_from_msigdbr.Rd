\name{get_geneSets_from_msigdbr}

\alias{get_geneSets_from_msigdbr}

\title{Get the gene-sets from the msigdbr package.}

\description{This is a wrapper for extraction a gene-sets collection as a vector list to match the data structure for massiveGST function.
}

\usage{
get_geneSets_from_msigdbr(category, what, subcategory = NULL, species = "Homo sapiens")
}
\arguments{
  \item{category}{MSigDB collection abbreviation, such as H or C1.
}
  \item{what}{a character string specifying the code representation of the genes; must be one of  "gene_symbol", "entrez_gene", "ensembl_gene", "human_gene_symbol", "human_entrez_gene", "human_ensembl_gene"; 
}
  \item{subcategory}{MSigDB sub-collection abbreviation, such as CGP or BP; NULL (default)
}
  \item{species}{Species name, such as 'Homo sapiens' or 'Mus musculus'.
}
}

\value{A vector list of gene-sets}

\author{Stefano M. Pagnotta}

\seealso{\link{msigdbr}
}

\examples{
library(massiveGST)

# get the gene-sets
geneSets <- get_geneSets_from_msigdbr(category = "H", what = "gene_symbol")

class(geneSets)
head(geneSets, 3)
}