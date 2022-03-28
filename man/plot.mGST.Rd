\name{plot.mGST}
\alias{plot.mGST}

\title{Graphical rendering of the enrichment analysis.}
\description{This function displays the enrichment analysis results both as a bar-plot and a network of gene-sets.}
\usage{
\method{plot}{mGST}(x, 
  gene_sets = NULL, 
  order_by = "logit2NES", 
  top = 30, 
  eps = 0.25, 
  as.network = FALSE, 
  similarity_threshold = 1/3, 
  manipulation = FALSE, 
  autoResize = TRUE, 
  ...
)
}

\arguments{
  \item{x}{a data structure coming from the massiveGST function}
  
  \item{gene_sets}{a character vector of gene-sets; mandatory for the network display}
  \item{order_by}{a character string specifying whick should be the ordering in the bar-plot; must be one of "relevance", "NES", "logit2NES" (default), "p.value", "BH.value", and "bonferroni". These are the same options of \link{summary.mGST}}
  \item{top}{an integer value controlling how many gene-sets have to be displaued in the bar-plot; top = 30 (default)}
  \item{as.network}{a logical value to switch to a network display; as.network = FALSE (default)}
  \item{similarity_threshold}{a real value to cut the similarities between gene-stes below this value; similarity_threshold = 1/3 (default)}
  \item{eps}{a real value between 0.0 and 1.0 controlling the contribution of the Jaccard and overlap similaties to their convex combination; eps = 0.25 (default), see details.}
  \item{manipulation}{a logical value allowing to manipulate the network; manipulation = FALSE (default); see \link{visOptions}}
  \item{autoResize}{a logical value allowing to resize the network; resize = TRUE (default); see \link{visOptions}}
  \item{\dots}{other graphical parameters}
}
\details{This function display the results of enrichment analysis both as a bar-plot and a network.

The network rendering is with the visNetwork package. 

The similarity between the gene-set is computed a convex combination of the Jaccard and overlap similarities. See the reference for further details.}
\value{In the case of network display, an object from the visNetwork package.
}

\references{Cerulo, Pagnotta (2021) \doi{10.1101/2021.02.15.431228}}

\author{Stefano M. Pagnotta}

\seealso{\link{massiveGST}, \link{visNetwork}, \link{visOptions}
}

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

# to get the bar-plot
plot(cut_by_significance(ans, level_of_significance = 0.01))

# to get the network of the gene-sets
plot(cut_by_significance(ans, level_of_significance = 0.01), 
     gene_sets = geneSets, as.network = TRUE)
}
