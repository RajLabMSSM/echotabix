#' \pkg{echolocatoR} output example: BST1 locus
#'
#' An example results file after running
#' \code{finemap_loci} on the \emph{BST1} locus.
#'
#' Data originally comes from the Parkinson's disease GWAS
#' by \href{https://www.biorxiv.org/content/10.1101/388165v3}{Nalls et al., (bioRxiv)}.
#'
#' @format data.table
#' \describe{
#'   \item{SNP}{SNP RSID}
#'   \item{CHR}{Chromosome}
#'   \item{POS}{Genomic position (in basepairs)}
#'   \item{...}{Optional: extra columns}
#' }
#' \href{https://www.biorxiv.org/content/10.1101/388165v3}{Nalls2019}
#'
#' @source
#' \code{
#' root_dir <- "~/Desktop/Fine_Mapping/Data/GWAS/Nalls23andMe_2019/BST1/Multi-finemap"
#' BST1 <- data.table::fread(file.path(root_dir, "Multi-finemap_results.txt"))
#' BST1 <- update_cols(dat = BST1)
#' BST1 <- find_consensus_SNPs(dat = BST1)
#' usethis::use_data(BST1, overwrite = TRUE)
#' }
#' @format data.table
#' @usage data("BST1")
"BST1"


#' Example results path for BST1 locus
#'
#' @source
#' \code{
#' locus_dir <- "results/GWAS/Nalls23andMe_2019/BST1"
#' usethis::use_data(locus_dir, overwrite = T)
#' }
#' @format path string
#' @usage data("locus_dir")
"locus_dir"
