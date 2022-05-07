#' \pkg{Rsamtools} warning
#' 
#' Warn users about outdated version of \code{htslib} used by \pkg{Rsamtools}.
#'  
#' @source \href{https://github.com/Bioconductor/Rhtslib/issues/4}{
#' Rhtslib (which Rsamtools and seqminer depend on for tabix) 
#' is very out of date (1.7 vs. 1.15)}.
#' @source \href{https://github.com/Bioconductor/Rsamtools}{
#' Rsamtools: GitHub}
#' @source \href{https://doi.org/doi:10.18129/B9.bioc.Rsamtools}{
#' Rsamtools: Bioconductor}
#' @keywords internal
rhtslib_warning <- function(verbose=TRUE){
    rhtslib_ver <- packageVersion("Rhtslib")
    if(rhtslib_ver<"1.99.2"){
        msg <- paste( 
             "The selected method depends on Rhtslib,",
             "and the version you have installed",
             paste0("(",rhtslib_ver,")"),"contains known bugs.",
             "Please install Rhtslib >=1.99.2",
             "via Bioconductor >=3.16:\n",
             "   BiocManager::install(version='devel')\n",
             "or via GitHub:\n",
             "   remotes::install_github('Bioconductor/Rhtslib')"
             )
        stop(msg)
    }
}
