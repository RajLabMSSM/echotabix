#' \pkg{Rsamtools} warning
#' 
#' Warn users about outdated version of \code{htslib} 
#' used by older versions of the \pkg{Rhtslib} R package.
#'  
#' @param rhtslib_pkgs List of R packages that depend on \code{Rhtslib}.
#' @param method Method requested.
#' @param verbose Print messages. 
#' 
#' @source  \href{https://github.com/Bioconductor/Rsamtools/issues/33#}{
#' Rsamtools/Rhtslib updates}
#' @source \href{https://github.com/Bioconductor/Rhtslib/issues/4}{
#' Rhtslib<1.99.2 (which Rsamtools and seqminer depend on for tabix) 
#' is very out of date (uses htslib 1.7 vs. 1.15)}.
#' 
#' @source \href{https://github.com/Bioconductor/Rsamtools}{
#' Rsamtools: GitHub}
#' @source \href{https://doi.org/doi:10.18129/B9.bioc.Rsamtools}{
#' Rsamtools: Bioconductor}
#' @keywords internal
#' @importFrom utils packageVersion
#' @importFrom BiocManager version
#' @returns Whether the Bioc version is invalid for this function.
rhtslib_warning <- function(rhtslib_pkgs = c("variantannotation",
                                             "rsamtools",
                                             "rtracklayer"),
                            method=NULL,
                            verbose=TRUE){
    rhtslib_ver <- utils::packageVersion("Rhtslib")
    version_invalid <- BiocManager::version()<"3.16"
    method <- tolower(method)[1]
    
    if(isTRUE(version_invalid) && any(method %in% rhtslib_pkgs)){
        msg <- paste( 
             "The selected method",
             if(!is.null(method)) paste0("(",method,")") else NULL,
             "depends on Rhtslib,",
             "and the version you have installed",
             paste0("(",rhtslib_ver,")"),"contains known bugs.",
             "Please install Rhtslib >=1.99.2",
             "via Bioconductor >=3.16:\n",
             "   BiocManager::install(version='devel')"
             )
        warning(msg) 
    }
    return(version_invalid)
}
