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
rsamtools_warning <- function(old_version="1.7",
                              latest_version="1.15",
                              verbose=TRUE){
    messager("WARNING:",
             "\nRsamtools uses an old version of htslib",
             paste0("(v",old_version,"),"),
             "which may cause some unexpected behaviours.",
             "\nThe latest version of htslib is",
             paste0(">=v",latest_version,"."),
             v=verbose)
}