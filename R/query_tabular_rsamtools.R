#' query_tabular_rsamtools
#' 
#' \pkg{Rsamtools} seems to be slower than \pkg{seqminer},
#'  but can query remote files. 
#'  However, it seems unable to read the header of 
#'  tabix files created with \pkg{seqminer}.
#'  
#' @keywords internal
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom data.table fread
query_tabular_rsamtools <- function(fullSS_tabix,
                                    chrom,
                                    start_pos,
                                    end_pos,
                                    verbose=TRUE){
    if(requireNamespace("Rsamtools")){
        rsamtools_warning(verbose = verbose)
    }
    messager("Querying tabular tabix file using: Rsamtools.",v=verbose)
    tab <- Rsamtools::TabixFile(fullSS_tabix) 
    gr2 <- GenomicRanges::GRanges(
        seqnames = chrom,
        ranges = IRanges::IRanges(
            start = start_pos,
            end = end_pos
        )
    )
    # tab_head <- Rsamtools::headerTabix(tab) # Really slow
    tab_dat <- Rsamtools::scanTabix(tab, gr2)
    dat <- data.table::fread(text = tab_dat[[1]], nThread = 1)
    return(dat)
}