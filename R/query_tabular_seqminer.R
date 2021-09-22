#' query_tabular_seqminer
#'
#' \pkg{seqminer} is faster than \code{Rsamtools} but doesn't work
#' for remote files. Also, \pkg{seqminer} causes errors on Windows
#' and doesn't seem to be actively maintained anymore,
#' so should be avoided where possible.
#'
#' @keywords internal
#' @importFrom seqminer tabix.read.table
query_tabular_seqminer <- function(fullSS_tabix,
                                   chrom,
                                   start_pos,
                                   end_pos,
                                   verbose=TRUE){
    messager("Using seqminer.", v=verbose)
    coords <- paste0(chrom, ":", start_pos, "-", end_pos)
    #### Returns as data.frame ####
    # Fails on Windows until they fix it
    if(get_os()=="Windows"){
        stop("seqminer doesn't not currently work on Windows.")
    }
    dat <- seqminer::tabix.read.table(
        tabixFile = normalizePath(fullSS_tabix),
        tabixRange = coords
    )
    #### Returns as raw text (withOUT header) ####
    # dat <- seqminer::tabix.read(
    #     tabixFile = normalizePath(fullSS_tabix),
    #     tabixRange = coords
    # ) 
    # dat <- data.table::fread(text = dat, nThread = 1)
    
    return(dat)
}