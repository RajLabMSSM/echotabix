#' Query tabular: \pkg{seqminer}
#'
#' \strong{Limitations of seqminer}:
#' \itemize{
#' \item{Doesn't work with remote files.}
#' \item{Assumes header column always starts with "#" (which is often incorrect)
#' without the option for the user to specify otherwise. 
#' The only way that summary statistics can be used with \pkg{seqminer}
#'  is if they adhere to this convention, or were indexed using \pkg{seqminer}'s
#'  \link[seqminer]{tabix.createIndex} function.}
#' \item{Maintainers are unresponsive to requests for bug fixes.}
#' }  
#' \strong{Advantages of seqminer}:
#' \itemize{
#' \item{\link[seqminer]{tabix.read.table} automatically converts query results
#' to data.frame format (though this can now also be done by 
#' \link[echotabix]{scanTabix_to_dt}).}
#' } 
#' \pkg{seqminer} appears to be maintained to some degree
#'  (based on the latest commits), but the maintainers
#' have been unresponsive to bug reports for years. 
#' This limits the consistent usability of \pkg{seqminer}.
#' 
#' @inheritParams construct_query 
#' @inheritParams query_table 
#' 
#' @source \href{https://github.com/zhanxw/seqminer/issues/25}{
#' GitHub Issues: coordinate order error}
#' @source \href{https://github.com/zhanxw/seqminer/issues/20}{
#' GitHub Issues: remote file error}
#' @source \href{https://www.utsouthwestern.edu/labs/zhan/contact/}{
#' Lab contact details for \code{seqminer} maintainer}
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/26394715/}{
#' \code{seqminer} publication}
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/32756942/}{
#' \code{seqminer2} publication (same package and GitHub repo, just updated)} 
#' @keywords internal 
query_table_seqminer <- function(## Target args
                                 target_path,
                                 target_index,
                                 ## Query args
                                 query_granges, 
                                 ## Extra args
                                 verbose=TRUE){
    requireNamespace("seqminer")
    messager("Querying tabular tabix file using: seqminer",v=verbose)
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat = query_granges,
                                     verbose = FALSE)
    #### Determine query_chrom format of tabix file ####
    ## Use my own header function, since seqminer doesn't return 
    ## other essential information like seqnames. 
    
    #### Ensure chromosome format is correct #### 
    query_granges <- fix_query_style(target_path=target_path,
                                     target_index=target_index,
                                     query_granges=query_granges,
                                     return_header = FALSE,
                                     verbose=verbose)
    #### Construct query ####
    coords <- granges_to_string(gr = query_granges) 
    #### Returns as data.frame #### 
    messager("Retrieving data.",v=verbose) 
    dat <- seqminer::tabix.read.table(
        tabixFile = target_path,
        tabixRange = coords, 
        col.names = TRUE
    )
    dat <- data.table::data.table(dat)
    #### Returns as raw text (withOUT header) ####
    # dat <- seqminer::tabix.read(
    #     tabixFile = target_path,
    #     tabixRange = coords
    # )
    # dat <- data.table::fread(text = dat, nThread = 1) 
    return(dat)
}
