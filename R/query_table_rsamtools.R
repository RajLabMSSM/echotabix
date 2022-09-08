#' Query tabular: \pkg{Rsamtools}
#' 
#' \pkg{Rsamtools} can query both local and remote files. 
#'  However, it seems unable to read the header of 
#'  tabix files created with \pkg{seqminer}.
#' @inheritParams construct_query 
#' @inheritParams query_table 
#'  
#' @source \href{https://github.com/Bioconductor/Rsamtools/issues/8}{
#'  Bugs in \code{Rsamtools::scanTabix}}
#' @keywords internal
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom data.table fread
query_table_rsamtools <- function(## Target args
                                  target_path,
                                  target_index = paste0(target_path,".tbi"),
                                  ## Query args 
                                  query_granges, 
                                  verbose=TRUE){ 
    requireNamespace("Rsamtools")
    messager("Querying tabular tabix file using: Rsamtools.",v=verbose) 
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat=query_granges,
                                     verbose = FALSE)
    #### Import file as TabixFile object ####
    ## This actually causes way more problems than it solves.
    ## Instead, just supply the file path directly to scanTabix.
    ## Specifying the .tbi index file doesn't seem to make a difference.
    # tab <- Rsamtools::TabixFile(target_path)   
    
    #### Ensure chromosome format is correct #### 
    fix_query_style_out <- fix_query_style(target_path=target_path,
                                           target_index=target_index,
                                           query_granges=query_granges,
                                           return_header = TRUE,
                                           verbose=verbose)
    query_granges <- fix_query_style_out$query_granges;
    header <- fix_query_style_out$header;
    #### Query ####
    messager("Retrieving data.",v=verbose)
    tbx <- Rsamtools::TabixFile(file = target_path, 
                                index = target_index)
    queries <- Rsamtools::scanTabix(file = tbx,
                                    param = query_granges) 
    #### Convert to data.table #### 
    query_dt <- scanTabix_to_dt(header = header, 
                                queries = queries, 
                                verbose = verbose)  
    return(query_dt)
}
