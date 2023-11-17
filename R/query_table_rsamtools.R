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
                                  yieldSize=NA_character_,
                                  verbose=TRUE){ 
    # devoptera::args2vars(query_table_rsamtools)
    
    requireNamespace("Rsamtools")
    messager("Querying tabular tabix file using: Rsamtools.",v=verbose)  
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat=query_granges,
                                     verbose = FALSE) 
    #### Ensure chromosome format is correct #### 
    fix_query_style_out <- fix_query_style(target_path=target_path,
                                           target_index=target_index,
                                           query_granges=query_granges,
                                           return_header = TRUE,
                                           verbose=verbose)
    query_granges <- fix_query_style_out$query_granges;
    header <- fix_query_style_out$header; 
    #### Query ####
    messager("Creating TabixFile connection.",v=verbose)
    tbx <- Rsamtools::TabixFile(file = target_path, 
                                index = target_index,
                                yieldSize = yieldSize)
    messager("Retrieving data.",v=verbose)
    queries <- Rsamtools::scanTabix(file = tbx,
                                    param = query_granges) 
    #### Convert to data.table #### 
    query_dt <- scanTabix_to_dt(header = header, 
                                queries = queries, 
                                verbose = verbose)  
    return(query_dt)
}
