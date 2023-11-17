#' Query tabular: \pkg{GenomicFiles}
#' 
#' \pkg{GenomicFiles} can query both local and remote files.  
#' @inheritParams construct_query 
#' @inheritParams query_table  
#' 
#' @keywords internal 
query_table_genomicfiles <- function(## Target args
                                     target_path,
                                     target_index = paste0(target_path,".tbi"),
                                     ## Query args 
                                     query_granges, 
                                     yieldSize=NA_character_,
                                     verbose=TRUE){ 
    # devoptera::args2vars(query_table_genomicfiles)
    
    requireNamespace("GenomicFiles")
    messager("Querying tabular tabix file using: GenomicFiles",v=verbose) 
    if(!is.null(query_granges)){
        #### Construct query (if not already in GRanges format) ####
        query_granges <- construct_query(query_dat = query_granges,
                                         verbose = FALSE) 
        #### Ensure chromosome format is correct #### 
        fix_query_style_out <- fix_query_style(target_path=target_path,
                                               target_index=target_index,
                                               query_granges=query_granges,
                                               return_header = TRUE,
                                               verbose=verbose)
        query_granges <- fix_query_style_out$query_granges;
        header <- fix_query_style_out$header;
    } 
    #### Query #### 
    messager("Retrieving data.",v=verbose) 
    tbx <- Rsamtools::TabixFile(file = target_path, 
                                yieldSize = yieldSize)
    MAP <- function(value, header,...){  
        scanTabix_to_dt(header = header, 
                        queries = value, 
                        verbose = TRUE) 
    }
    REDUCE <- function(x,...) data.table::rbindlist(l = x, fill = TRUE)
    
    YIELD <- function(x, param,...) Rsamtools::scanTabix(x)
    DONE <- function(value) length(value) == 0L
    query_dt <- GenomicFiles::reduceByYield(X = tbx,  
                                            YIELD = YIELD,
                                            MAP = MAP,
                                            REDUCE = REDUCE, 
                                            DONE = DONE,
                                            iterate = TRUE,
                                            header = header
                                            ) 
    return(query_dt)
}
