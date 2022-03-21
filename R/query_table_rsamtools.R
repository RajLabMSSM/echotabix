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
query_table_rsamtools <- function(target_path,
                                  query_chrom,
                                  query_start_pos,
                                  query_end_pos=query_start_pos,
                                  verbose=TRUE){
    if(requireNamespace("Rsamtools")){
        rhtslib_warning(verbose = verbose)
    }
    messager("Querying tabular tabix file using: Rsamtools.",v=verbose)
    #### Construct queries as granges ####
    gr <- construct_query(query_chrom=query_chrom, 
                          query_start_pos=query_start_pos,
                          query_end_pos=query_end_pos, 
                          verbose=verbose)
    #### Import file as TabixFile object ####
    ## This actually causes way more problems than it solves.
    ## Instead, just supply the file path directly to scanTabix.
    ## Specifying the .tbi index file doesn't seem to make a difference.
    # tab <- Rsamtools::TabixFile(target_path)   
    
    #### Ensure chromosome format is correct #### 
    fix_query_style_out <- fix_query_style(target_path=target_path,
                                           gr=gr,
                                           return_header = TRUE,
                                           verbose=verbose)
    gr <- fix_query_style_out$gr;
    header <- fix_query_style_out$header;
    #### Query ####
    messager("Retrieving data.",v=verbose)
    queries <- Rsamtools::scanTabix(file = target_path,
                                    param = gr) 
    #### Convert to data.table #### 
    query_dt <- scanTabix_to_dt(header = header, 
                                queries = queries, 
                                verbose = verbose)  
    return(query_dt)
}
