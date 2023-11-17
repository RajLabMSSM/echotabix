#' scanTabix to data.table
#' 
#' Convert the output of \link[Rsamtools]{scanTabix}
#'  into a \link[data.table]{data.table}. 
#'  Can handle tabix files that are missing column names, 
#'  and query results that only have a single row.
#'  
#' @param header Header, from the output of \link[Rsamtools]{headerTabix}.
#' @param queries A named list of query results, 
#' from the output of \link[Rsamtools]{scanTabix}.
#' @param add_query_names Add the names of each query to a new column 
#' named 'query'.
#' @param remove_duplicates Remove any duplicated rows.
#'  Set \code{add_query_names=FALSE} to prevent each unique range (e.g. SNP) 
#'  from appearing in more than one row.
#' @param verbose Print messages.
#' @inheritParams data.table::fread
#' 
#' @export
#' @examples 
#' fl <- system.file("extdata", "example.gtf.gz", package="Rsamtools",
#'                   mustWork=TRUE)
#' tbx <- Rsamtools::TabixFile(fl)
#' 
#' param <- GenomicRanges::GRanges(
#'     c("chr1", "chr2"),
#'     IRanges::IRanges(c(1, 1), width=100000))
#' queries <- Rsamtools::scanTabix(tbx, param=param)
#' header <- Rsamtools::headerTabix(fl)
#' 
#' #### Convert ####
#' query_dt <-  echotabix::scanTabix_to_dt(header = header,
#'                                         queries = queries) 
scanTabix_to_dt <- function(header,
                            queries, 
                            add_query_names=TRUE,
                            remove_duplicates=TRUE,
                            sep="\t",
                            verbose=TRUE){
    messager("Converting query results to data.table.", v=verbose)  
    #### Reconstruct data.table ####
    query_dt_list <- lapply(names(queries), function(nm){
        messager("Processing query:",nm,v=verbose) 
        query <- queries[[nm]]
        qdt <- tryCatch({
            #### Add extra rows ####
            ## Input has to contain >=2 rows for 
            ## data.table to be able to parse it.
            single_row <- length(query)==1
            if(single_row){
                query <- rep(query, 2)
            }
            #### Parse ####
            qdt <- data.table::fread(text = paste(query,
                                                  collapse = "\n"), 
                                     header = FALSE,
                                     sep = sep,
                                     fill = TRUE)
            #### Add missing header back in ####
            ## But only if there's a header in the tabix file to begin with.  
            if(length(header$header)>0){ 
                colnames(qdt) <- strsplit(header$header, split = sep)[[1]] 
            }  
            #### Remove any artificially added rows ####
            if(single_row){
                qdt <- qdt[1,]
            }
            qdt
        }, error = function(e) {print(e); NULL})
        return(qdt)
    })|> `names<-`(names(queries)) 
    
    ##### Add new column with the name of each query #####
    if(add_query_names){
        messager("Adding 'query' column to results.",v=verbose) 
    }
    query_dt <- data.table::rbindlist(l = query_dt_list, 
                          # use.names = add_query_names,
                          idcol = if(add_query_names) "query" else NULL, 
                          fill = TRUE) 
    if(remove_duplicates) query_dt <- unique(query_dt)
    return(query_dt)
}
