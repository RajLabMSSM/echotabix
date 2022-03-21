fix_query_style <- function(target_path,
                            query_granges,
                            return_header=FALSE,
                            verbose=TRUE){
    ## This is essential for files stored on remote servers where you can't
    ## edit the file itself. Thus, you have to edit the query.
    messager("Checking query chromosome style is correct.",v=verbose)
    header <- Rsamtools::headerTabix(file = target_path) 
    has_chr <- infer_chrom_type(header$seqnames)
    if(has_chr){
        query_granges <- granges_style(gr = query_granges, 
                                       style = "UCSC")
    }
    if(return_header){
        return(list(query_granges=query_granges,
                    header=header))
    } else {
        return(query_granges)
    } 
}