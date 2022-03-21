fix_query_style <- function(target_path,
                            gr,
                            return_header=FALSE,
                            verbose=TRUE){
    ## This is essential for files stored on remote servers where you can't
    ## edit the file itself. Thus, you have to edit the query.
    messager("Checking query chromosome style is correct.",v=verbose)
    header <- Rsamtools::headerTabix(file = target_path) 
    has_chr <- infer_chrom_type(header$seqnames)
    if(has_chr){
        gr <- granges_style(gr = gr, 
                            style = "UCSC")
    }
    if(return_header){
        return(list(gr=gr,
                    header=header))
    } else {
        return(gr)
    } 
}