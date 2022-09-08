filter_table_snps <- function(query_res,
                              query_granges,
                              verbose=TRUE){
    SNP <- NULL;
    ## Issue; requires that SNP is in the query_res cols
    if("SNP" %in% names(query_res)){
        if(!is.null(query_granges$SNP)){
            query_res <- query_res[
                SNP %in% unlist(strsplit(query_granges$SNP,";")),]    
        } else {
            msg <- paste(
                "Warning: Unable to run overlapping_only",
                "since there is no 'SNP' column in query_granges.")
            messager(msg, v=verbose)
        } 
    } else {
        msg <- paste(
            "Warning: Unable to run overlapping_only",
            "since there is no 'SNP' column in the query results.")
        messager(msg, v=verbose)
    }
    return(query_res)
}
