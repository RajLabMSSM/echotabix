#' Tabix-index a file: seqminer
#' 
#' Tabix-index a tabular summary statistics file.
#' @inheritParams index
index_seqminer <- function(bgz_file,
                           chrom_i,
                           start_i,
                           end_i,
                           comment_char,
                           skipLines=0,
                           verbose=TRUE){
    messager("echotabix:: Tabix-indexing file using seqminer.",v=verbose) 
    requireNamespace("seqminer")
    seqminer::tabix.createIndex(
        bgzipFile = bgz_file,
        sequenceColumn = chrom_i,
        startColumn = start_i,
        endColumn = end_i,  
        metaChar = comment_char,
        skipLines = skipLines
    )  
}
