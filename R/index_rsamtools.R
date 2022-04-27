#' Tabix-index a file: Rsamtools
#' 
#' Tabix-index a tabular summary statistics file.
#' @inheritParams construct_query
#' @inheritParams index
#' @keywords internal
index_rsamtools <- function(bgz_file,
                            chrom_i,
                            start_i,
                            end_i,
                            comment_char,
                            skip=0L,
                            zeroBased=FALSE,
                            verbose=TRUE,
                            ...){
    
    if(requireNamespace("Rsamtools")){
        rhtslib_warning(verbose = verbose)
    }
    messager("Tabix-indexing file using: Rsamtools",v=verbose)  
    
    # paste("Rscript -e",
    #       shQuote(paste(
    #           "Rsamtools::indexTabix(file = \"",s,"\")"
    #           )
    #       )
    #   ) 
    Rsamtools::indexTabix(file = bgz_file,
                          seq = chrom_i,
                          start = start_i,
                          end = end_i, 
                          comment = comment_char, 
                          skip = skip,
                          zeroBased = zeroBased,
                          ...)
}
