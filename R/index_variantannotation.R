#' Tabix-index file: VariantAnnotation
#' 
#' Convert summary stats file to tabix format
#'
#' @source Borrowed function from 
#' \href{https://github.com/RajLabMSSM/echotabix/blob/main/R/convert.R}{
#' echotabix}.
#' 
#' @param path Path to VCF.
#' @param verbose Print messages.
#' 
#' @return Path to tabix-indexed tabular file
#'
#' @family tabix
#' @keywords internal
index_variantannotation <- function(path,
                                    verbose=TRUE){ 
    #### When remote ####
    if(!file.exists(path) && 
       any(startsWith(tolower(path),c("http","ftp")))){  
        messager("File inferred to be remote.",v=verbose)
        return(path)
        #### When already tabix ####
    } else if(is_tabix(path = path)){
        messager("File already tabix-indexed.",v=verbose)
        return(path)
        #### When local and non-tabix ####
    } else {
        messager("Compressing and tabix-indexing VCF file.",v=verbose)
        if(!endsWith(path,".bgz|.gz")){
            path <- Rsamtools::bgzip(file = path, 
                                     dest = sprintf("%s.bgz",
                                                    sub("\\.gz$|\\.bgz$", "",
                                                        path)),
                                     overwrite = TRUE)
        } 
        path <- VariantAnnotation::indexVcf(x = path)$path 
        return(path)
    } 
}
