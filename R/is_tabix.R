#' Is tabix
#' 
#' Is a file bgz-zip compressed and tabix-indexed.
#' @param path Path to file.
#' @returns logical: whether the file is tabix-indexed or not.
#' @keywords internal
#' @return logical
is_tabix <- function(path) {
    ## Must meet all of these conditions 
    ## in order to use a pre-existing tabix files. 
    check_func <- function(path){
        all_suffixes <- suffixes(tabular = FALSE, 
                                 vcf = FALSE)
        file.exists(path) &&
            any(endsWith(path, all_suffixes)) &&
            file.exists(paste0(path, ".tbi")) &&
            file.size(path) > 0
    }
    #### Iterate over multiple inputs ####
    res <- unlist(lapply(path, check_func))
    return(res)
}
