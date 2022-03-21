is_tabix <- function(path) {
    ## Must meet all of these conditions 
    ## in order to use a pre-existing tabix files.
    check_func <- function(path){
        file.exists(path) &&
            any(endsWith(path, c(".gz",".bgz"))) &&
            file.exists(paste0(path, ".tbi")) &&
            file.size(path) > 0
    }
    #### Iterate over multiple inputs ####
    res <- sapply(path, check_func)
    return(res)
}
