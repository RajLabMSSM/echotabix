is_tab_delimited <- function(path){
    delim <- check_delimiter(path=path)
    is_tab <- delim %in% c("\t","\\t")
    return(is_tab)
}