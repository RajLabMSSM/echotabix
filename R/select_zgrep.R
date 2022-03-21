select_zgrep <- function(target_path,
                         verbose=TRUE){
    z_grep <- if(endsWith(target_path,".gz")) {
        messager("Searching for header row with zgrep.",v=verbose)
        "zgrep"
    } else { 
        messager("Searching for header row with grep.",v=verbose)
        "grep"
    } 
    return(z_grep)
}