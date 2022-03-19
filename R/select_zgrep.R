select_zgrep <- function(fullSS_path,
                         verbose=TRUE){
    z_grep <- if(endsWith(fullSS_path,".gz")) {
        messager("Searching for header row with zgrep.",v=verbose)
        "zgrep"
    } else { 
        messager("Searching for header row with grep.",v=verbose)
        "grep"
    } 
    return(z_grep)
}