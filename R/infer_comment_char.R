infer_comment_char <- function(target_path,
                               comment_char,
                               verbose=TRUE){
    #### Infer comment_char arg from header ####
    if(is.null(comment_char)) { 
        comment_char <- echodata::get_header(path = target_path)[1]
        messager("Inferring comment_char from header:",
                 shQuote(comment_char),
                 v=verbose)
    }
    return(comment_char)
}