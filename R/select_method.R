#' Select method
#' 
#' Select a valid alternative method when the one chosen is not compatible with
#' the current Bioconductor version.
#' When provided a valid method, simply reutrns that method.
#' @param fn Unevaluated function to select method for.
#' @param fn_arg Name of the argument that lists the various methods available
#'  for that particular function.
#' @param method Method selected by user.
#' @param verbose Print messages.
#' @inheritParams rhtslib_warning
#' 
#' @keywords internal
#' @returns A valid method.
select_method <- function(fn,
                          fn_arg="method",
                          method,
                          rhtslib_pkgs = c("variantannotation",
                                           "rsamtools",
                                           "rtracklayer"),
                          verbose=TRUE){
    
    #### Select first valid method ####
    if(length(method)>1){
        args <- eval(formals(fn)[[fn_arg]]) 
        method <- method[method %in% args]
        if(length(method)>0){
            method <- method[1]
        } else {
            stp <- paste("method must be one of:",
                         paste0("\n - ",args, collapse = ""))
            stop(stp)
        }
    } 
    version_invalid <- rhtslib_warning(method = method,
                                       rhtslib_pkgs = rhtslib_pkgs,
                                       verbose = verbose) 
    if(version_invalid){   
        args <- eval(formals(fn)[[fn_arg]]) 
        method <- args[!args %in% rhtslib_pkgs][1]
        if(length(method)>0){
            messager("Selecting valid alternative method:",method,v=verbose)    
        } else {
            stop("Not valid methods available with current Bioc version.")
        } 
    } 
    return(method)
}
