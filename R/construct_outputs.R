#' Construct outputs 
#' 
#' Construct a list of requested outputs.
#' @param outputs
#' \itemize{
#' \item{"command"}{Text string of the command (without executing it.)}
#' \item{"path"}{Path to the saved data file.}
#' \item{"data"}{The resulting data in \link[data.table]{data.table} format.}
#' }  
#' @param command Whether to return the command.
#' @param path Whether to return the path.
#' @param data Whether to return the data.
#' @param verbose Print messages.
#' @keywords internal
construct_outputs <- function(outputs,
                              command=NULL,
                              path=NULL,
                              data=NULL,
                              verbose=TRUE){
    
    messager("Constructing outputs",v=verbose)
    out <- list(command=NULL, path=NULL, data=NULL)
    if("command" %in% outputs){
        out[["command"]] <- command
    }
    if("path" %in% outputs){
        out[["path"]] <- path
    }
    if("data" %in% outputs){
        out[["data"]] <- data
    } 
    ## If all but one are NULL, 
    ## return just that item instead of a list.
    nulls <- sapply(out, is.null)
    nonnull <- names(out)[!nulls]
    if(sum(!nulls)==1){
        return(out[[nonnull]])
    }
    return(out)
}
