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
