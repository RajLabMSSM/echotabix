check_outputs <- function(outputs,
                          func){
    outputs <- tolower(outputs) 
    opts <- eval(formals(func)$outputs)
    #### Find and report invalid options ####
    non_opts <- outputs[!outputs %in% opts]
    if(length(non_opts)>0){
        messager(paste0(
            "Warning: outputs must be one or more of:\n",
            paste(" -",opts,collapse = "\n")
        ))
    }
    #### Check that there's at least 1 valid option ####
    valid_opts <- outputs[outputs %in% opts]
    if(length(valid_opts)==0){
        stop(paste0(
            "outputs must be one or more of:\n",
            paste(" -",opts,collapse = "\n")
        ))
    }
    return(valid_opts)
}
