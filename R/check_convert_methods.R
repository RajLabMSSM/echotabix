check_convert_methods <- function(convert_methods=list(),
                                  verbose=TRUE){
    #### All options ####
    opts <- list(
        sort_coordinates=eval(formals(sort_coordinates)$method), 
        run_bgzip=eval(formals(run_bgzip)$method),
        index=eval(formals(index)$method)
        )
    #### None supplied ####
    if(length(convert_methods)==0){
        messager("Returning all options for all convert steps.",v=verbose)
        return(opts)
    }
    #### Check valid vs non-valid user-supplied methods #### 
    non_names <- names(convert_methods)[
        !names(convert_methods) %in% names(opts)
        ] 
    if(length(non_names)>0) {
        messager(
            paste0("WARNING: The following step names were not recognized",
                   "and will be dropped:\n",
                   paste(" -",non_names, collapse = "\n")),v=verbose)
    }
    #### Check for missing names ####
    missing_names <- names(opts)[!names(opts) %in% names(convert_methods)]
    if(length(missing_names)>0){
        messager(paste0(
            "Assigning default method for the following steps:\n",
           paste(" -", missing_names, collapse = "\n")
        ),v=verbose)
        for(nm in missing_names){
            ### Assign the default
            convert_methods[nm] <- tolower(opts[[nm]])[1]
        }
    }
    #### Check that each option is valid ####
    valid_names <- names(convert_methods)[
        names(convert_methods) %in% names(opts)
        ]
    method_new <- lapply(valid_names, function(nm){ 
        selected <- tolower(convert_methods[[nm]][1])
        method_opts <- tolower(opts[[nm]])
        if(!selected %in% method_opts){
            messager("WARNING: ",paste0(nm,"=",selected),
                     "was not recognized as a valid method",
                     "and will be replaced with the default:",
                     method_opts[1])
            return(method_opts[1])
        } else {
            return(selected)
        }
    }) 
    names(method_new) <- valid_names
    return(method_new)
}
