get_gunzip <- function(gunzip_ex=NULL,
                       conda_env="echoR_mini",
                       verbose=TRUE){
    if(!is.null(gunzip_ex) && file.exists(gunzip_ex)){
        messager("Using:",gunzip_ex,v=verbose)  
    } else {
        out <- system("which gunzip",intern = TRUE)
        if(length(out)>0){
            gunzip_ex <- as.character(out)
        } else{
            #### Find gunzip executable with conda instead #####
            gzip <- echoconda::find_packages(packages = "gzip", 
                                             conda_env = conda_env,
                                             return_path = TRUE,
                                             verbose = verbose)
            ## These tools are installed together so we can simply substitute.
            gunzip_ex <- gsub("gzip","gunzip",gzip[[1]])
        }
    }
    #### Report which executable will be used, if any ####
    if(length(gunzip_ex)==0) {
        stop("gunzip executable could be identified.")
    } else {
        messager("Using:",gunzip_ex,v=verbose) 
    } 
    return(gunzip_ex)
}