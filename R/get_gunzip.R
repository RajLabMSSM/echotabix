get_gunzip <- function(gunzip_ex=NULL,
                       conda_env="echoR",
                       verbose=TRUE){
    if(!is.null(gunzip_ex) && file.exists(gunzip_ex)){
        messager("Using:",gunzip_ex,v=verbose)  
    } else {
        out <- system("which gunzip",intern = TRUE)
        if(length(out)>0){
            gunzip_ex <- as.character(out)
        } else{
            #### Find gunzip executable with conda instead #####
            pkgs <- echoconda::find_packages(packages = "gzip", 
                                             conda_env = conda_env,
                                             verbose = verbose)
            ## These tools are installed together so we can simply substitute.
            gunzip_ex <- gsub("gzip","gunzip",pkgs$path[[1]][1])
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