#' Run gunzip: conda
#' 
#' Support function for \link[echotabix]{run_gunzip}.
#' 
#' @param path Path to file. 
#' @inheritParams sort_coordinates
#' @inheritParams echoconda::find_packages
#' @importFrom echoconda find_packages yaml_to_env
#' @keywords internal
run_gunzip_conda <- function(path,
                             gunzip_ex=NULL,
                             outputs=c("command","path","data"),
                             save_path=gsub(".gz|.bgz","",path),
                             conda_env="echoR_mini",
                             verbose=TRUE){ 
    
    ### Set up conda echoR ####
    conda_env <- echoconda::yaml_to_env(yaml_path = conda_env,
                                        verbose = verbose)
    #### Find conda executable ####
    gunzip_ex <- get_gunzip(gunzip_ex=gunzip_ex,
                            conda_env=conda_env,
                            verbose=verbose) 
    #### Create save dir ####
    if(!is.null(save_path)) {
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    }
    #### Run command #####
    cmd <- paste(gunzip_ex, 
                 "-c",path,
                 if(!is.null(save_path)) paste(">",save_path) else save_path
                 ) 
    #### Return ####
    dat <- NULL
    if(any(c("path","data") %in% outputs)){
        echoconda::cmd_print(cmd, verbose = verbose)
        system(cmd)
        if("data" %in% outputs){
            dat <- data.table::fread(text = save_path, nThread = 1) 
        } 
    }
    out <- construct_outputs(outputs = outputs, 
                             command = paste("(",cmd,")"), 
                             path = save_path,
                             data = dat) 
    return(out) 
}
