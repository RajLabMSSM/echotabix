get_bgzip <- function(conda_env="echoR"){
    #### Find bgzip binary ####
    ## Not working
    # pkgs <- echoconda::find_packages(packages = "bgzip", 
    #                                  conda_env = conda_env) 
    python <- echoconda::find_python_path(conda_env = conda_env)
    bgzip_ex <- file.path(dirname(python),"bgzip")
    if(!file.exists(bgzip_ex)) bgzip_ex <- "bgzip"
    if(length(system("which bgzip", intern = TRUE))==0) {
        stop("bgzip executable could be identified.")
    }
    return(bgzip_ex)
}