#' Run bgzip: conda
#' 
#' Support function for \link[echotabix]{run_bgzip}.
#' @inheritParams run_bgzip
#' @inheritParams construct_query
#' 
#' @keywords internal 
#' @importFrom R.utils isGzipped gunzip
#' @importFrom echoconda yaml_to_env
#' @importFrom echodata set_permissions
run_bgzip_conda <- function(target_path,
                            bgz_file = construct_tabix_path(
                                target_path = target_path
                                ),
                            chrom_col,
                            start_col,
                            end_col,
                            comment_char=NULL,
                            conda_env="echoR_mini",
                            verbose=TRUE){
    
    messager("bgzipping file with conda.", v = verbose)  
    ### Set up conda echoR ####
    conda_env <- echoconda::yaml_to_env(yaml_path = conda_env,
                                        verbose = verbose)
    #### Infer comment_char arg from header ####
    comment_char <- infer_comment_char(target_path = target_path, 
                                       comment_char = comment_char,
                                       verbose = verbose)
    #### Must gz unzip before zipping again with bgzip ####
    if(R.utils::isGzipped(target_path)){
        target_path <- R.utils::gunzip(target_path,
                                       overwrite=TRUE, remove=FALSE) 
    } 
    #### Run full command ####
    bgzip_ex <- get_bgzip(conda_env = conda_env)
    echodata::set_permissions(path = target_path,
                              verbose = verbose)
    cmd <- paste(## Pipe in file contents
                 "cat",target_path,
                 ## Compress with bgzip
                 "|",bgzip_ex,
                 ## Force new bgzip file
                 "-f",
                 ## Write to file 
                 ">",bgz_file)
    echoconda::cmd_print(cmd,verbose = verbose, raw = TRUE)
    system(cmd)
    return(bgz_file)
}
