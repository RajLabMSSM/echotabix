#' Run bgzip: conda
#' 
#' Support function for \link[echotabix]{run_bgzip}.
#' @inheritParams run_bgzip
#' @inheritParams construct_query
#' 
#' @keywords internal 
#' @importFrom R.utils isGzipped gunzip
run_bgzip_conda <- function(fullSS_path,
                            bgz_file = tabix_path(path = fullSS_path),
                            chrom_col,
                            start_col,
                            end_col,
                            comment_char=NULL,
                            conda_env="echoR",
                            verbose=TRUE){
    
    messager("echotabix:: bgzipping file with conda.", v = verbose)    
    #### Infer comment_char arg from header ####
    comment_char <- infer_comment_char(fullSS_path = fullSS_path, 
                                       comment_char = comment_char,
                                       verbose = verbose)
    #### Must gz unzip before zipping again with bgzip ####
    if(R.utils::isGzipped(fullSS_path)){
        fullSS_path <- R.utils::gunzip(fullSS_path,
                                       overwrite=TRUE, remove=FALSE) 
    } 
    #### Run full command ####
    bgzip_ex <- get_bgzip(conda_env = conda_env)
    cmd <- paste(fullSS_path,
                 ## Compress with bgzip
                 "|",bgzip_ex,"-f",
                 ## Write to file 
                 ">",bgz_file)
    echoconda::cmd_print(cmd,verbose = verbose)
    system(cmd)
    return(bgz_file)
}
