#' Query tabular: conda
#' 
#' Uses a conda-based installation of tabix instead of compiled 
#' \code{C} code from \pkg{Rhtslib}.
#' 
#' @inheritParams construct_query 
#' @inheritParams query_table 
#' 
#' @keywords internal
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom data.table fread
query_table_conda <- function(target_path,
                              query_chrom,
                              query_start_pos,
                              query_end_pos,
                              conda_env="echoR",
                              verbose=TRUE){ 
    messager("echotabix:: Querying tabular tabix file using: conda",
             v=verbose)
    #### Find executable ####
    pkgs <- echoconda::find_packages(packages = "tabix",
                                     conda_env = conda_env,
                                     verbose = verbose)
    tabix <- pkgs$path[[1]][1]
    #### Construct query ####
    coords <- paste0(query_chrom,":",query_start_pos,"-",query_end_pos)  
    #### Run ####
    ## Confusingly, "-h" means " --print-header", not "help".
    ## Worse yet, "-H" means "--only-header", which is not intuitive at all.
    cmd <- paste(tabix,"-h",target_path,coords)
    echoconda::cmd_print(cmd, verbose = verbose)
    #### Read in results directly to R ####
    dat <- data.table::fread(cmd=cmd, 
                             nThread = 1)
    return(dat)
}
