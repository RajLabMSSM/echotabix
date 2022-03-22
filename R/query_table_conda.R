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
#' @importFrom echoconda yaml_to_env
query_table_conda <- function(## Target args
                              target_path,
                              ## Query args
                              query_granges, 
                              ## Extra args
                              conda_env="echoR",
                              verbose=TRUE){ 
    messager("Querying tabular tabix file using: conda",
             v=verbose)
    ### Set up conda echoR ####
    conda_env <- echoconda::yaml_to_env(conda_env)
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat=query_granges,
                                     verbose = FALSE)
    #### Find executable ####
    pkgs <- echoconda::find_packages(packages = "tabix",
                                     conda_env = conda_env,
                                     verbose = verbose)
    tabix <- pkgs$path[[1]][1]
    
    #### Ensure chromosome format is correct #### 
    query_granges <- fix_query_style(target_path=target_path,
                                     query_granges=query_granges,
                                     return_header = FALSE,
                                     verbose=verbose)
    #### Construct query ####
    coords <- granges_to_string(gr = query_granges) 
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
