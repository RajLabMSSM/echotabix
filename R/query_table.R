#' Query a tabix file
#'
#' Query by genomic coordinates.
#'
#' @param target_path Path to tabix file.
#' @param save_path File path to save query subset to (as table). 
#' @param local Whether \code{target_path} is stored locally or 
#' on a remote server/website.
#' By default (\code{NULL}) will infer local status and 
#' use the appropriate \code{query_method}.  
#' @param verbose Print messages.
#' @inheritParams query
#' @inheritParams construct_query
#' @inheritParams convert_and_query
#' @inheritParams echoconda::find_packages
#'
#' @return \code{data.table} with the queried subset of genomic data.
#'
#' @family tabix functions
#' @export 
#' @importFrom data.table fread 
#' @examples 
#' query_dat <- echodata::BST1
#'
#' #### local ####
#' target_path <- echodata::example_fullSS()
#' tabix_files <- echotabix::convert(target_path = target_path, 
#'                                   start_col = "BP")
#' query_res <- echotabix::query_table(
#'     target_path = tabix_files$path,
#'     query_dat = query_dat)
#'
#' #### remote ####
#' target_path <- file.path(
#'     "https://egg2.wustl.edu/roadmap/data/byFileType",
#'     "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
#'     "E099_15_coreMarks_dense.bed.bgz"
#' )
#' query_res2 <- echotabix::query_table(
#'     target_path = target_path,
#'     query_granges = query_dat) 
query_table <- function(## Target args
                        target_path,
                        target_index = paste0(target_path,".tbi"),
                        ## Query args 
                        query_granges, 
                        ## Extra args
                        query_method = c("rsamtools",
                                         "seqminer",
                                         "conda"),
                        local = NULL,
                        overlapping_only = FALSE,
                        query_save = TRUE,
                        save_path = tempfile(fileext = "tsv.gz"),
                        conda_env = "echoR_mini",
                        nThread = 1,
                        verbose = TRUE) {
    
    query_method <- select_method(fn = query_table,
                                  fn_arg = "query_method",
                                  method = query_method, 
                                  verbose = verbose) 
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat = query_granges,
                                     verbose = FALSE)
    query_method <- tolower(query_method)[1]
    if (is.null(local)) local <- echodata::is_local(target_path)
    #### Make sure that query_method is compatible with remote/local status ####
    if(isFALSE(local) && 
       query_method=="seqminer"){
        default <- "rsamtools"
        messager("WARNING: 'seqminer' cannot query remote files.",
                 "Switching query_method to",paste0(shQuote(default),"."),
                 v=verbose)
        query_method <- default
    }
    #### Select query_method ####
    if (query_method=="rsamtools") { 
        #### Remote tabular tabix file ####
        # Rsamtools is slower but works for remote files
        query_res <- query_table_rsamtools(target_path = target_path,
                                           target_index = target_index,
                                           query_granges = query_granges,
                                           verbose = verbose)
    } else if(query_method=="seqminer") { 
        #### Local tabular tabix file ####
        query_res <- query_table_seqminer(target_path = target_path,
                                          target_index = target_index,
                                          query_granges = query_granges,
                                          verbose = verbose)
    } else {
        query_res <- query_table_conda(target_path = target_path,
                                       target_index = target_index,
                                       query_granges = query_granges,
                                       conda_env = conda_env,
                                       verbose = verbose) 
    }
    #### Remove non-overlapping variants ####
    if(isTRUE(overlapping_only)){
        query_res <- filter_table_snps(query_res = query_res,
                                       query_granges = query_granges,
                                       verbose = verbose)
    }
    #### Remove duplicate rows ####
    query_res <- unique(query_res)
    #### Report ####
    report_tabular(query_res = query_res, 
                   verbose = verbose)
    #### Save ####
    if(query_save){
        save_tabular(query_res=query_res,
                     save_path=save_path, 
                     nThread=nThread,
                     verbose=verbose)
    } 
    return(query_res)
}
