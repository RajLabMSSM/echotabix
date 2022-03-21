#' Query a tabix file
#'
#' Query by genomic coordinates.
#'
#' @param target_path Path to tabix file.
#' @param local Whether \code{target_path} is stored locally or 
#' on a remote server/website.
#' By default (\code{NULL}) will infer local status and 
#' use the appropriate \code{query_method}.  
#' @param verbose Print messages.
#' @inheritParams construct_query
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
#'     query_dat = query_dat) 
query_table <- function(## Target args
                        target_path,
                        ## Query args 
                        query_dat,
                        query_granges = construct_query(  
                            query_dat=query_dat,
                            query_chrom_col="CHR",
                            query_start_col="POS",
                            query_snp_col="SNP"), 
                        ## Extra args
                        query_method = c("rsamtools","seqminer","conda"),
                        local = NULL,
                        # overlapping_only = FALSE,
                        query_save = TRUE,
                        save_path = tempfile(fileext = "tsv.gz"),
                        conda_env = "echoR",
                        nThread = 1,
                        verbose = TRUE) {
    
    query_method <- tolower(query_method)[1]
    if (is.null(local)) local <- echodata::is_local(target_path)
    #### Make sure that query_method is compatible with remote/local status ####
    if(isFALSE(local) && query_method=="seqminer"){
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
                                           query_dat = query_dat, 
                                           query_granges = query_granges,
                                           verbose = verbose)
    } else if(query_method=="seqminer") { 
        #### Local tabular tabix file ####
        query_res <- query_table_seqminer(target_path = target_path,
                                          query_dat = query_dat, 
                                          query_granges = query_granges,
                                          verbose = verbose)
    } else {
        query_res <- query_table_conda(target_path = target_path,
                                       query_dat = query_dat, 
                                       query_granges = query_granges,
                                       conda_env=conda_env,
                                       verbose=verbose) 
    }
    #### Remove non-overlapping variants ####
    # if(overlapping_only){ 
    #     query_res <- echodata::merge_robust(x = query_res, 
    #                                         y = query_dat, 
    #                                         by = )
    # }  
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
