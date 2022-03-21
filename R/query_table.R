#' Query a tabix file
#'
#' Query by genomic coordinates.
#'
#' @param target_path Path to tabix file.
#' @param local Whether \code{target_path} is stored locally or 
#' on a remote server/website.
#' By default (\code{NULL}) will infer local status and 
#' use the appropriate method.  
#' @param verbose Print messages.
#' @inheritParams construct_query
#'
#' @return \code{data.table} with the queried subset of genomic data.
#'
#' @family tabix functions
#' @export 
#' @importFrom data.table fread 
#' @examples 
#' dat <- echodata::BST1
#'
#' #### local ####
#' fullSS_path <- echodata::example_fullSS()
#' tabix_files <- echotabix::convert(fullSS_path = fullSS_path, 
#'                                   start_col = "BP")
#' query_res <- echotabix::query_table(
#'     target_path = tabix_files$data,
#'     query_chrom = dat$CHR[1],
#'     query_start_pos = min(dat$POS),
#'     query_end_pos = max(dat$POS)
#' )
#'
#' #### remote ####
#' target_path <- file.path(
#'     "https://egg2.wustl.edu/roadmap/data/byFileType",
#'     "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
#'     "E099_15_coreMarks_dense.bed.bgz"
#' )
#' tab <- echotabix::query_table(
#'     target_path = target_path,
#'     query_chrom = dat$CHR[1],
#'     query_start_pos = min(dat$POS),
#'     query_end_pos = max(dat$POS)
#' ) 
query_table <- function(target_path,
                        query_chrom,
                        query_start_pos,
                        query_end_pos=query_start_pos,
                        method = c("rsamtools","seqminer","conda"),
                        local = NULL,
                        query_save = TRUE,
                        save_path = tempfile(fileext = "tsv.gz"),
                        conda_env = "echoR",
                        nThread = 1,
                        verbose = TRUE) {
    
    method <- tolower(method)[1]
    if (is.null(local)) local <- echodata::is_local(target_path)
    #### Make sure that method is compatible with remote/local status ####
    if(isFALSE(local) && method=="seqminer"){
        default <- "rsamtools"
        messager("WARNING: 'seqminer' cannot query remote files.",
                 "Switching method to",paste0(shQuote(default),"."),
                 v=verbose)
        method <- default
    }
    #### Select method ####
    if (method=="rsamtools") { 
        #### Remote tabular tabix file ####
        # Rsamtools is slower but works for remote files
        dat <- query_table_rsamtools(target_path = target_path,
                                     query_chrom = query_chrom, 
                                     query_start_pos = query_start_pos, 
                                     query_end_pos = query_end_pos,
                                     verbose = verbose)
    } else if(method=="seqminer") { 
        #### Local tabular tabix file ####
        dat <- query_table_seqminer(target_path = target_path,
                                      query_chrom = query_chrom, 
                                      query_start_pos = query_start_pos, 
                                      query_end_pos = query_end_pos,
                                      verbose = verbose)
    } else {
        dat <- query_table_conda(target_path = target_path,
                                   query_chrom = query_chrom,
                                   query_start_pos = query_start_pos,
                                   query_end_pos = query_end_pos,
                                   conda_env=conda_env,
                                   verbose=verbose) 
    }
    #### Report ####
    report_tabular(dat = dat, 
                   verbose = verbose)
    #### Save ####
    if(query_save){
        save_tabular(dat=dat,
                     save_path=save_path, 
                     nThread=nThread,
                     verbose=verbose)
    } 
    return(dat)
}
