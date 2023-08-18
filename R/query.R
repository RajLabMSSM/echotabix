#' Query tabix
#' 
#' Query a tabix table or VCF.
#' 
#' @param cleanup_tbi Remove local copies of tabix index file (\emph{.tbi}) 
#' after completing queries.
#' @inheritParams construct_query 
#' @inheritParams convert_and_query
#' @inheritParams query_vcf
#' @inheritParams query_table
#' @export
#' @examples 
#' query_dat <- echodata::BST1
#'
#' #### local ####
#' target_path <- echodata::example_fullSS()
#' tabix_files <- echotabix::convert(target_path = target_path, 
#'                                   start_col = "BP")
#' query_res <- echotabix::query(
#'     target_path = tabix_files$path,
#'     query_granges = query_dat)
query <- function(## Target args
                  target_path,
                  target_index = paste0(target_path,".tbi"),
                  target_format = NULL, 
                  
                  ## Query args 
                  query_granges,
                  samples = character(),
                  #### Extra Parameters 
                  query_save = TRUE,
                  query_save_path=tempfile(fileext = ".gz"),
                  
                  ## Genome builds 
                  target_genome = "GRCh37", 
                  query_genome = "GRCh37",
                  
                  ## Method args 
                  query_method=c("rsamtools",
                                 "variantannotation",
                                 "conda",
                                 "seqminer"),
                  conda_env = "echoR_mini",
                  
                  ### Force new  
                  query_force_new = FALSE,
                  
                  ## Extra args
                  as_datatable = TRUE,
                  overlapping_only = FALSE,
                  cleanup_tbi = TRUE,
                  nThread = 1,
                  verbose = TRUE){
    
    messager("========= echotabix::query =========", v=verbose)
    # query_method <- select_method(fn = query,
    #                               fn_arg = "query_method",
    #                               method = query_method,
    #                               verbose = verbose) 
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat = query_granges,
                                     verbose = verbose)
    #### Check format type ####
    target_format <- infer_tabix_format(format = target_format, 
                                        path = target_path,
                                        verbose = verbose) 
    #### Liftover ###  
    ## If builds are identical, will simply return query_dat
    query_granges <- liftover(dat = query_granges, 
                              query_genome = query_genome,
                              target_genome = target_genome, 
                              style = "NCBI",
                              as_granges = TRUE,
                              verbose = verbose) 
    #### VCF ####
    if(target_format=="vcf"){ 
        query_res <- query_vcf(
            target_path = target_path,
            target_index = target_index,
            target_genome = target_genome,
            query_granges = query_granges,
            samples = samples,
            method = query_method,
            overlapping_only = overlapping_only, 
            query_save = query_save,
            save_path = construct_vcf_path(target_path = target_path,
                                           query_granges = query_granges), 
            force_new = query_force_new,
            as_datatable = as_datatable,
            cleanup_tbi = cleanup_tbi,
            conda_env = conda_env,
            verbose = verbose) 
    
    #### Table ####
    } else {  
        query_res <- query_table(
            target_path = target_path,
            target_index = target_index,
            query_granges = query_granges,
            query_method = query_method,
            local = NULL,
            query_save = query_save,
            save_path = query_save_path, 
            overlapping_only = overlapping_only,
            cleanup_tbi = cleanup_tbi,
            conda_env = conda_env,
            nThread = nThread,
            verbose = verbose)
    } 
    return(query_res)
}
