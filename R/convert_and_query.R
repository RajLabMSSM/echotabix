#' Convert and query
#'
#' If it is not tabix format already
#' (determined by checking for a \code{.tbi}
#' file of the same name in the same directory),
#' the full summary statistics file is converted into tabix format
#' for super fast querying.
#' A query is then made using the min/max genomic positions to extract a
#'  locus-specific summary stats file.
#' 
#' @param study_dir Path to study folder.
#' @param query_save Whether to save the queried data subset.
#' @param nThread Number of threads to use.
#' @param verbose Print messages.
#' @inheritParams construct_query
#' @inheritParams convert
#' @inheritParams query
#' @inheritParams query_vcf
#' @inheritParams query_table
#'
#' @family tabix
#' @return \link[data.table]{data.table} or \link[VariantAnnotation]{VCF}
#' of requested subset of \code{fullSS_path}.
#' @examples 
#' query_dat <- echodata::BST1
#' target_path <- echodata::example_fullSS() 
#' 
#' query_res <- echotabix::convert_and_query( 
#'     target_path = target_path,
#'     target_start_col = "BP", 
#'     query_dat = query_dat,
#'     query_force_new = TRUE) 
#' @export
#' @importFrom data.table fwrite
convert_and_query <- TABIX <- function(## Target args
                                       target_path,
                                       target_format = NULL,
                                       study_dir = NULL,  
                                       #### Parameters - Set 1
                                       target_chrom_col = "CHR",
                                       target_start_col = "POS",
                                       target_end_col = target_start_col, 
                                       
                                       ## Query args
                                       #### Parameters - Set 1
                                       query_dat = NULL,  
                                       query_chrom_col="CHR",
                                       query_start_col="POS",
                                       query_end_col=query_start_col,
                                       query_snp_col="SNP", 
                                       #### Parameters - Set 2
                                       query_chrom = NULL,
                                       query_start_pos=NULL,
                                       query_end_pos=query_start_pos,
                                       #### Extra Parameters
                                       samples = character(),
                                       query_save = TRUE,
                                       locus_dir = tempdir(),
                                       query_save_path=tempfile(
                                           fileext = ".gz"),
                                       
                                       ## Genome builds 
                                       target_genome = "GRCh37", 
                                       query_genome = "GRCh37",
                                       
                                       ## Method args
                                       convert_method=list(
                                           sort_coordinates="bash", 
                                           run_bgzip="Rsamtools",
                                           index="Rsamtools"),
                                       query_method=c(
                                           "rsamtools",
                                           "seqminer", 
                                           "conda"), 
                                       conda_env = "echoR",
                                       
                                       ### Force new 
                                       convert_force_new = FALSE,
                                       query_force_new = FALSE,
                                       
                                       ## Extra args
                                       nThread = 1,
                                       verbose = TRUE) {
     
    #### Check existing tabix ####
    ## Check if  fullSS_path, (or the predicted filename target_path)
    ## are already an indexed tabix file.
    fullSS_path <- target_path;
    target_path <- tabix_path(
        path = fullSS_path,
        study_dir = study_dir
    )
    #### Tabix file exists ####
    if (any(is_tabix(c(fullSS_path, target_path)))) {
        # Checks if the file (in the study dir) already exists,
        # and whether it is a tabix-indexed file.
        # If so, jump ahead and query target_path file.
        messager("echotabix:: Using existing tabix file:",
            target_path,
            v = verbose
        )
        if(is_tabix(fullSS_path)) {
            file.copy(fullSS_path, target_path, overwrite = TRUE)
            target_path <- fullSS_path 
        }
    #### Tabix file does not exist ####
    } else {  
        #### Convert to tabix-index file ####
        tabix_files <- convert(
            fullSS_path = fullSS_path, 
            chrom_col = target_chrom_col,
            start_col = target_start_col,
            end_col = target_end_col, 
            method = convert_method,
            conda_env = conda_env,
            force_new = convert_force_new,
            verbose = verbose) 
        target_path <- tabix_files$data
    }  
    #### Query #### 
    query_res <- query(## Target args
                       target_path=target_path,
                       target_format = target_format,
                       
                       ## Query args
                       #### Parameters - Set 1
                       query_dat = query_dat,  
                       query_chrom_col=query_chrom_col,
                       query_start_col=query_start_col,
                       query_end_col=query_end_col,
                       query_snp_col=query_snp_col, 
                       #### Parameters - Set 2
                       query_chrom = query_chrom,
                       query_start_pos=query_start_pos,
                       query_end_pos=query_end_pos,
                       #### Extra Parameters
                       samples = samples,
                       query_save = query_save,
                       locus_dir = locus_dir,
                       query_save_path=query_save_path,
                       
                       ## Genome builds 
                       target_genome = target_genome, 
                       query_genome = query_genome,
                       
                       ## Method args 
                       query_method=query_method, 
                       conda_env =conda_env,
                       
                       ### Force new  
                       query_force_new = query_force_new,
                       
                       ## Extra args
                       nThread =nThread,
                       verbose = verbose)
    return(query_res)
}
