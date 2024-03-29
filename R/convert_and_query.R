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
#' @param target_path Path to full GWAS/QTL summary statistics file.
#' @param target_index Tabix index file for \code{target_path}.
#' @param target_format Format of the \code{target_path} file: "vcf" or "table".
#' @param target_chrom_col Name of the chromosome column 
#' in the \code{target_path} file.
#' @param target_start_col Name of the genomic start position column 
#' in the \code{target_path} file.
#' @param target_end_col Name of the genomic end position column 
#' in the \code{target_path} file.
#' @param query_granges \link[GenomicRanges]{GRanges} object 
#' to be used for querying the \code{target_path} file. 
#' Alternatively, can be variant-level summary statistics to be converted into a
#' \link[GenomicRanges]{GRanges} object by \link[echotabix]{construct_query}. 
#' @param query_save_path Path to save retrieved query subset to.
#' @param query_genome Genome build that the \code{query_granges} is aligned to.
#' @param query_method Method used for querying. 
#' See \link[echotabix]{query} for available options.
#' @param convert_force_new If the \code{target_path} is already in 
#' sorted/indexed tabix format, set \code{convert_force_new=TRUE} to re-convert 
#' it into tabix format.
#' @param query_force_new If the query subset (\code{query_save_path})
#'  already exists, set \code{query_force_new=TRUE}
#'   to retrieve a new query subset. 
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
#' of requested subset of \code{target_path}.
#' @examples 
#' query_dat <- echodata::BST1
#' target_path <- echodata::example_fullSS() 
#' 
#' query_res <- echotabix::convert_and_query( 
#'     target_path = target_path,
#'     target_start_col = "BP", 
#'     query_granges = query_dat,
#'     query_force_new = TRUE) 
#' @export
#' @importFrom data.table fwrite
convert_and_query <- function(## Target args
                              target_path,
                              target_index = paste0(target_path,".tbi"),
                              target_format = NULL,
                              study_dir = NULL,  
                              #### Parameters - Set 1
                              target_chrom_col = "CHR",
                              target_start_col = "POS",
                              target_end_col = target_start_col, 
                              
                              ## Query args 
                              query_granges, 
                              samples = character(),
                              #### Extra Parameters 
                              query_save = TRUE, 
                              query_save_path=tempfile(
                                  fileext = ".gz"),
                              
                              ## Genome builds 
                              target_genome = "GRCh37", 
                              query_genome = "GRCh37",
                              
                              ## Method args
                              convert_methods=list(
                                  sort_coordinates="bash", 
                                  run_bgzip="Rsamtools",
                                  index="Rsamtools"),
                              query_method=c(
                                  "rsamtools",
                                  "seqminer", 
                                  "conda"), 
                              conda_env = "echoR_mini",
                              
                              ### Force new 
                              convert_force_new = FALSE,
                              query_force_new = FALSE,
                              
                              ## Extra args
                              nThread = 1,
                              verbose = TRUE) {
     
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat=query_granges,
                                     verbose = verbose)
    #### Check existing tabix ####
    ## Check if  target_path, (or the predicted filename target_path)
    ## are already an indexed tabix file.
    target_path_original <- data.table::copy(target_path);
    target_path <- construct_tabix_path(target_path = target_path,
                                        study_dir = study_dir)
    #### Tabix file exists ####
    if (any(is_tabix(c(target_path_original, target_path)))) {
        # Checks if the file (in the study dir) already exists,
        # and whether it is a tabix-indexed file.
        # If so, jump ahead and query target_path file.
        messager("Using existing tabix file:",target_path_original,
                 v = verbose)
        if(is_tabix(target_path_original)) {
            file.copy(from = target_path_original, 
                      to = target_path,
                      overwrite = TRUE)
            target_path <- target_path_original 
        }
    #### Tabix file does not exist ####
    } else {  
        #### Convert to tabix-index file ####
        tabix_files <- convert(
            target_path = target_path_original, 
            chrom_col = target_chrom_col,
            start_col = target_start_col,
            end_col = target_end_col, 
            convert_methods = convert_methods,
            conda_env = conda_env,
            force_new = convert_force_new,
            verbose = verbose) 
        target_path <- tabix_files$path
    }  
    #### Query #### 
    query_res <- query(## Target args
                       target_path = target_path,
                       target_index = target_index,
                       target_format = target_format,
                       
                       ## Query args
                       query_granges = query_granges,
                       #### Extra Parameters 
                       query_save = query_save,
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
