#' Query VCF file
#'
#' Query a Variant Call Format (VCF) file. 
#' The VCF file can be either local or remote.
#'
#' @param target_path Path to local VCF file or remote URL.
#' @param target_genome Genome build of the VCF file. 
#' @param overlapping_only Remove variants that do not overlap with the 
#' positions in \code{query_dat}.
#' @param query_save Whether to save the results of the query on disk. 
#' \emph{Note}: Writing to disk can take some time.
#' @param save_path Path to save VCF results in. 
#' @param force_new Force the creation of a new VCF subset file
#'  even if one exists.
#' @param as_datatable Return the VCF subset 
#' file as a \link[data.table]{data.table} 
#' (using \link[echotabix]{vcf_to_dt}).
#' If \code{save_path=TRUE} the file will still be saved 
#' as a bgzip-compressed VCF file. 
#' @param verbose Print messages.
#'
#' @inheritParams construct_query
#' @inheritParams VariantAnnotation::readVcf
#' 
#' @return \link[VariantAnnotation]{VCF} object, 
#' or \link[data.table]{data.table} (when \code{as_datatable=TRUE}).
#'
#' @family tabix functions
#' @importFrom data.table fwrite
#' @importFrom VariantAnnotation readVcf
#' @export
#' @examples
#' query_dat <- echodata::BST1
#' target_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
#'                     package = "echotabix")
#' 
#' #### Import ####
#' vcf <- echotabix::query_vcf(
#'     query_granges = query_dat,
#'     target_path = target_path)
query_vcf <- function(## Target args
                      target_path,
                      target_genome = "GRCh37",
                      ## Query args 
                      query_granges,
                      samples = character(),
                      ## Extra args
                      overlapping_only = FALSE, 
                      query_save = TRUE,
                      save_path = construct_vcf_path(
                          target_path = target_path,
                          query_granges = query_granges), 
                      force_new = FALSE,
                      as_datatable = FALSE,
                      verbose = TRUE) {
    
    messager("Querying VCF tabix file.",v=verbose)   
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat = query_granges,
                                     verbose = FALSE)
    #### CHECK FOR EMPTY VCF FILES! ####
    ## These can be created if you stop the query early, or if the query fails.
    remove_empty_tabix(f = save_path,
                       verbose = verbose)
    #### Import existing file or create new one ####
    if ((!file.exists(save_path)) | force_new) {
        #### Query ####
        vcf <- query_vcf_variantannotation(
            target_path = target_path, 
            query_granges = query_granges, 
            target_genome = target_genome,
            samples = samples,
            query_save = query_save,
            save_path = save_path
        )
        #### Remove non-overlapping variants ####
        if(overlapping_only){ 
            vcf <- filter_vcf_snps(vcf=vcf,
                                   query_granges=query_granges,
                                   verbose=verbose)
        }  
        #### Save ####
        save_path <- save_vcf(vcf=vcf,
                              query_save=query_save,
                              save_path=save_path,
                              verbose=verbose)
    } else {
        messager("Importing existing VCF file:",save_path,v = verbose)
        vcf <- VariantAnnotation::readVcf(save_path)
    }
    #### Report ####
    report_vcf(vcf=vcf,
               verbose=verbose)
    #### Return object #### 
    if(as_datatable){ 
        vcf_dt <- vcf_to_dt(vcf=vcf,
                            verbose = verbose) 
        return(vcf_dt)
    } else {
        return(vcf)
    }   
}
