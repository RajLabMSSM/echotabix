#' Construct tabix path
#' 
#' Given some summary stats file, 
#' construct a name for the resulting sorted, bgzip-compressed tabix file.
#' @param study_dir [optional] Path to study-specific subfolder.
#' @inheritParams convert_and_query
#' 
#' @family tabix functions
#' @export
#' @examples
#' bgz_file <- echotabix::construct_tabix_path(
#'     target_path = "mysumstatsfile.vcf.tsv.gz")
construct_tabix_path <- function(target_path,
                                 study_dir = NULL) {
    
    study_dir <- if (is.null(study_dir)) dirname(target_path) else study_dir
    target_path.gz <- gsub(paste(get_vcf_suffixes(),collapse = "|"), 
                           ".bgz", target_path)
    target_path.gz <- gsub("\\.gz|\\.bgz", ".bgz", target_path)
    if(!endsWith(target_path.gz,".bgz")) {
        target_path.gz <- paste0(target_path.gz,".bgz")
    }
    tabix_out <- file.path(study_dir, basename(target_path.gz))
    return(tabix_out)
}
