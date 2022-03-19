#' Construct tabix path
#' 
#' Given some summary stats file, 
#' construct a name for the resulting bgzip-compressed tabix file.
#' @param path Path to file.
#' @param study_dir [optional] Path to study-specific subfolder.
#' @export
#' @examples 
#' bgz_file = echotabix::construct_tabix_path(path = "mysumstatsfile.tsv.gz") 
construct_tabix_path <- function(path,
                                 study_dir = NULL) {
    study_dir <- if (is.null(study_dir)) dirname(path) else study_dir
    path.gz <- gsub(".gz|.bgz", ".bgz", path)
    if(!endsWith(path.gz,".bgz")) path.gz <- paste0(path.gz,".bgz")
    tabix_out <- file.path(study_dir, basename(path.gz))
    return(tabix_out)
}
