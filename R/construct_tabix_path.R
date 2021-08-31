construct_tabix_path <- function(fullSS_path,
                                 study_dir = NULL) {
    study_dir <- if (is.null(study_dir)) dirname(fullSS_path) else study_dir
    fullSS.gz <- gsub(".gz|.bgz", ".bgz", fullSS_path)
    tabix_out <- file.path(study_dir, basename(fullSS.gz))
    return(tabix_out)
}
