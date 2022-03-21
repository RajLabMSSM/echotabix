#' Run bgzip: Rsamtools
#' 
#' Support function for \link[echotabix]{run_bgzip}.
#' @inheritParams run_bgzip
#' @keywords internal 
run_bgzip_rsamtools <- function(fullSS_path,
                                bgz_file,
                                force_new=TRUE,
                                verbose=TRUE){ 
    if(requireNamespace("Rsamtools")){
        rhtslib_warning(verbose = verbose)
    }
    messager("echotabix:: bgzipping file with Rsamtools.", v = verbose) 
    bgz_file <- Rsamtools::bgzip(file = fullSS_path, 
                                 dest = bgz_file,
                                 overwrite = force_new)
    return(bgz_file)
}
