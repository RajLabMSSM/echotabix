#' Save VCF 
#' 
#' Save a VCF file to disk.
#' @inheritParams query_vcf
#' @keywords internal
#' @returns save_path
save_vcf <- function(vcf,
                     query_save,
                     save_path,
                     verbose=TRUE){
    #### Save ####
    if (isTRUE(query_save) && (!is.null(save_path))) {
        messager("Saving VCF subset ==>", save_path, v = verbose)
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
        start_save <- Sys.time() 
        save_path <- VariantAnnotation::writeVcf(
            obj = vcf,
            # Otherwise will name the file "...bgz.bgz"
            filename = gsub(".bgz|.gz", "", save_path),
            index = TRUE
        )
        report_time(start = start_save, v = verbose)
    } 
    return(save_path)
}
