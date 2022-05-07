#' Is VCF
#' 
#' Determine whether a file if a VCF based on its path name.
#' @param path Local or remote file path.
is_vcf <- function(path){
    vcf_suffixes <- suffixes(tabular = FALSE, 
                             tabular_compressed = FALSE)
    grepl(pattern = paste(vcf_suffixes, collapse = "|"),
          x = path, ignore.case = TRUE)
}