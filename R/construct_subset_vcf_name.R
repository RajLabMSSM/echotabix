#' Construct the path to vcf subset
#'
#'
#' \code{
#' locus_dir <- echodata::locus_dir
#' BST1 <- echodata::BST1
#' vcf_subset <- construct_subset_vcf_name(
#'     dat = BST1,
#'     locus_dir = locus_dir,
#'     vcf_name = "1KGlocal"
#' )
#' }
#' @family LD
#' @keywords internal
construct_subset_vcf_name <- function(dat,
                                      vcf_name = NULL,
                                      locus_dir,
                                      whole_vcf = FALSE) {
    vcf_folder <- get_locus_vcf_folder(locus_dir = locus_dir)
    # Don't use the chr prefix:
    # https://www.internationalgenome.org/faq/how-do-i-get-sub-section-vcf-file/
    dat$CHR <- gsub("chr", "", dat$CHR)
    chrom <- unique(dat$CHR)
    vcf_subset <- file.path(
        vcf_folder,
        if (whole_vcf) {
            paste(basename(vcf_name), paste0("chr", chrom), sep = ".")
        } else {
            paste(basename(locus_dir), basename(vcf_name), sep = ".")
        }
    )
    dir.create(path = dirname(vcf_subset), recursive = T, showWarnings = F)
    if (!any(endsWith(vcf_subset, c(".vcf.gz", ".vcf.bgz", ".vcf")))) {
        vcf_subset <- paste0(vcf_subset, ".vcf.bgz")
    }
    vcf_subset <- normalizePath(vcf_subset)
    return(vcf_subset)
}
