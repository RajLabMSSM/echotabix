#' Query VCF file
#'
#' Query a (remote) Variant Call Format (VCF) file.
#'
#' @param dat SNP-level data table.
#' @param vcf_url URL or path to VCF.
#' @param locus_dir Directory to store LD in.
#' @param vcf_name VCF reference name (e.g. "1KGphase1").
#' @param ref_genome Genome build of the VCF file.
#' @param samples Sample names to subset the VCF by before computing LD.
#' @param force_new_vcf Force the creation of a new LD file even if one exists.
#' @param verbose Print messages.
#'
#' @return \link[VariantAnnotation]{VCF} object.
#'
#' @family LD
#' @importFrom data.table fwrite
#' @importFrom VariantAnnotation readVcf
#' @export
query_vcf <- function(dat,
                      vcf_url,
                      locus_dir = file.path(tempdir(), "LD"),
                      vcf_name = gsub(".vcf|.gz.|bgz", "", basename(vcf_url)),
                      ref_genome = "GRCh37",
                      samples = NULL,
                      force_new_vcf = FALSE,
                      verbose = TRUE) {
    messager("Querying VCF tabix file.",v=verbose)
    vcf_url <- normalizePath(vcf_url)
    vcf_subset <- construct_subset_vcf_name(
        dat = dat,
        locus_dir = locus_dir,
        vcf_name = vcf_name
    )
    # CHECK FOR EMPTY VCF FILES!
    ## These can be created if you stop the query early, or if the url fails.
    remove_empty_tabix(
        f = vcf_subset,
        verbose = verbose
    )
    #### Import existing file or create new one ####
    if ((!file.exists(vcf_subset)) | force_new_vcf) {
        vcf <- query_vcf_variantannotation(
            vcf_url = vcf_url,
            dat = dat,
            samples = samples,
            genome = ref_genome,
            save_path = vcf_subset
        )
    } else {
        messager("+ Identified existing VCF subset file. Importing...",
            vcf_subset,
            v = verbose
        )
        vcf <- VariantAnnotation::readVcf(vcf_subset)
    }
    messager("Returning VCF with", formatC(nrow(vcf), big.mark = ","), "SNPs",
        "across", formatC(ncol(vcf), big.mark = ","), "samples.",
        v = verbose
    )
    return(vcf)
}
