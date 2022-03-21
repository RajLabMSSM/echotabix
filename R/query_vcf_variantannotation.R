#' Query VCF: \pkg{VariantAnnotation}
#' 
#' Query a subset of a VCF file (remote or local) 
#' using \link[VariantAnnotation]{writeVcf}.
#' \strong{Advantages of \pkg{VariantAnnotation}:}
#' \itemize{
#' \item{Is at least as fast as \link[Rsamtools]{scanTabix}.}
#' \item{Can query a specific subset of samples, 
#' unlike \link[Rsamtools]{scanTabix} which queries all samples at once.}
#' \item{
#' Automatically imports query results as a
#'  \link[VariantAnnotation]{CollapsedVCF} object, which contain lots of 
#' organized information about the query data and can be further processed
#' using other functions from 
#' \pkg{VariantAnnotation} and \pkg{snpStats}. 
#' By contrast, \link[Rsamtools]{scanTabix} returns a raw list of strings
#' that must be parsed by the user.
#' }
#' }
#' 
#' @inheritParams construct_query
#' @inheritParams query_vcf
#' @inheritParams VariantAnnotation::readVcf
#'
#' @keywords internal
#' @importFrom GenomicRanges GRanges
#' @importFrom VariantAnnotation ScanVcfParam readVcf writeVcf 
#' @importFrom IRanges IRanges 
#' 
#' @source \href{https://bioconductor.org/packages/devel/bioc/vignettes/TVTB/inst/doc/VcfFilterRules.html}{
#' \pkg{VariantAnnotation} filtering vignette} 
#' @source
#' \code{
#' BST1 <- echodata::BST1
#' query_dat <- BST1[seq(1, 50), ]
#' target_path <- file.path(
#'     "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/",
#'     "ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz"
#' )
#' vcf <- query_vcf_variantannotation(
#'     target_path = target_path,
#'     query_dat = query_dat)
#' } 
query_vcf_variantannotation <- function(target_path,
                                        target_genome = "GRCh37", 
                                        query_dat,
                                        query_chrom_col="CHR",
                                        query_start_col="POS",
                                        query_end_col=query_start_col,
                                        samples = character(), 
                                        query_snp_col="SNP",
                                        query_save = FALSE,
                                        save_path = NULL,
                                        verbose = TRUE) {
    # https://github.com/MRCIEU/gwasvcf/blob/0c48479836dd16b3f27280b87a3ded41e6034a17/R/query.r#L180
    messager("Querying VCF file using: VariantAnnotation", v = verbose) 
    #### Construct query as granges ####
    gr <- construct_query(query_dat = query_dat,
                          query_chrom_col = query_chrom_col,
                          query_start_col = query_start_col,
                          query_end_col = query_end_col,
                          as_blocks = TRUE,
                          verbose = verbose)
    #### Ensure chromosome format is correct #### 
    gr <- fix_query_style(target_path = target_path,
                          gr = gr,
                          verbose = verbose)
    #### Convert query from GRanges to ScanVcfParam ####
    param <- filter_vcf_query_samples(gr = gr, 
                                      samples = samples, 
                                      verbose = verbose)
    #### Query ####
    {
        messager("Retrieving data.",v=verbose)
        start_query <- Sys.time() 
        vcf <- VariantAnnotation::readVcf(
            ## Can also supply TabixFile, 
            ## but doesn't make any difference in speed
            # file = Rsamtools::TabixFile(target_path),
            file = target_path,
            genome = target_genome,
            param = param
        )
        report_time(start = start_query, v = verbose)
    }
    
    #### Useful functions for extracting SNP-level information ####
    # snp_summary <- VariantAnnotation::snpSummary(vcf)
    # snp_info <- VariantAnnotation::info(vcf) 
    #### Return ####
    return(vcf)
}
