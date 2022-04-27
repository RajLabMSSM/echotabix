#' Query VCF: \pkg{rtracklayer}
#' 
#' Query a subset of a VCF file (remote or local) 
#' using \code{rtracklayer::import}, which is essentially just a wrapper for 
#' \link[Rsamtools]{scanTabix}.
#' \strong{Advantages of \pkg{rtracklayer}:}
#' \itemize{
#' \item{None to speak of.}
#' }
#' \strong{Disadvantages of \pkg{rtracklayer}:}
#' \itemize{
#' \item{Unable to query a subset of samples, 
#' unlike \link[VariantAnnotation]{scanVcf}.}
#' \item{Unable to return results as a structured 
#' \link[VariantAnnotation]{CollapsedVCF} object.}
#' }
#' 
#' @returns Raw text?
#' 
#' @inheritParams construct_query
#' @inheritParams query_vcf 
#'
#' @keywords internal
#' @importFrom GenomicRanges GRanges
#' @importFrom rtracklayer import
#' @importFrom Rsamtools TabixFile
#' @importFrom IRanges IRanges 
#' 
#' @source \href{https://github.com/lawremi/rtracklayer/issues/62}{
#' Inconsistencies with  \pkg{rtracklayer}} 
#' @source
#' \code{
#' BST1 <- echodata::BST1
#' query_dat <- BST1[seq(1, 50), ]
#' target_path <- paste(
#'     "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/",
#'     "ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz",
#'     sep="/"
#' )
#' vcf <- echotabix:::query_vcf_rtracklayer(
#'     target_path = target_path,
#'     query_granges = query_dat)
#' } 
query_vcf_rtracklayer <- function(## Target args 
                                  target_path, 
                                  ## Query args 
                                  query_granges,
                                  samples = character(),
                                  ## Extra args
                                  query_save = FALSE,
                                  save_path = NULL,
                                  verbose = TRUE) {
    
    messager("Querying VCF file using: rtracklayer", v = verbose)  
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat = query_granges, 
                                     verbose = FALSE)
    #### Ensure chromosome format is correct #### 
    query_granges <- fix_query_style(target_path = target_path,
                                     query_granges = query_granges,
                                     verbose = verbose) 
    #### Query ####
    {
        messager("Retrieving data.",v=verbose)
        start_query <- Sys.time() 
        tbx <- Rsamtools::TabixFile(target_path)
        out <- rtracklayer::import(tbx, which=query_granges) 
        # Similar method, but doesn't work
        # out <- BiocIO::import(con = tbx, which=query_granges) 
        report_time(start = start_query, v = verbose)
    } 
    #### Return ####
    return(out)
}

