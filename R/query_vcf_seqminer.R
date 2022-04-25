#' Query VCF: \pkg{seqminer}
#' 
#' Query a subset of a VCF file (remote or local) 
#' using \link[seqminer]{tabix.read.table}.
#' \strong{Advantages of \pkg{seqminer}:}
#' \itemize{
#' \item{Does not rely on \pkg{Rsamtools} or \pkg{Rhtslib}, 
#' which are very outdated and prone to breaking.}
#' }
#' \strong{Disadvantages of \pkg{rtracklayer}:}
#' \itemize{
#' \item{Unable to query a subset of samples, 
#' unlike \link[VariantAnnotation]{scanVcf}.}
#' \item{Unable to return results as a structured 
#' \link[VariantAnnotation]{CollapsedVCF} object.}
#' }
#' 
#' @source \href{https://github.com/zhanxw/seqminer/issues/26}{
#' seqminer::readVCFToListByRange and seqminer::readVCFToMatrixByRange
#'  do not work and instead cause Rstudio to crash.
#' }
#' 
#' @returns A variant x sample data.frame
#' 
#' @inheritParams construct_query
#' @inheritParams query_vcf 
#'
#' @keywords internal
#' @importFrom GenomicRanges GRanges
#' @importFrom seqminer tabix.read.table 
#' @importFrom IRanges IRanges 
#'  
#' @source
#' \code{ 
#' query_dat <- echodata::BST1[seq(1, 50), ]
#' target_path <- paste(
#'     "ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/release/20110521/",
#'     "ALL.chr4.phase1_release_v3.20101123.snps_indels_svs.genotypes.vcf.gz",
#'     sep="/"
#' )
#' vcf <- echotabix:::query_vcf_rtracklayer(
#'     target_path = target_path,
#'     query_granges = query_dat)
#' } 
query_vcf_seqminer <- function(## Target args 
                               target_path,
                               target_genome = "GRCh37", 
                               ## Query args 
                               query_granges,
                               samples = character(),
                               ## Extra args
                               query_save = FALSE,
                               save_path = NULL,
                               verbose = TRUE) {

    messager("Querying VCF file using: seqminer", v = verbose)  
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat = query_granges,
                                     verbose = FALSE) 
    #### Ensure chromosome format is correct #### 
    query_granges <- fix_query_style(target_path = target_path,
                                     query_granges = query_granges,
                                     verbose = verbose) 
    #### Convert query to string ####
    query_str <- granges_to_string(gr = query_granges,
                                   verbose = verbose)
    #### Determine cols/samples to query ####
    main_cols <- c("CHROM","POS","ID","REF","ALT",
                   "QUAL","FILTER","INFO","FORMAT")
    vcfColumn <- unique(c(main_cols,toupper(samples)))
    #### Query ####
    {
        messager("Retrieving data.",v=verbose)
        start_query <- Sys.time() 
        dat <- seqminer::tabix.read.table(tabixFile = target_path, 
                                          tabixRange = query_str)  
        dat <- data.table::data.table(dat)
        data.table::setnames(x = dat, 
                             old = colnames(dat), 
                             new = toupper(colnames(dat))) 
        report_time(start = start_query, v = verbose)
    } 
    #### Filter samples ####
    ## Since seqminer doesn't have the ability to subset samples 
    ## during querying, need to do this afterwards.
    if(length(samples)>0){ 
        dat <- dat[,vcfColumn, with=FALSE]
    }
    # #### Convert to VCF #### 
    # gr <- echodata::dt_to_granges(dat = dat,
    #                               chrom_col = "CHROM",
    #                               start_col = "POS")
    # vr <- VariantAnnotation::makeVRangesFromGRanges(gr = gr)
    # vr <- methods::as(gr,"VRanges") 
    # 
    # vcf <- VariantAnnotation::VCF()
    # ## rowRanges
    # vcf@rowRanges <- gr[,0]
    # ## fixed
    # fixed_cols <- colnames(VariantAnnotation::fixed(vcf))
    # vcf@fixed <- methods::as(
    #     dat[,colnames(dat)[colnames(dat) %in% fixed_cols], with=FALSE],
    #     "DataFrame"
    # )
    # ## info
    # fixed_cols <- colnames(VariantAnnotation::info(vcf)) 
    # cols <- stringr::str_split(dat$INFO[1], "=|;")[[1]]
    # cols <- cols[seq(1,length(cols),2)]
    # for(x in cols){
    #     dat[,x:=grep(pattern = paste0(x,"=*;"), x=INFO), with=FALSE]
    # }
    # inf <- dat[, cols := data.table::tstrsplit(INFO, ";", fixed=TRUE), with=FALSE]
 
    #### Return ####
    return(dat)
}

