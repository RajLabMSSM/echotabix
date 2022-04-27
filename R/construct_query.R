#' Construct query
#' 
#' Convert a \link[data.table]{data.table} containing variant-level 
#' summary statistics into a \link[GenomicRanges]{GRanges} object to be used 
#' for querying tabix-indexed files. Depending on the parameters supplied, 
#' the resulting \link[GenomicRanges]{GRanges} object can contain one or more
#' ranges, across one or more chromosomes. 
#' @section Parameters sets:
#' If you supply the \emph{Set 1} of parameters, you do not need to supply
#' the \emph{Set 2} parameters (and vice versa). 
#' \emph{Extra Parameters} apply to both \emph{Set 1} and \emph{Set 2}.
#' \itemize{
#' \item{Set 1: }{
#' \code{query_chrom},  \code{query_start_pos},  \code{query_end_pos}
#' }
#' \item{Set 2: }{
#'  \code{query_dat},  \code{query_chrom_col}, 
#'   \code{query_start_col},  \code{query_end_col},  \code{as_blocks}
#' }
#' \item{Extra Parameters: }{
#'   \code{style},  \code{samples}, \code{as_string},  \code{verbose}
#' }
#' } 
#' 
#' @param query_chrom Which chromosome to query (e.g. 1 or "chr1").
#' @param query_start_pos The first position to query.
#' @param query_end_pos The last position to query.
#' 
#' @param query_dat Variant-level summary statistics.
#' @param query_chrom_col Name of the chromosome column in 
#' \code{query_dat} (e.g. "CHR").
#' @param query_start_col Name of the starting genomic position
#'  column in \code{query_dat} (e.g. "POS","start").
#' @param query_end_col Name of the ending genomic position
#'  column in \code{query_dat} (e.g. "POS","end"). 
#'  Can be the same as \code{query_start_col} when \code{query_dat}
#'   only contains SNPs that span 1 base pair (bp) each.
#' @param query_snp_col Name of the RSID or variant ID column (e.g. "SNP").
#' @param standardise_colnames Automatically rename all columns to a standard 
#' nomenclature using \link[MungeSumstats]{standardise_header}.
#' 
#' @param as_blocks If \code{TRUE} (default), will query a single range 
#'  per chromosome that covers all ranges requested plus anything in between. 
#' @param style Style to return \link[GenomicRanges]{GRanges} in:
#' \itemize{
#' \item{"NCBI": }{Chromosome format: 1 (default)}
#' \item{"UCSC": }{Chromosome format: "chr1"}
#' }
#' Uses \link[GenomeInfoDb]{seqlevelsStyle}.
#' @param samples [Optional] Sample names to subset the VCF by. 
#' If this option is used, the \link[GenomicRanges]{GRanges} object will be
#' converted to a \link[VariantAnnotation]{ScanVcfParam} for usage by 
#' \link[VariantAnnotation]{readVcf}. 
#' @param as_string Convert a \link[GenomicRanges]{GRanges} object 
#' to a concatenated string of coordinates 
#' (e.g. "chr4:70000-90000,chr10:200-150001"). 
#' This can be used for specifying
#' which regions you want to query (e.g. when using \code{tabix}).
#' Uses \link[echotabix]{granges_to_string}.
#' @param verbose Print messages.
#' 
#' @returns \link[GenomicRanges]{GRanges} or
#' \link[VariantAnnotation]{ScanVcfParam} object.
#' @export
#' @importFrom GenomicRanges GRanges GRangesList mcols
#' @importFrom IRanges IRanges
#' @importFrom data.table copy
#' @examples 
#' gr <- echotabix::construct_query(query_dat=echodata::BST1)
construct_query <- function(## Set 1
                            query_chrom=NULL, 
                            query_start_pos=NULL,
                            query_end_pos=query_start_pos, 
                            ## Set 2
                            query_dat=NULL,
                            query_chrom_col="CHR",
                            query_start_col="POS",
                            query_end_col=query_start_col,
                            query_snp_col="SNP",
                            standardise_colnames = FALSE,
                            ## Extra args
                            as_blocks=TRUE,
                            style=c("NCBI", "UCSC"),
                            samples=character(),
                            as_string=FALSE,
                            verbose=TRUE){
    
    #### If it's already a GRanges, simply return the object ####
    if(methods::is(query_dat, "GRanges")) {
        messager("query_dat is already a GRanges object. Returning directly.",
                 v=verbose)
        if(!is.null(style)){
            query_dat <- granges_style(gr = query_dat, style=style)
        }
        return(query_dat)
    }
    #### Handle empty end_ args ####
    if(is.null(query_end_pos)) query_end_pos <- query_start_pos
    if(is.null(query_end_col)) query_end_col <- query_start_col
    check_set1 <- function(query_chrom, 
                           query_start_pos,
                           query_end_pos){
        if(is.null(query_chrom)) {
            stop("Must provide query_chrom.")
        }
        if(is.null(query_start_pos)) {
            stop("Must provide query_start_pos.")
        } 
        if(is.null(query_end_pos)) {
            stop("Must provide query_end_pos.")
        }  
    }
    check_set2 <- function(query_dat,
                           query_chrom_col, 
                           query_start_col,
                           query_end_col){
        if(is.null(query_chrom_col) | 
           (!query_chrom_col %in% colnames(query_dat))) {
            stop("Must provide valid query_chrom_col.")
        }
        if(is.null(query_start_col) | 
           (!query_start_col %in% colnames(query_dat))) {
            stop("Must provide valid query_start_col.")
        } 
        if(is.null(query_end_col) | 
           (!query_end_col %in% colnames(query_dat))) {
            stop("Must provide valid query_end_col.")
        }  
    }
    #### Make a series of ranges (can span multiple chromosomes) ####
    if(!is.null(query_dat)){ 
        #### Standardise colnames ####
        if(standardise_colnames){
            requireNamespace("MungeSumstats")
            query_dat <- MungeSumstats::standardise_header(
                sumstats_dt = data.table::copy(query_dat), 
                uppercase_unmapped = FALSE, 
                return_list = FALSE)
            query_dat <- echodata::mungesumstats_to_echolocatoR(dat = query_dat)
        } 
        #### Construct query_granges ####
        messager("Constructing GRanges query using min/max",
                 "ranges across one or more chromosomes.",v=verbose)
        check_set2(query_dat=query_dat,
                   query_chrom_col=query_chrom_col, 
                   query_start_col=query_start_col,
                   query_end_col=query_end_col)
        #### Only make 1 large query per chromosome ####
        if(as_blocks){
            messager("+ as_blocks=TRUE:",
                     "Will query a single range per chromosome that",
                     "covers all regions requested",
                     "(plus anything in between).",v=verbose) 
            #### Iterate over chromosomes ####
            grl <- lapply(unique(query_dat[[query_chrom_col]]), function(chr){
                dat_sub <- query_dat[query_dat[[query_chrom_col]]==chr,]
                GenomicRanges::GRanges(
                    seqnames = as.integer(chr),
                    ranges = IRanges::IRanges(
                        start = as.integer(min(dat_sub[[query_start_col]],
                                               na.rm = TRUE)),
                        end = as.integer(max(dat_sub[[query_end_col]], 
                                             na.rm = TRUE))
                    )
                )
            })
            gr <- unlist(GenomicRanges::GRangesList(grl))
            #### Record SNPs by collapsing them into one row ####
            if(query_snp_col %in% colnames(query_dat)){
                GenomicRanges::mcols(gr)["SNP"] <- paste(
                    unique(query_dat[[query_snp_col]]), collapse = ";"
                    )
            }
        #### Make a series of very small queries ####
        } else {
            gr <- GenomicRanges::GRanges(
                seqnames = as.integer(query_dat[[query_chrom_col]]),
                ranges = IRanges::IRanges(
                    start = as.integer(query_dat[[query_start_col]]),
                    end = as.integer(query_dat[[query_end_col]])
                )
            )
            #### Record SNP IDs ####
            if(query_snp_col %in% colnames(query_dat)){
                GenomicRanges::mcols(gr)["SNP"] <- query_dat[[query_snp_col]]
            }
        } 
    #### Make single range (can only span one chromosome) ####
    } else {
        messager("Constructing GRanges query using min/max",
                 "ranges within a single chromosome.",v=verbose) 
        #### Check inputs: set 1 ####
        check_set1(query_chrom = query_chrom, 
                   query_start_pos = query_start_pos, 
                   query_end_pos = query_end_pos)
        gr <- GenomicRanges::GRanges(
            seqnames = as.integer(gsub("chr","",query_chrom[1],ignore.case = TRUE)),
            ranges = IRanges::IRanges(
                start = as.integer(min(query_start_pos, na.rm = TRUE)),
                end = as.integer(max(query_end_pos, na.rm = TRUE))
            )
        )
    } 
    #### Record  query_snp_col as a dummy col ####
    if(!query_snp_col %in% colnames(GenomicRanges::mcols(gr))){
        GenomicRanges::mcols(gr)["SNP"] <- NA
    }
    if(!is.null(style)){
        gr <- granges_style(gr = gr, style=style)
    }
    #### Convert to string and return early ####
    if(as_string){
        string <- granges_to_string(gr=gr,
                                    verbose=verbose)
        return(string)
    } 
    #### Subset by samples [Optional] ####
    ## If samples is empty, will simply return the original GRanges object.
    out <- filter_vcf_query_samples(gr=gr,
                                    samples=samples,
                                    verbose=verbose) 
    return(out)
}
