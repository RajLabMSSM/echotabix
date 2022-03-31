#' Genome build liftover
#'
#' Transfer genomic coordinates from one genome build to another.
#'
#' @source \href{https://doi.org/doi:10.18129/B9.bioc.liftOver}{liftOver}
#' @source \href{https://hgdownload.cse.ucsc.edu/goldenpath/hg19/liftOver/}{
#' UCSC chain files}
#'
#' @param dat \link[data.table]{data.table} of GWAS/QTL summary statistics.
#' @param query_genome name of the reference genome used for the GWAS
#'  (e.g. "GRCh37" or "GRCh38"). Argument is case-insensitive. 
#' @param target_genome name of the reference genome to convert to
#' ("GRCh37" or "GRCh38"). This will only occur if the \code{query_genome} does
#' not match the \code{target_genome}. 
#' @param pos_prefix After the \code{start_col}\code{end_col}
#'  has been lifted over, how should these columns be named?
#' @param as_granges Return results as \link[GenomicRanges]{GRanges} 
#' instead of a \link[data.table]{data.table} (default: \code{FALSE}).
#' @param style Style to return \link[GenomicRanges]{GRanges} object in
#' (e.g.  "NCBI" = 4; "UCSC" = "chr4";) (default: \code{"NCBI"}).
#' @param verbose Print messages.
#' @inheritParams construct_query
#' @inheritParams echodata::dt_to_granges
#'
#' @returns Lifted summary stats in \code{data.table} 
#' or \link[GenomicRanges]{GRanges} format.
#' @family liftover functions
#' 
#' @export
#' @importFrom rtracklayer liftOver width strand end
#' @importFrom GenomeInfoDb seqnames mapGenomeBuilds
#' @importFrom data.table data.table as.data.table setnames :=
#' @importFrom echodata dt_to_granges
#' @importFrom methods is
#' @examples 
#' dat <- echodata::BST1
#' #### hg19 ==> hg38 ####
#' dat_lifted <- echotabix::liftover(
#'     dat = dat,
#'     query_genome = "hg19",
#'     target_genome = "hg38"
#' )
liftover <- function(dat,
                     query_genome,
                     target_genome,
                     query_chrom_col = "CHR",
                     query_start_col = "POS",
                     query_end_col = query_start_col,
                     pos_prefix = "POS",
                     as_granges = FALSE,
                     style = "NCBI", 
                     verbose = TRUE) {
    
    width <- strand <- end <- seqnames <- NULL;
    
    #### Map genome build synonyms #### 
    query_ucsc <- if(!is.null(query_genome)){
        GenomeInfoDb::mapGenomeBuilds(genome = query_genome)$ucscID[1]
    } else {query_genome}
    target_ucsc <- if(!is.null(target_genome)){
        GenomeInfoDb::mapGenomeBuilds(genome = target_genome)$ucscID[1]
    } else {target_genome}
    
    #### Check if one or more of the genomes couldn't be mapped ####
    null_builds <- c("query_genome", "target_genome")[
        c(is.null(query_ucsc), is.null(target_ucsc))
        ] 
    if(length(null_builds)>0){
        stp <- paste0("Could not recognize genome build of:\n",
                      paste(" -",null_builds,collapse = "\n"))
        stop(stp)
    } 
    
    #### Check if liftover is necessary ####
    ## i.e. the desired genome build isn't the current one
    if (query_ucsc != target_ucsc) { 
        #### Check that dat is not NULL ####
        if(is.null(dat)){
            stop("query_dat must not be NULL",
                 " when query_genome != target_genome")
        }
        #### Check that liftover is available ####
        if(query_ucsc=="hg38" && target_ucsc=="hg19") {
            build_conversion <- "hg38ToHg19"
        } else if (query_ucsc=="hg19" && target_ucsc=="hg38"){
            build_conversion <- "hg19ToHg38"
        } else {
            stop("Can only perform liftover between hg19 <---> hg38")
        }
        messager("Performing liftover: ", query_ucsc, " ==> ",target_ucsc, 
                 v=verbose) 
        #### Convert to GRanges (if necessary) ####
        if (methods::is(dat, "GRanges")) {
            #### ensure seqnames are in UCSC format ####
            gr <- granges_style(gr = dat,
                                style = "UCSC")
        } else {
            #### Convert to GRanges and ensure seqnames are in UCSC format #### 
            gr <- echodata::dt_to_granges(
                dat = dat,
                chrom_col = query_chrom_col,
                start_col = query_start_col,
                end_col = query_end_col,
                style = "UCSC",
                verbose = verbose
            )
        }
        #### Specify chain file ####
        chain <- get_chain_file(
            build_conversion = build_conversion, 
            verbose = verbose
        )
        #### Liftover ####
        gr_lifted <- unlist(rtracklayer::liftOver(
            x = gr,
            chain = chain
        ))
        #### Return format ####
        if (as_granges) {
            gr_lifted <- granges_style(
                gr = gr_lifted,
                style = style
            )
            return(gr_lifted)
        } else {
            dat <- data.table::as.data.table(gr_lifted)
            #### rename columns back to original ####
            ## Note: "seqnames" col can be removed since the "CHR" column was 
            ## retained by dt_to_granges().
            dat[, width := NULL]
            dat[, strand := NULL]
            dat[, seqnames := NULL]
            #### Remove end_col if it was the same as start_col ####
            if (query_start_col == query_end_col) {
                dat[, end := NULL]
                data.table::setnames(dat, "start", query_start_col)
            } else {
                data.table::setnames(dat,c("start","end"),
                                     c(query_start_col, query_end_col) 
                )
            }
        }
    }
    return(dat)
}
