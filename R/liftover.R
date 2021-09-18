#' Genome build liftover
#'
#' Transfer genomic coordinates from one genome build to another.
#'
#' @source \href{https://doi.org/doi:10.18129/B9.bioc.liftOver}{liftOver}
#' @source \href{https://hgdownload.cse.ucsc.edu/goldenpath/hg19/liftOver/}{
#' UCSC chain files}
#'
#' @param sumstats_dt data table of the GWAS/QTL summary statistics.
#' @param convert_ref_genome name of the reference genome to convert to
#' ("GRCh37" or "GRCh38"). This will only occur if the current genome build does
#' not match. Default is not to convert the genome build (\code{NULL}).
#' @param ref_genome name of the reference genome used for the GWAS ("GRCh37" or
#' "GRCh38"). Argument is case-insensitive.
#'  Default is \code{NULL} which infers the reference genome from the data.
#' @param as_granges Return results as \link[GenomicRanges]{GRanges}.
#' @param style \link[GenomicRanges]{GRanges} style 
#' (e.g. "UCSC" = "chr4; "NCBI" = 4).
#' @param verbose Print messages.
#' @inheritParams dt_to_granges
#'
#' @return Lifted summary stats \code{data.table}
#'
#' @export
#' @importFrom rtracklayer liftOver width strand end
#' @importFrom GenomeInfoDb seqnames
#' @importFrom data.table data.table as.data.table setnames :=
#' @importFrom methods is
liftover <- function(sumstats_dt,
                     convert_ref_genome = NULL,
                     ref_genome = NULL,
                     chrom_col = "CHR",
                     start_col = "POS",
                     end_col = start_col,
                     as_granges = FALSE,
                     style="NCBI",
                     verbose = TRUE) {
    # check it's necessary i.e. the desired ref genome isn't the current one
    if (!is.null(convert_ref_genome) &&
        toupper(convert_ref_genome) != toupper(ref_genome)) {
        msg <- paste0(
            "Performing liftover: ", ref_genome, " ==> ",
            convert_ref_genome
        )
        message(msg)

        if (toupper(convert_ref_genome) %in% 
            c("GRCH38", "HG38")) {
            build_conversion <- "hg38ToHg19"
            ucsc_ref <- "hg38"
        } else if (toupper(convert_ref_genome) %in% 
                   c("GRCH37", "HG37", "HG19")) {
            build_conversion <- "hg19ToHg38"
            ucsc_ref <- "hg19"
        }

        #### Convert to GRanges (if necessary) ####
        if(methods::is(sumstats_dt,"GRanges")){
            gr <- granges_style(sumstats_dt, 
                                style="UCSC")
        } else {
            gr <- dt_to_granges(
                dat = sumstats_dt,
                style = "UCSC",
                chrom_col = chrom_col,
                start_col = start_col,
                end_col = end_col
            )
        } 
        #### Specify chain file ####
        chain <- get_chain_file(
            build_conversion = build_conversion,
            ucsc_ref = ucsc_ref,
            verbose = verbose
        )
        #### Liftover ####
        gr_lifted <- unlist(rtracklayer::liftOver(
            x = gr,
            chain = chain
        ))
        #### Return format ####
        if(as_granges){
            gr_lifted <- granges_style(gr = gr_lifted, 
                                       style = style)
            return(gr_lifted)
        } else {
            sumstats_dt <- data.table::as.data.table(gr_lifted)
            #### Rename columns back to original ####
            sumstats_dt[, width := NULL]
            sumstats_dt[, strand := NULL]
            sumstats_dt[, end := NULL]
            sumstats_dt[, seqnames := NULL]
            data.table::setnames(sumstats_dt, "start", start_col)
        } 
    }
    return(sumstats_dt)
}
