#' GRanges to string
#' 
#' Convert a \link[GenomicRanges]{GRanges} object 
#' to a concatenated string of coordinates 
#' (e.g. "chr4:70000-90000,chr10:200-150001"). 
#' This can be used for specifying
#' which regions you want to query (e.g. when using \code{tabix}).
#' 
#' @param gr \link[GenomicRanges]{GRanges} object. 
#' @param pos_sep Character to separate start/end genomic positions with.
#' @param chrom_sep Character to separate chromosome name 
#' from start/end positions.
#' @param ranges_sep Character to separate each genomic range with.
#' 
#' @param verbose Print messages.
#' 
#' @returns A concatenated string of coordinates.
#' 
#' @export
#' @importFrom GenomicRanges seqnames start end 
#' @examples  
#' gr1 <- echotabix::construct_query(query_dat = echodata::BST1)
#' gr2 <- echotabix::construct_query(query_dat = echodata::LRRK2)
#' gr <- suppressWarnings(c(gr1, gr2))
#' 
#' string <- echotabix::granges_to_string(gr=gr)
granges_to_string <- function(gr,
                              pos_sep = "-",
                              chrom_sep = ":",
                              ranges_sep = ",",
                              verbose=TRUE){
    messager("Converting GRanges query to a string of coordinates.",
             v=verbose)
    #### Can also supply a data.frame directly ####
    ## But only when formatted according to echolocatoR's standard colnames.
    if(methods::is(gr,"data.frame")){
        gr <- echodata::dt_to_granges(dat = gr)
    }
    #### Construct string ####
    gr <- unique(gr)
    coords <- paste(
        paste(as.character(GenomicRanges::seqnames(gr)),
              paste(
                  as.character(as.integer(GenomicRanges::start(gr))),
                  as.character(as.integer(GenomicRanges::end(gr))),
                  sep=pos_sep
              ),
              sep=chrom_sep
        ),
        collapse=ranges_sep
    )
    return(coords)
}
