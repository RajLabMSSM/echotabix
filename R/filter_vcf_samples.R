#' Filter VCF query samples
#' 
#' Only include certain samples in a VCF query. 
#' If at least one sample is supplied, returns
#'  a \link[VariantAnnotation]{ScanVcfParam}.
#' Otherwise, returns the original \link[GenomicRanges]{GRanges} object
#'  (\code{gr}).
#'  
#' @param gr A \link[GenomicRanges]{GRanges} object 
#' generated by \link[echotabix]{construct_query}.
#' @inheritParams construct_query
#' 
#' @returns \link[GenomicRanges]{GRanges} or
#' \link[VariantAnnotation]{ScanVcfParam} object.
#' @keywords internal
#' @importFrom VariantAnnotation ScanVcfParam
filter_vcf_query_samples <- function(gr,
                                     samples=character(),
                                     verbose=TRUE){
    if (is.null(samples)) samples <- character() 
    if(length(samples)>0){
        samples <- unique(samples)
        messager("Filtering query to",
                 formatC(length(samples), big.mark = ","),
                 "samples and returning ScanVcfParam object.",
                 v=verbose)
        param <- VariantAnnotation::ScanVcfParam(samples = samples,
                                                 which = gr)  
        return(param)
    } else {
        return(gr)
    }
}
