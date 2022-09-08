fix_query_style <- function(target_path,
                            target_index,
                            query_granges,
                            return_header=FALSE,
                            verbose=TRUE){
    ## This is essential for files stored on remote servers where you can't
    ## edit the file itself. Thus, you have to edit the query.
    messager("Checking query chromosome style is correct.",v=verbose)
    # if(is_vcf(target_path)){
    #   #### Alternative method for VCFs
    #     vcf_file <- VariantAnnotation::VcfFile(
    #         file = target_path, 
    #         index = paste0(target_path,".tbi"),
    #         yieldSize = 10L)
    #     header <- VariantAnnotation::scanVcf(file = vcf_file) 
    #     VariantAnnotation::seqlevels(header) 
    # }else {
        ## Seems to work for both tabular and VCF files 
        ## despite the error message
        tbx <- Rsamtools::TabixFile(file = target_path, 
                                    index = target_index)
        header <- Rsamtools::headerTabix(file = tbx) 
    # } 
    if(length(header$seqnames)==0) stop("header$seqnames is empty.")
    has_chr <- infer_chrom_type(chrom = header$seqnames)
    if(has_chr){
        query_granges <- granges_style(gr = query_granges, 
                                       style = "UCSC")
    }
    if(return_header){
        return(list(query_granges=query_granges,
                    header=header))
    } else {
        return(query_granges)
    } 
}