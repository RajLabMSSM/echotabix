filter_vcf_snps <- function(vcf,
                            query_dat,
                            query_snp_col,
                            verbose=TRUE){
    # rowranges <- vcf@rowRanges
    if(is.null(query_snp_col) || (!query_snp_col %in% colnames(query_dat))){
        messager("A valid query_snp_col that is present in query_dat",
                 "must be provided in order to run overlapping_only.",
                 "Skipping this step.")
    } else {
        common_snps <- intersect(query_dat$SNP, rownames(vcf))
        removed_snps <-  unique(
            rownames(vcf)[ rownames(vcf) %in% common_snps]
        )
        messager("Removing",formatC(length(removed_snps), big.mark = ","),
                 "/",formatC(nrow(vcf),big.mark = ","),
                 "non-overlapping SNPs.",v=verbose)
        vcf <- vcf[common_snps, ]
    } 
    return(vcf)
}