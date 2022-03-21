filter_vcf_snps <- function(vcf,
                            query_granges,
                            verbose=TRUE){
    
    if(is.na(query_granges$SNP)){
        messager("Cannot find SNP information in query_granges object.",
                 "Please provide a query_granges object constructed with",
                 "query_dat and query_snp_col.",
                 "Skipping this filtering step.",v=verbose)
        return(vcf)
    } 
    #### Extract SNPs stored in query_granges ####
    query_snps <- unique(
        unlist(
            lapply(
                seq_len(length(query_granges)), function(i){
                    strsplit(query_granges[i,]$SNP, ";")[[1]]
                })
        )
    ) 
    #### Actually filter ####
    common_snps <- intersect(query_snps, rownames(vcf))
    removed_snps <-  unique(
        rownames(vcf)[ rownames(vcf) %in% common_snps]
    )
    messager("Removing",formatC(length(removed_snps), big.mark = ","),
             "/",formatC(nrow(vcf),big.mark = ","),
             "non-overlapping SNPs.",v=verbose)
    vcf <- vcf[common_snps, ] 
    return(vcf)
}
