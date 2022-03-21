report_vcf <- function(vcf,
                       verbose=TRUE){
    n_snps <- nrow(vcf)
    n_samples <- ncol(vcf)
    messager("Retrieved data with",
             formatC(n_snps, big.mark = ","), "rows",
             "across", formatC(n_samples, big.mark = ","), "samples.",
             v = verbose
    ) 
}