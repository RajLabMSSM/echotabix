get_vcf_suffixes <- function(){
    vcf_suffixes <- c(".vcf.gz",".vcf.bgz", ".vcf",
                      ## Add some weird suffixes 
                      ## used by some consortia (e.g. PGC)
                      paste0(".vcf",c(".tsv",".csv",".txt"),".gz"))
    vcf_suffixes <- c(vcf_suffixes, toupper(vcf_suffixes))
    return(vcf_suffixes)
}