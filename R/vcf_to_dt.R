#' Variant Call Format (VCF) --> data.table
#' 
#' Function to convert a \link[VariantAnnotation]{VCF}
#'  object to a \link[data.table]{data.table}. 
#' @inheritParams MungeSumstats::vcf2df
#' @inheritParams echodata::mungesumstats_to_echolocatoR
#' @return data.frame version of VCF 
#' 
#' @export
#' @importFrom utils getFromNamespace 
#' @importFrom data.table setnames
#' @importFrom echodata mungesumstats_to_echolocatoR
#' @examples
#' vcf_file <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
#'                         package = "echodata")
#' vcf <- VariantAnnotation::readVcf(file = vcf_file)
#' vcf_dt <- echotabix::vcf_to_dt(vcf = vcf)
vcf_to_dt <- function(vcf, 
                      add_sample_names=TRUE,
                      add_rowranges=TRUE,
                      standardise_colnames=TRUE,
                      verbose=TRUE) {
    vcf2df <- utils::getFromNamespace("vcf2df","MungeSumstats") 
    df <- vcf2df(vcf = vcf,)
    
    if(standardise_colnames){
        df <- echodata::mungesumstats_to_echolocatoR(
            dat = df, 
            standardise_colnames = TRUE,
            verbose = verbose)
        if((!"POS" %in% names(df)) &&
           ("START" %in% names(df))) {
            data.table::setnames(df,"START","POS")
        }
    } 
    return(df)
}
