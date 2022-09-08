#' Variant Call Format (VCF) --> data.table
#' 
#' Function to convert a \link[VariantAnnotation]{VCF}
#'  object to a \link[data.table]{data.table}. 
#' @inheritParams MungeSumstats::vcf2df
#' @inheritParams echodata::mungesumstats_to_echolocatoR
#' @returns data.frame version of VCF 
#' 
#' @export
#' @importFrom utils getFromNamespace 
#' @importFrom data.table setnames
#' @importFrom echodata mungesumstats_to_echolocatoR
#' @importFrom dplyr distinct across all_of
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
    df <- MungeSumstats::vcf2df(vcf = vcf,
                                add_sample_names = add_sample_names,
                                add_rowranges = add_rowranges,
                                ## Cols not unlisting properly atm, 
                                ## so this causes errors.
                                # drop_empty_cols = FALSE,
                                unique_rows = FALSE,
                                verbose = verbose)
    ## Find any remaining list cols, 
    ## and exclude them from finding unique rows.
    list_cols <- lapply(df, methods::is,"list")
    list_cols <- names(list_cols)[unlist(list_cols)] 
    df <- dplyr::distinct(.data = df,
                          dplyr::across(.cols = -dplyr::all_of(list_cols)))
    #### Standardise columns ####
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
