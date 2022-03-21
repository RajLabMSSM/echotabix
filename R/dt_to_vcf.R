#' \link[data.table]{data.table} to VCF
#' 
#' Convert a \link[data.table]{data.table} to a VCF file
#' Used to be performed with 
#' \href{https://github.com/RajLabMSSM/echolocatoR/blob/0ccf40d2f126f755074e731f82386e4e01d6f6bb/R/dataframe_2_vcf.R}{
#' \code{bcftools convert}, but \code{MungeSumstats} works much better}.
#' 
#' @param ... Additional arguments passed to 
#' \link[MungeSumstats]{standardise_header}.
#' @inheritParams MungeSumstats::write_sumstats
#' 
#' @export
#' @importFrom echoconda find_packages
#' @examples
#' save_path <- echotabix::dt_to_vcf(dat=echodata::BST1)
dt_to_vcf <- function(dat,
                      save_path=tempfile(fileext = "_converted.vcf"),
                      tabix_index=FALSE,
                      nThread=1,
                      ...){
  
  requireNamespace("MungeSumstats")
  dat <- MungeSumstats::standardise_header(sumstats_dt = dat,
                                           uppercase_unmapped = FALSE, 
                                           return_list = FALSE, 
                                           ...)
  save_path <- MungeSumstats::write_sumstats(sumstats_dt = dat,
                                             save_path = save_path, 
                                             return_path = TRUE, 
                                             tabix_index = tabix_index, 
                                             write_vcf = TRUE, 
                                             nThread = nThread)
  return(save_path)
}
