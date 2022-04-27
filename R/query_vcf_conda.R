#' Query VCF: \pkg{conda}
#' 
#' Query a subset of a VCF file (remote or local) 
#' using \code{tabix} via \pkg{echoconda}.
#' \strong{Advantages:}
#' \itemize{
#' \item{Fast.} 
#' \item{Not dependent on any R packages}.
#' }
#' 
#' @returns \link[VariantAnnotation]{CollapsedVCF} object.
#' 
#' @inheritParams construct_query
#' @inheritParams query_vcf
#' @inheritParams VariantAnnotation::readVcf
#' @inheritParams echoconda::find_packages
#'
#' @keywords internal
#' @importFrom VariantAnnotation ScanVcfParam readVcf writeVcf 
#' @importFrom echoconda find_packages yaml_to_env
query_vcf_conda <- function(## Target args 
                            target_path, 
                            ## Query args 
                            query_granges,
                            samples = character(),
                            ## Extra args
                            query_save = FALSE,
                            save_path = NULL,
                            conda_env = "echoR_mini",
                            verbose = TRUE) {
    
    messager("Querying VCF file using: conda", v = verbose)  
    conda_env <- echoconda::yaml_to_env(yaml_path = conda_env,
                                        verbose = verbose)
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat = query_granges, 
                                     verbose = FALSE)
    #### Ensure chromosome format is correct #### 
    query_granges <- fix_query_style(target_path = target_path,
                                     query_granges = query_granges,
                                     verbose = verbose) 
    #### Convert query from string ####
    query_str <- granges_to_string(gr = query_granges,
                                   verbose = verbose)
    #### Query ####
    {
        messager("Retrieving data.",v=verbose)
        start_query <- Sys.time() 
        tabix <- echoconda::find_packages(packages = "tabix",
                                         conda_env = conda_env,
                                         return_path = TRUE, 
                                         verbose = verbose)
        tmp <- tempfile(fileext = "subset.vcf")
        cmd <- paste(tabix[[1]],
                     "-h",
                     target_path,
                     query_str,
                     ">",tmp)
        echoconda::cmd_print(cmd)
        system(cmd)
        #### Read in subset #####
        param <- VariantAnnotation::ScanVcfParam(samples = samples) 
        vcf <- VariantAnnotation::readVcf(file = tmp, 
                                          param=param)
        report_time(start = start_query, v = verbose)
    } 
    #### Return ####
    return(vcf)
}

