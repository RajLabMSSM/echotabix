infer_tabix_format <- function(format=NULL,
                               path,
                               verbose=TRUE){
    
    #### Check user-input argument first #### 
    if(!is.null(format)){
        #### Options ####
        vcf_opts <- c("vcf","variantcallformat","v")
        table_opts <- c("table","tabular","t")
        format <- tolower(format)[1]
        #### Check vcf ####
        vcf_hits <- grepl(format, vcf_opts, ignore.case = TRUE)
        if(sum(vcf_hits)>0) {
            messager("Explicit format: 'vcf'",v=verbose)
            return("vcf")
        }
        #### Check table ####
        table_hits <- grepl(format, table_opts, ignore.case = TRUE)
        if(sum(table_hits)>0) {
            messager("Explicit format: 'table'",v=verbose)
            return("table")
        }
    } 
    #### If no hits, try inferring from path ####
    if(grepl(".vcf",path, ignore.case = TRUE)){
        messager("Inferred format: 'vcf'",v=verbose)
        return("vcf")
    } else {
        messager("Inferred format: 'table'",v=verbose)
        return("table")
    } 
}
