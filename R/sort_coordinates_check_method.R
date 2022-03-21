sort_coordinates_check_method <- function(method,
                                          target_path, 
                                          chrom_col,
                                          verbose=TRUE){ 
    method <- tolower(method)[1]
    #### Check for "chr" prefix ####
    has_chr <- infer_chrom_type(path = target_path, 
                                chrom_col = chrom_col,
                                verbose = verbose)
    
    if(has_chr){
        messager("WARNING: Chromosomes must be in numeric format (e.g. 1)",
                 "and NOT in string format (e.g. 'chr1')",
                 "in order to be sorted outside of R",
                 "(which is more memory-efficient).",
                 "\nWill instead import full data into R",
                 "to sort and rewrite to disk.",
                 v=verbose) 
    }
    #### Check delimiter ####
    ## Delimiter must be \t in order sort bash method to work.
    is_tab <- is_tab_delimited(path = target_path)
    if(!is_tab){
        messager("WARNING: Columns must be tab-separated ('\\t')",
                 "in order to be sorted outside of R",
                 "(which is more memory-efficient).",
                 "\nWill instead import full data into R",
                 "to sort and rewrite to disk.",
                 v=verbose)
    }
    if((has_chr) | (!is_tab)) method <- "data.table"
    return(method)
}
