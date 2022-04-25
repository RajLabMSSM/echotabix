infer_comment_char <- function(target_path,
                               comment_char=NULL,
                               format=NULL,
                               verbose=TRUE){
    if(is.null(comment_char)){
        format <- infer_tabix_format(path = target_path,
                                     format = format,
                                     verbose = verbose)
        #### VCF ####
        if(format=="vcf"){
            messager("Inferring comment_char from VCF format:",
                     shQuote(comment_char),
                     v=verbose)
            comment_char <- "#CHROM"
            #### Tabular ####
        } else {
            #### Infer comment_char arg from header ####
            if(is.null(comment_char)) { 
                comment_char <- echodata::get_header(path = target_path)[1]
                messager("Inferring comment_char from tabular header:",
                         shQuote(comment_char),
                         v=verbose)
            }
        } 
    } 
    return(comment_char)
}
