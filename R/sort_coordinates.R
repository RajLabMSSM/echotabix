#' Sort coordinates
#' 
#' Sort a table of summary statistics by their genomic coordinates 
#' without reading the file into R.
#' @param return_cmd Return a named list containing 
#' one or more of the following:
#' \itemize{
#' \item{"command"}{Text string of the command (without executing it.)}
#' \item{"path"}{Path to the saved data file.}
#' \item{"data"}{The resulting data in \link[data.table]{data.table} format.}
#' }  
#' @param save_path File to save the results to.
#' 
#' @source \href{https://www.systutorials.com/docs/linux/man/1-tabix/}{
#' sorting instructions}
#' @source \href{https://unix.stackexchange.com/a/382801}{
#' StackExchange discussion of how \code{sort} handles numbers}
#' 
#' @inheritParams convert
#' @export 
#' @importFrom echoconda cmd_print
#' @importFrom echodata determine_chrom_type
#' @examples 
#' dat <- echodata::BST1
#' tmp <- tempfile()
#' data.table::fwrite(dat, tmp)
#' out <- echotabix::sort_coordinates(fullSS_path=tmp, 
#'                                    chrom_col = "CHR", 
#'                                    start_col = "POS")
sort_coordinates <- function(fullSS_path,
                             chrom_col,
                             start_col,
                             end_col=start_col,
                             comment_char=NULL,
                             save_path=NULL,
                             outputs=c("command","path","data"),
                             verbose=TRUE){
    
    #### Check outputs arg ####
    outputs <- check_outputs(outputs = outputs, 
                             func = sort_coordinates)
    #### save_path must be decompressed ####
    if(!is.null(save_path)) save_path <- gsub(".gz|.bgz|.zip","",save_path)
    #### Infer comment_char arg from header ####
    comment_char <- infer_comment_char(fullSS_path = fullSS_path, 
                                       comment_char = comment_char,
                                       verbose = verbose)
    #### Check for "chr" prefix ####
    has_chr <- echodata::determine_chrom_type(file_path = fullSS_path, 
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
    is_tab <- is_tab_delimited(path = fullSS_path)
    if(!is_tab){
        messager("WARNING: Columns must be tab-separated ('\\t')",
                 "in order to be sorted outside of R",
                 "(which is more memory-efficient).",
                 "\nWill instead import full data into R",
                 "to sort and rewrite to disk.",
                 v=verbose)
    }
    #### Sort (or create sort command) ####
    if((has_chr) | (!is_tab)){ 
        #### sort with data.table ####
        out <- sort_coordinates_datatable(fullSS_path=fullSS_path,
                                          chrom_col=chrom_col,
                                          start_col=start_col,
                                          end_col=end_col,
                                          save_path=save_path,
                                          outputs=outputs,
                                          verbose=verbose) 
    } else{
        #### sort with bash ####
        out <- sort_coordinates_bash(fullSS_path=fullSS_path, 
                                     chrom_col=chrom_col,
                                     start_col=start_col,
                                     end_col=end_col,
                                     comment_char=comment_char,
                                     save_path=save_path,
                                     outputs=outputs,
                                     verbose=verbose)
    } 
    return(out)
}
