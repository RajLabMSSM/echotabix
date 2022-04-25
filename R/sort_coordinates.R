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
#' @inheritParams construct_query
#' @inheritParams convert_and_query
#' 
#' @source \href{https://www.systutorials.com/docs/linux/man/1-tabix/}{
#' sorting instructions}
#' @source \href{https://unix.stackexchange.com/a/382801}{
#' StackExchange discussion of how \code{sort} handles numbers}
#' 
#' @inheritParams convert
#' @export 
#' @importFrom echoconda cmd_print
#' @examples 
#' dat <- echodata::BST1
#' tmp <- tempfile()
#' data.table::fwrite(dat, tmp)
#' out <- echotabix::sort_coordinates(target_path=tmp, 
#'                                    chrom_col = "CHR", 
#'                                    start_col = "POS")
sort_coordinates <- function(target_path,
                             chrom_col,
                             start_col,
                             end_col=start_col,
                             comment_char=NULL,
                             save_path=NULL,
                             method = c("bash","data.table"),
                             outputs=c("command","path","data"),
                             verbose=TRUE){
     
    #### Check outputs arg ####
    outputs <- check_outputs(outputs = outputs, 
                             func = sort_coordinates)
    #### save_path must be decompressed ####
    if(!is.null(save_path)) save_path <- gsub(".gz|.bgz|.zip","",save_path)
    #### Infer comment_char arg from header ####
    comment_char <- infer_comment_char(target_path = target_path, 
                                       comment_char = comment_char,
                                       verbose = verbose)
    #### Check which methods can be used given the data ####
    method <- sort_coordinates_check_method(method = method, 
                                            target_path = target_path, 
                                            chrom_col = chrom_col, 
                                            verbose = verbose)
    #### Sort (or create sort command) ####
    if(method=="data.table"){ 
        #### sort with data.table ####
        out <- sort_coordinates_datatable(target_path=target_path,
                                          chrom_col=chrom_col,
                                          start_col=start_col,
                                          end_col=end_col,
                                          save_path=save_path,
                                          outputs=outputs,
                                          skip=comment_char,
                                          verbose=verbose) 
    } else{
        #### sort with bash ####
        out <- sort_coordinates_bash(target_path=target_path, 
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
