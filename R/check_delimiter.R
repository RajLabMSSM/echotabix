#' Check delimiter
#' 
#' Automatically infer what delimiter is used to separate columns 
#' (e.g. ",", "\\t"," "). Uses messages from \link[data.table]{fread} to 
#' extract this inference.
#' @param path Path to file.
#' @param verbose Print messages.
#' @export
#' @importFrom data.table fread
#' @importFrom utils capture.output
#' @examples 
#' dat <- echodata::BST1
#' path <- tempfile()
#' data.table::fwrite(dat, path, sep="\t")
#' delim <- echotabix::check_delimiter(path=path)
check_delimiter <- function(path,
                            verbose=TRUE){
    messager("Detecting column delimiter.",v=verbose)
    #### Check file exists ####
    if(!file.exists(path)) stop("File does not exist.")
    #### Get verbose fread output ####
    out <- utils::capture.output(
        data.table::fread(path, 
                          nrows = 2, 
                          verbose = TRUE)
    )
    #### Get detected separator ####
    delim <- gsub("sep=|'","", 
                  grep("sep=", 
                      trimws(
                          strsplit( 
                              grep("sep='",out, value = TRUE),
                              " ")[[1]]
                          ),
                      value = TRUE
                      ) 
    )  
    messager("Identified column separator:",delim)
    ### Get rid of extra backslash ####
    if(delim=="\\t") delim <- "\t"
    return(delim)
}
