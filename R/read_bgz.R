#' Read bgz
#' 
#' Read a compressed bgz file into R.
#' 
#' NOTE: Packages like \pkg{Rsamtools} and \pkg{seqminer} aren't great for 
#' this because they require that the bgz file it also indexed already.
#' 
#' @param ... Additional arguments passed to 
#' \link[data.table]{fread} or \link[utils]{read.delim2}.
#' @inheritParams data.table::fread
#' 
#' @family tabix functions
#' @export
#' @examples 
#' tmp <- tempfile(fileext = ".tsv.gz")
#' data.table::fwrite(echodata::BST1, file = tmp, sep = "\t")
#' path <- Rsamtools::bgzip(file = tmp, overwrite=TRUE)
#' dat <- echotabix::read_bgz(path=path, method="utils")
read_bgz <- function(path, 
                     method = c("data.table","utils"),
                     nrows = NULL,
                     header = TRUE, 
                     verbose = TRUE,
                     ...){   
    
    method <- tolower(method)[1]
    #### Adjust nrows for whether there's a header or not ####
    nrows <- if(is.null(nrows)) {
        -1L
    } else {
        if(header) nrows + 1L else nrows
    }
    if(!echodata::is_local(path) & method=="utils"){
        messager("WARNING: method='utils' does not work for URLs.",
                 "Switching to method='data.table'.",v=verbose)
        method <- "data.table"
    }
    #### Read bgz ####
    if(method=="data.table"){
        dat <- read_bgz_datatable(path=path,
                                  nrows=nrows,
                                  header=header, 
                                  verbose=verbose,
                                  ...) 
    } else { 
        dat <- read_bgz_utils(path=path,
                              nrows=nrows,
                              header=header,
                              verbose=verbose, 
                              ...)
    } 
    return(dat)
}
