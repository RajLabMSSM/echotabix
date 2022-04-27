#' Run gunzip: R.utils
#' 
#' Support function for \link[echotabix]{run_gunzip}.
#' 
#' @param path Path to file. 
#' @inheritParams sort_coordinates
#' @keywords internal
#' @importFrom R.utils gunzip
#' @importFrom data.table fread
run_gunzip_rutils <- function(path, 
                              outputs = c("command","path","data"),
                              overwrite=TRUE,
                              remove=FALSE,
                              verbose = TRUE){
    
   
    path <- R.utils::gunzip(path, 
                            overwrite=overwrite,
                            remove=remove) 
    dat <- NULL;
    if("data" %in% outputs){
        dat <- data.table::fread(path, nThread = 1)
    }
    out <- construct_outputs(outputs = outputs, 
                             command = "R.utils::gunzip", 
                             path = path,
                             data = dat) 
    return(out)
}
