read_bgz_datatable <- function(path,
                               nrows = -1,
                               header = TRUE,
                               verbose = TRUE,
                               ...){
    messager("Reading bgzipped file using: data.table",v=verbose) 
    #### Remote bgz #####
    ## Remote bgz files are a little trickier ####
    ## You have to download them first.
    if(!echodata::is_local(path)){
        tmp <- file.path(tempdir(), basename(path))
        utils::download.file(url = path, 
                             destfile = tmp) 
        path <- tmp
    }  
    #### Read in file ####
    dat <- data.table::fread(text= readLines(con = path,
                                             n = nrows),
                             check.names = FALSE,
                             header = header,
                             ...)
    return(dat)
}
