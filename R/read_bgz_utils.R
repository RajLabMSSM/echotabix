read_bgz_utils <- function(path,
                           nrows=-1,
                           header=TRUE,
                           verbose=TRUE,
                           ...){
    messager("Reading bgzipped file using: utils",v=verbose)  
    zz <- gzfile(path,'rt')   
    if(is.infinite(nrows)) nrows <- -1
    dat <- utils::read.delim2(file = zz, 
                              header=header, 
                              nrows = nrows,
                              ...)
    ## read.delim2 doesn't do a great job of inferring column types
    ## on its own. Use this to infer the correct column types.  
    dat <- data.table::data.table(dat)
    dat <- echodata::fix_coltypes(dat = dat)
    return(dat)
}