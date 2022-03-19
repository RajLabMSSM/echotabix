#' Sort coordinates: data.table
#' 
#' Support function for \link[echotabix]{sort_coordinates}.
#'  
#' @inheritParams sort_coordinates
#' @inheritParams convert
#' @keywords internal  
sort_coordinates_datatable <- function(fullSS_path,
                                       chrom_col,
                                       start_col,
                                       end_col=start_col,
                                       outputs=c("command","path","data"),
                                       save_path=NULL,
                                       conda_env="echoR",
                                       verbose=TRUE){
    
    messager("Sorting rows by coordinates via data.table.",v=verbose)
    #### Check outputs arg ####
    outputs <- check_outputs(outputs = outputs, 
                             func = sort_coordinates_datatable)
    #### Make save_path ####
    ## Always make it .tsv 
    rm_strings <- ".tsv|.csv|.txt|.gz|.bgz"
    if(is.null(save_path)) { 
        save_path <- file.path(tempdir(), 
                               paste0(
                                   gsub(rm_strings,"",fullSS_path,
                                        ignore.case = TRUE),
                                   "_sorted.tsv"
                                   )
                               )
        messager("WARNING: save_path is required",
                 "for sort_coordinates_datatable().",
                 "Setting save_path to:",save_path,
                 v=verbose) 
    } else {
        save_path <- paste0(
            gsub(rm_strings,"",save_path, 
                 ignore.case = TRUE),
            "_sorted.tsv"
            ) 
    }  
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    #### Read ####
    dat <- data.table::fread(fullSS_path, nThread = 1)
    #### Sort ####
    data.table::setnames(dat, chrom_col, "CHR")
    dat[,CHR:=as.integer(gsub("chr","",CHR, ignore.case = TRUE))]
    data.table::setkeyv(dat, c("CHR", start_col, end_col))
    data.table::setkey(dat,NULL)
    data.table::setnames(dat, "CHR",chrom_col) 
    #### Save ####
    data.table::fwrite(dat, save_path, nThread = 1, sep="\t") 
    #### Return ####
    out <- construct_outputs(outputs = outputs, 
                             command = paste("(",save_path,")"), 
                             path = save_path,
                             data = dat)
    return(out)
}
