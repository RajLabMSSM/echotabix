save_tabular <- function(dat,
                         save_path, 
                         nThread=1,
                         verbose=TRUE){
    #### Save subset #### 
    messager("echotabix:: Saving query ==>", save_path, v = verbose)
    dir.create(dirname(save_path),
               showWarnings = FALSE, recursive = FALSE
    )
    data.table::fwrite(dat,
                       file = save_path,
                       nThread = nThread,
                       sep = "\t"
    ) 
}
