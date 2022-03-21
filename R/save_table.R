save_tabular <- function(query_res,
                         save_path, 
                         nThread=1,
                         verbose=TRUE){
    #### Save subset #### 
    messager("echotabix:: Saving query ==>", save_path, v = verbose)
    dir.create(dirname(save_path),
               showWarnings = FALSE, recursive = FALSE
    )
    data.table::fwrite(query_res,
                       file = save_path,
                       nThread = nThread,
                       sep = "\t"
    ) 
}
