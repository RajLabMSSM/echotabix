report_tabular <- function(query_res,
                           verbose=TRUE){
    n_snps <- nrow(query_res) 
    messager("Retrieved data with",
             formatC(n_snps, big.mark = ","), "rows", 
             v = verbose
    ) 
}