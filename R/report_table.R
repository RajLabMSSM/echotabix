report_tabular <- function(dat,
                           verbose=TRUE){
    n_snps <- nrow(dat) 
    messager("Retrieved data with",
             formatC(n_snps, big.mark = ","), "rows", 
             v = verbose
    ) 
}