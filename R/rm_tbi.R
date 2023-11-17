#' Remove tbi
#' 
#' Remove local copies of tabix index file (\emph{.tbi}) 
#' after completing queries.
#' @inheritParams base::list.files
#' @returns None
#' 
#' @keywords internal
rm_tbi <- function(path=".",
                   pattern = "\\.tbi$"){
    f <- list.files(path = path,
                    pattern = pattern,
                    full.names = TRUE)
    if(length(f)>0) tmp <- file.remove(f)
}