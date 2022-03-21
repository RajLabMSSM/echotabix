#' Report time at end of function
#'
#' @keywords internal
#' @importFrom utils capture.output
report_time <- function(start,
                        v = TRUE, 
                        return_time = FALSE) {
    elapsed <- difftime(Sys.time(), start)
    messager(
        utils::capture.output(round(elapsed, 1)),
        v = v
    )
    if(return_time) return(elapsed)
}
