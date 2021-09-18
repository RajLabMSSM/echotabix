#' Query a tabix file
#'
#' Query by genomic coordinates.
#'
#' @param fullSS_tabix Path to tabix file.
#' @param chrom Chromosome to query.
#' @param start_pos Minimum genomic position to query.
#' @param end_pos Maximum genomic position to query.
#' @param verbose Print messages.
#' 
#' @return \code{data.table} with the queried subset of genomic data.
#'
#' @keywords internal
#' @family query functions
#' @examples
#' \dontrun{
#' BST1 <- echodata::BST1
#' fullSS_path <- echodata::example_fullSS()
#' fullSS_tabix <- convert(fullSS_path = fullSS_path, position_col = "BP")
#' tab <- query_tabular(
#'     fullSS_tabix = fullSS_tabix,
#'     chrom = BST1$CHR[1],
#'     start_pos = min(BST1$POS),
#'     end_pos = max(BST1$POS)
#' )
#' }
query_tabular <- function(fullSS_tabix,
                          chrom,
                          start_pos,
                          end_pos,
                          verbose = TRUE) {
    coords <- paste0(chrom, ":", start_pos, "-", end_pos)
    messager("echotabix:: Extracting subset of sum stats", v = verbose)
    dat <- seqminer::tabix.read.table(
        tabixFile = fullSS_tabix,
        tabixRange = coords
    )
    messager("echotabix:: Returning", paste(dim(dat), collapse = " x "),
        "data.table",
        v = verbose
    )
    return(dat)
}
