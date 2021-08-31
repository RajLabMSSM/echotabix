#' Query a tabix file
#'
#' Query by genomic coordinates.
#'
#' @family query functions
#' @examples
#' \dontrun{
#' data("BST1")
#' fullSS_path <- echolocatoR::example_fullSS()
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
