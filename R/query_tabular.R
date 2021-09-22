#' Query a tabix file
#'
#' Query by genomic coordinates.
#'
#' @param fullSS_tabix Path to tabix file.
#' @param chrom Chromosome to query.
#' @param start_pos Minimum genomic position to query.
#' @param end_pos Maximum genomic position to query.
#' @param local Whether \code{fullSS_tabix} is stored locally or 
#' on a remote server/website.
#' By default (\code{"infer"}) will infer local status and 
#' use the appropriate method.  
#' @param verbose Print messages.
#'
#' @return \code{data.table} with the queried subset of genomic data.
#'
#' @family query functions
#' @examples
#' \dontrun{
#' BST1 <- echodata::BST1
#'
#' #### local ####
#' fullSS_path <- echodata::example_fullSS()
#' fullSS_tabix <- convert(fullSS_path = fullSS_path, start_col = "BP")
#' tab <- query_tabular(
#'     fullSS_tabix = fullSS_tabix,
#'     chrom = BST1$CHR[1],
#'     start_pos = min(BST1$POS),
#'     end_pos = max(BST1$POS)
#' )
#'
#' #### remote ####
#' fullSS_tabix <- file.path(
#'     "https://egg2.wustl.edu/roadmap/data/byFileType",
#'     "chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final",
#'     "E099_15_coreMarks_dense.bed.bgz"
#' )
#' tab <- query_tabular(
#'     fullSS_tabix = fullSS_tabix,
#'     chrom = BST1$CHR[1],
#'     start_pos = min(BST1$POS),
#'     end_pos = max(BST1$POS)
#' )
#' }
#' @export
#' @importFrom seqminer tabix.read.table
#' @importFrom data.table fread
query_tabular <- function(fullSS_tabix,
                          chrom,
                          start_pos,
                          end_pos,
                          local = "infer",
                          verbose = TRUE) {
    if (local == "infer") {local <- is_local(fullSS_tabix)}
    if (!local) {
        messager("Querying remote tabular tabix file.",v=verbose)
        #### Remote tabular tabix file ####
        # Rsamtools is slower but works for remote files
        dat <- query_tabular_rsamtools(fullSS_tabix = fullSS_tabix,
                                       chrom = chrom, 
                                       start_pos = start_pos, 
                                       end_pos = end_pos,
                                       verbose = verbose)
    } else {
        messager("Querying local tabular tabix file.",v=verbose)
        #### Local tabular tabix file ####
        dat <- query_tabular_seqminer(fullSS_tabix = fullSS_tabix,
                                      chrom = chrom, 
                                      start_pos = start_pos, 
                                      end_pos = end_pos,
                                      verbose = verbose)
    }
    messager("echotabix:: Returning",
             paste(formatC(dim(dat), big.mark = ","), collapse = " x "),
        "data.table",
        v = verbose
    )
    return(dat)
}
