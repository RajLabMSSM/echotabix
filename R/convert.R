#' Convert summary stats file to tabix format
#'
#'
#' @param fullSS_path Path to full GWAS/QTL summary statistics file.
#' @param verbose Print messages.
#' @inheritParams dt_to_granges
#'
#' @family tabix
#' @examples
#' \dontrun{
#' fullSS_path <- echodata::example_fullSS()
#' fullSS_tabix <- convert(fullSS_path = fullSS_path, position_col = "BP")
#' }
#' @export
#' @importFrom Rsamtools bgzip
#' @importFrom seqminer tabix.createIndex
convert <- function(fullSS_path,
                    chrom_col = "CHR",
                    start_col = "POS",
                    end_col = start_col,
                    verbose = TRUE) {
    messager("echotabix:: Converting full summary stats file to",
        "tabix format for fast querying...",
        v = verbose
    )
    cDict <- column_dictionary(file_path = fullSS_path)
    # Make sure input file isn't empty
    if (file.size(fullSS_path) == 0) {
        messager("echotabix:: Removing empty file =", fullSS_path)
        file.remove(fullSS_path)
    }
    ### File MUST be bgzipped first
    messager("echotabix:: Ensuring file is bgzipped.", v = verbose)
    bgz_file <- Rsamtools::bgzip(file = fullSS_path, overwrite = TRUE)

    ### Tabix-index file
    messager("echotabix:: Tabix-indexing file.")
    seqminer::tabix.createIndex(
        bgzipFile = bgz_file,
        sequenceColumn = cDict[[chrom_col]],
        startColumn = cDict[[start_col]],
        endColumn = cDict[[end_col]],
        ## Just use the first columns name
        metaChar = names(cDict)[1]
    )
    return(bgz_file)
}
