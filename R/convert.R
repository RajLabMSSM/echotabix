#' Convert summary stats file to tabix format
#'
#' @family tabix
#' @examples
#' \dontrun{
#' fullSS_path <- echolocatoR::example_fullSS()
#' fullSS_tabix <- convert(fullSS_path = fullSS_path, position_col = "BP")
#' }
#' @export
#' @importFrom Rsamtools bgzip
#' @importFrom seqminer tabix.createIndex
convert <- function(fullSS_path,
                    chrom_col = "CHR",
                    position_col = "POS",
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
        startColumn = cDict[[position_col]],
        endColumn = cDict[[position_col]],
        ## Just use the first columns name
        metaChar = names(cDict)[1]
    )
    return(bgz_file)
}
