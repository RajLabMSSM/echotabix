#' Convert and query
#'
#' If it is not tabix format already
#' (determined by checking for a \code{.tbi}
#' file of the same name in the same directory),
#' the full summary statistics file is converted into tabix format
#' for super fast querying.
#' A query is then made using the min/max genomic positions to extract a
#'  locus-specific summary stats file.
#'
#' @param fullSS_path Path to the full summary statistics file (GWAS or QTL).
#' It is usually best to provide the absolute path
#'  rather than the relative path.
#' @param study_dir Path to study folder.
#' @param subset_path Path to save queried data subset as. 
#' @param min_POS Minimum genomic position to query.
#' @param max_POS Maximum genomic position to query.
#' @param chrom Chromosome to query (e.g. "chr12" or "12").
#' @param save_subset Whether to save the queried data subset.
#' @param nThread Number of threads to use.
#' @param verbose Print messages.
#' @inheritParams dt_to_granges
#'
#' @family tabix
#' @return \code{data.table} of locus subset summary statistics
#' @examples
#' \dontrun{
#' BST1 <- echodata::BST1
#' fullSS_path <- echodata::example_fullSS()
#' subset_path <- file.path(tempdir(), "BST1_Nalls23andMe_2019_subset.tsv.gz")
#' dat <- convert_and_query(
#'     fullSS_path = fullSS_path,
#'     subset_path = subset_path,
#'     min_POS = min(BST1$POS),
#'     max_POS = max(BST1$POS),
#'     chrom = BST1$CHR[1]
#' )
#' }
#' @export
#' @importFrom data.table fwrite
convert_and_query <- TABIX <- function(fullSS_path,
                                       study_dir = NULL,
                                       subset_path = tempfile(".tsv.gz"), 
                                       chrom_col = "CHR",
                                       start_col = "BP",
                                       end_col = start_col,
                                       min_POS,
                                       max_POS,
                                       chrom,
                                       save_subset = TRUE,
                                       nThread = 1, 
                                       verbose = TRUE) {
    #### Check if it's already an indexed tabix file ####
    tabix_out <- construct_tabix_path(
        fullSS_path = fullSS_path,
        study_dir = study_dir
    )
    if (infer_if_tabix(tabix_out)) {
        # Checks if the file (in the study dir) already exists,
        # and whether it is a tabix-indexed file.
        messager("echotabix:: Using existing tabix file:",
            tabix_out,
            v = verbose
        )
        # Jump ahead and query tabix_out file
    } else {
        if (infer_if_tabix(fullSS_path)) {
            messager("echotabix:: Copying existing tabix file ==>",
                fullSS_path,
                v = verbose
            )
            file.copy(fullSS_path, tabix_out, overwrite = TRUE)
            tabix_out <- fullSS_path
        } else {
            tabix_out <- convert(
                fullSS_path = fullSS_path,
                chrom_col = chrom_col,
                start_col = start_col,
                end_col = end_col,
                verbose = verbose
            )
        }
    }
    #### Check chrom format ####
    cDict <- column_dictionary(file_path = tabix_out)
    has_chr <- determine_chrom_type(
        file_path = tabix_out,
        chrom_col = chrom_col,
        verbose = verbose
    )
    chrom <- if (has_chr) {
        paste0("chr", gsub("chr", "", chrom))
    } else {
        gsub("chr", "", chrom)
    }
    #### Query ####
    dat <- query_tabular(
        fullSS_tabix = tabix_out,
        chrom = chrom,
        start_pos = min_POS,
        end_pos = max_POS,
        verbose = verbose
    )
    #### Save subset ####
    if (save_subset) {
        messager("echotabix:: Saving query ==>", subset_path, v = verbose)
        dir.create(dirname(subset_path),
            showWarnings = FALSE, recursive = FALSE
        )
        data.table::fwrite(dat,
            file = subset_path,
            nThread = nThread,
            sep = "\t"
        )
    }
    return(dat)
}
