#' Convert to tabix 
#' 
#' Convert a tabular file to compressed (bgzip), indexed, tabix format 
#' for rapid querying.
#'
#' @param fullSS_path Path to full GWAS/QTL summary statistics file.
#' @param method A named list containing methods to run each step with.
#' @param force_new Force the creation of a 
#' new bgzip file (\emph{.bgz}) and a new tabix index file (\emph{.tbi}).
#' @param verbose Print messages.
#' @inheritParams convert_and_query 
#' @inheritParams construct_query 
#'
#' @family tabix functions
#' @source \href{https://github.com/samtools/htslib/issues}{
#' samtools/tabix GitHub repo}
#' @source \href{https://github.com/Bioconductor/Rhtslib/issues/4}{
#' Rhtslib (which Rsamtools and seqminer depend on for tabix) 
#' is very out of date (1.7 vs. 1.15)}.
#' @source \href{https://github.com/bioconda/bioconda-recipes/issues/23404}{
#' \code{conda install tabix} was recently fixed so it installs htslib}
#' 
#' @export  
#' @examples
#' #### Example with full data ####
#' # tmp <- echodata::example_fullSS()
#' #### Example with single locus ####
#' dat <- echodata::BST1
#' tmp <- tempfile()
#' data.table::fwrite(dat, tmp, sep="\t")
#'  
#' tabix_files <- echotabix::convert(fullSS_path = tmp)
convert <- function(fullSS_path,
                    bgz_file = tabix_path(path = fullSS_path), 
                    chrom_col = "CHR",
                    start_col = "POS",
                    end_col = start_col,
                    comment_char = NULL,
                    method = list(sort_coordinates="bash", 
                                  run_bgzip="Rsamtools",
                                  index="Rsamtools"),
                    conda_env = "echoR",
                    force_new = TRUE,
                    verbose = TRUE) {
    messager("========= echotabix::convert =========", v=verbose)
    messager("Converting full summary stats file to",
             "tabix format for fast querying.",
             v = verbose 
    )  
    #### Make sure input file isn't empty ####
    if(!file.exists(fullSS_path)) {
        stop("Cannot find file specified by fullSS_path.")
    }
    if (file.size(fullSS_path) == 0) {
        messager("echotabix:: Removing empty file:", fullSS_path, v=verbose)
        try({file.remove(fullSS_path)})
    }
    #### infer comment char ####
    comment_char <- infer_comment_char(fullSS_path=fullSS_path, 
                                       comment_char=comment_char, 
                                       verbose=verbose)
    #### Sort file by genomic coordinates (required) #### 
    sort_out <- sort_coordinates(fullSS_path = fullSS_path,
                                 chrom_col = chrom_col,
                                 start_col = start_col, 
                                 end_col = end_col,
                                 comment_char = comment_char,
                                 method = method$sort_coordinates,
                                 save_path=tempfile(fileext = "_sorted.tsv"),
                                 outputs = "path",
                                 verbose = verbose)
    #### bgzip-compress file (required) ####
    bgz_file <- run_bgzip(fullSS_path = sort_out, 
                          bgz_file = bgz_file,
                          chrom_col = chrom_col,
                          start_col = start_col,
                          end_col = end_col,
                          comment_char = comment_char,
                          force_new = force_new,
                          method = method[["run_bgzip"]],
                          ## Sorting already done in previous step
                          sort_rows = FALSE, 
                          conda_env = conda_env,
                          verbose = verbose)
    #### Tabix-index file (required) #### 
    tbi_file <- index(bgz_file = bgz_file, 
                      chrom_col = chrom_col,
                      start_col = start_col,
                      end_col = end_col,
                      comment_char = comment_char,
                      force_new = force_new, 
                      method = method[["index"]], 
                      conda_env = conda_env, 
                      verbose = verbose)
    #### Report ####
    messager("Data successfully converted to",
             "bgzip-compressed, tabix-indexed format:\n",
             " - data:",bgz_file,"\n",
             " - index:",tbi_file,
             v=verbose) 
    return(list(
        data=bgz_file,
        index=tbi_file
    ))
}
