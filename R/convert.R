#' Convert to tabix 
#' 
#' Convert a tabular file to compressed (bgzip), indexed, tabix format 
#' for rapid querying.
#' @param bgz_file Path to resulting bgz-compressed file after
#'  \code{target_path} has been converted to tabix format.
#' @param chrom_col Name of the chromosome column 
#' in the \code{target_path} file.
#' @param start_col Name of the genomic start position column 
#' in the \code{target_path} file.
#' @param end_col Name of the genomic end position column 
#' in the \code{target_path} file.
#' @param format Format of the \code{target_path} file: "vcf" or "table".
#' @param convert_methods A named list containing methods to run each step with.
#' @param force_new Force the creation of a 
#' new bgzip file (\emph{.bgz}) and a new tabix index file (\emph{.tbi}).
#' @param verbose Print messages.
#' @inheritParams convert_and_query 
#' @inheritParams construct_query 
#' @inheritParams sort_coordinates
#' @inheritParams echoconda::find_packages
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
#' tabix_files <- echotabix::convert(target_path = tmp)
convert <- function(target_path,
                    bgz_file = construct_tabix_path(
                        target_path = target_path), 
                    chrom_col = "CHR",
                    start_col = "POS",
                    end_col = start_col,
                    comment_char = NULL,
                    format = NULL,
                    convert_methods = list(sort_coordinates="bash", 
                                           run_bgzip="Rsamtools",
                                           index="Rsamtools"),
                    conda_env = "echoR_mini",
                    force_new = TRUE,
                    verbose = TRUE) {
    
    # echoverseTemplate:::source_all()
    # echoverseTemplate:::args2vars(echotabix::convert)
    
    messager("========= echotabix::convert =========", v=verbose)
    messager("Converting full summary stats file to",
             "tabix format for fast querying.",
             v = verbose 
    )  
    convert_methods <- check_convert_methods(convert_methods=convert_methods, 
                                             verbose=verbose)
    #### Make sure input file isn't empty ####
    if(!file.exists(target_path)) {
        stop("Cannot find file specified by target_path.")
    }
    if (file.size(target_path) == 0) {
        messager("Removing empty file:", target_path, v=verbose)
        try({file.remove(target_path)})
    }
    #### Infer format #####
    format <- infer_tabix_format(format=format,
                                 path=target_path,
                                 verbose=verbose)
    #### infer comment char #### 
    comment_char <- infer_comment_char(target_path=target_path, 
                                       comment_char=comment_char,
                                       format = format,
                                       verbose=verbose) 
    #### Sort file by genomic coordinates #### 
    ## Required separate step if tabular 
    if(format=="table"){
        sort_out <- sort_coordinates(
            target_path = target_path,
            chrom_col = chrom_col,
            start_col = start_col, 
            end_col = end_col,
            comment_char = comment_char,
            method = convert_methods$sort_coordinates,
            save_path=tempfile(fileext = "_sorted.tsv"),
            outputs = "path",
            verbose = verbose)
    } else {
        sort_out <- target_path
    }
    #### bgzip-compress file (required) ####
    bgz_file <- run_bgzip(target_path = sort_out, 
                          bgz_file = bgz_file,
                          chrom_col = chrom_col,
                          start_col = start_col,
                          end_col = end_col,
                          comment_char = comment_char,
                          force_new = FALSE,
                          method = convert_methods[["run_bgzip"]],
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
                      method = convert_methods[["index"]], 
                      conda_env = conda_env, 
                      verbose = verbose)
    #### Report ####
    messager("Data successfully converted to",
             "bgzip-compressed, tabix-indexed format.",
             v=verbose) 
    return(list(
        path=bgz_file,
        index=tbi_file
    ))
}
