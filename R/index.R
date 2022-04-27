#' Tabix-index a file
#' 
#' Tabix-index a tabular summary statistics file.
#' @param bgz_file Path to a file that has been compressed with \code{bgzip}
#' (e.g. via \link[echotabix]{run_bgzip}).
#' @param comment_char Comment character denoting which row contains the column 
#' names (e.g. "#CHR" or "SNP").
#' @param method Method to index tabix file with.
#' @inheritParams construct_query
#' @inheritParams convert
#' @inheritParams echoconda::find_packages
#' 
#' @family tabix functions
#' @export
#' @examples 
#' dat <- echodata::BST1
#' tmp <- tempfile(fileext = ".tsv.gz")
#' data.table::fwrite(dat, tmp, sep="\t")
#' bgz_file <- echotabix::run_bgzip(target_path = tmp,
#'                                  chrom_col = "CHR",
#'                                  start_col = "POS")
#' tbi_file <- echotabix::index(bgz_file = bgz_file, 
#'                              chrom_col = "CHR", 
#'                              start_col = "POS")
index <- function(bgz_file,
                  chrom_col,
                  start_col,
                  end_col=start_col,
                  comment_char=NULL,
                  force_new=TRUE,
                  method = c("conda","seqminer","Rsamtools"), 
                  conda_env = "echoR_mini",
                  verbose = TRUE
                  ){
    method <- tolower(method)[1]
    #### Infer comment_char arg from header ####
    comment_char <- infer_comment_char(target_path = bgz_file, 
                                       comment_char = comment_char,
                                       verbose = verbose)
    indices <- get_column_indices(target_path=bgz_file,
                                  chrom_col=chrom_col,
                                  start_col=start_col,
                                  end_col=end_col)
    if(method=="seqminer"){
        index_seqminer(bgz_file=bgz_file,
                       chrom_i=indices$chrom_i,
                       start_i=indices$start_i,
                       end_i=indices$end_i,
                       comment_char=comment_char, 
                       verbose=verbose)
    } else if(method=="rsamtools"){
        index_rsamtools(bgz_file=bgz_file,
                        chrom_i=indices$chrom_i,
                        start_i=indices$start_i,
                        end_i=indices$end_i,
                        comment_char=comment_char, 
                        verbose=verbose)
    } else {
        index_conda(bgz_file=bgz_file,
                    chrom_i=indices$chrom_i,
                    start_i=indices$start_i,
                    end_i=indices$end_i,
                    comment_char=comment_char, 
                    force_new=force_new,
                    conda_env=conda_env,
                    verbose=verbose) 
    } 
    tbi_file <- paste0(bgz_file,".tbi")
    if(!file.exists(tbi_file)) stop("Tabix indexing failed.")
    return(tbi_file)
}
