#' Run bgzip
#' 
#' Compress a file using bgzip.
#' 
#' @param sort_rows Sort rows by genomic coordinates.
#' @param validate Check that the bgzip file exists and can be read in as a 
#' \link[data.table]{data.table}. 
#' @export
#' @examples 
#' #### Example with full data ####
#' # tmp <- echodata::example_fullSS()
#' #### Example with single locus ####
#' dat <- echodata::BST1
#' tmp <- tempfile()
#' data.table::fwrite(dat, tmp)
#' 
#' bgz_file <-  echotabix::run_bgzip(fullSS_path=tmp, 
#'                                  chrom_col="CHR", 
#'                                  start_col="BP")
run_bgzip <- function(fullSS_path,
                      chrom_col,
                      start_col,
                      end_col=start_col,
                      comment_char = NULL,
                      bgz_file = construct_tabix_path(path = fullSS_path), 
                      sort_rows = TRUE,
                      force_new = TRUE,
                      method = c("Rsamtools","conda"),
                      conda_env = "echoR",
                      validate = TRUE,
                      verbose = TRUE){
    
    method <- tolower(method[1])
    if(missing(chrom_col)) stop("chrom_col required.")
    if(missing(start_col)) stop("start_col required.") 
    #### Make sure input file isn't empty #### 
    remove_empty_tabix(f = fullSS_path, 
                       verbose = verbose)
    if(!file.exists(fullSS_path)) stop("Must provide a valid fullSS_path.")
    #### Search for existing bgzipped file ####
    if(file.exists(bgz_file) && force_new==FALSE){
        messager("Using existing bgzipped file:",bgz_file,
                 "\nSet force_new=TRUE to override this.",
                 v=verbose)
        return(bgz_file)
    }
    #### Sort file first [optional] ####
    if(sort_rows){
        fullSS_path <- sort_coordinates(fullSS_path = fullSS_path,
                                        chrom_col = chrom_col,
                                        start_col = start_col,
                                        end_col = end_col,
                                        comment_char = comment_char,
                                        save_path = fullSS_path,
                                        outputs = "path",
                                        verbose = verbose)
    } 
    #### Rsamtools (uses old version: bgzip==1.13) ####
    if(method=="rsamtools"){  
        bgz_file <- bgzip_rsamtools(fullSS_path=fullSS_path,
                                    bgz_file=bgz_file,
                                    force_new=force_new,
                                    verbose=verbose) 
    #### conda (uses latest version: bgzip>=1.15) ####
    } else if(method=="conda"){  
        bgz_file <- bgzip_conda(fullSS_path=fullSS_path,
                                bgz_file=bgz_file,
                                chrom_col=chrom_col,
                                start_col=start_col,
                                end_col = end_col,
                                comment_char=comment_char, 
                                conda_env=conda_env,
                                verbose=verbose)
    }  
    #### Validate bgz file ####
    if(validate){
        header <- echodata::read_bgz(file = bgz_file, 
                                     nrows=5)
        if(!methods::is(header,"data.frame")){
            stop("bgz_file header is not a data.frame as expected.")
        }
       if(verbose){
           messager("Header preview:")
           echodata::preview(file = header,
                             nrows = 5L)
       }
    }
    return(bgz_file)
}
