#' Infer chromosome type
#' 
#' In a given vector or stored table, 
#' determine whether chromosome type is of the format \code{"chr1" }
#' (returns \code{TRUE}) or \code{1} (returns \code{FALSE}).
#' 
#' @param chrom A value (character, numeric, or integer) 
#' giving an example of the chromosome column.
#' @param path Path to stored file.
#' @param chrom_col The name of the chromosome column in the file.
#' @param verbose Print messages.
#' @export
#' @importFrom echodata get_header
#' @examples 
#' echotabix::infer_chrom_type(chrom="chr1")
#' echotabix::infer_chrom_type(chrom=1)
infer_chrom_type <- function(chrom=NULL,
                             path=NULL,
                             chrom_col="CHR", 
                             verbose=TRUE){
  chrom <- chrom[1]
  if(!is.null(chrom)){
    has_chr <- grepl("chr",chrom, ignore.case = TRUE)
  } else { 
    messager("Determining chrom type from file header.")
    #### Infer chrom column is not provided ####
    if(is.null(chrom_col)){
      chrom_col <- infer_chrom_column(path=path,
                                      verbose=verbose) 
    } 
    #### Now infer chrom type ####
    if((!is.null(names(chrom_col))) && 
       (startsWith(names(chrom_col),"has_chr="))){ 
      #### evaluate the name to assign the has_chr variable ####
      eval(parse(text=names(chrom_col)[1]))
    } else {
      #### get header ####
      header <- echodata::get_header(path = path,
                                     colnames_only = FALSE,
                                     nrows = 5)
      has_chr <- grepl("chr",header[[chrom_col]][1], ignore.case = TRUE)
    } 
  }
  messager("Chromosome format:",if(has_chr) "chr1" else "1", v=verbose)
  return(has_chr)
}
