get_column_indices <- function(fullSS_path,
                               chrom_col,
                               start_col,
                               end_col=start_col){
    #### Get indices of each required column ####
    cDict <- echodata::column_dictionary(file_path = fullSS_path)
    ### Check column exist ####
    if(!chrom_col %in% names(cDict)) stop("chrom_col not found in file.")
    if(!start_col %in% names(cDict)) stop("start_col not found in file.")
    if(!end_col %in% names(cDict)) stop("end_col not found in file.")
    chrom_i <- cDict[[chrom_col]]
    start_i <-  cDict[[start_col]]
    end_i <- cDict[[end_col]] 
    return(list(
        chrom_i=chrom_i,
        start_i=start_i,
        end_i=end_i
    ))
}