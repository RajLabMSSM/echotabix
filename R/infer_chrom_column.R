infer_chrom_column <- function(path,
                               verbose=TRUE){
    
    Corrected <- NULL;
    messager("Inferring chromosome column name.",v=verbose)
    if(endsWith(path,".bgz")){
        header <- read_bgz(path = path, 
                           nrows = 2, 
                           verbose = verbose)
    } else {
        header <- echodata::get_header(path = path,
                                       colnames_only = FALSE, 
                                       nrows = 2)
    }
    colmap <- subset(echodata::sumstatsColHeaders, Corrected=="CHR")
    colmap <- rbind(
        colmap, 
        c("CHROMOSOMES","CHR"),
        c("CHRO","CHR"),
        c("seq","CHR"),
        c("seqnames","CHR")
    )
    if(all(startsWith(colnames(header),"V"))){
        messager("No column names detected in header.",
                 "Inferring chrom type from first several rows of data.",
                 v=verbose)
        chr_detect <- sapply(header, function(x){
            all(grepl("chr", x, ignore.case = TRUE))
        })
        chrom_col <- names(chr_detect[chr_detect])[1]
        if(sum(chr_detect, na.rm = TRUE)>0){
            names(chrom_col) <- "has_chr=TRUE"
        }   else {
            names(chrom_col) <-"has_chr=FALSE"
        }
        return(chrom_col)
    } else {
        chrom_col <- grep(paste(colmap$Uncorrected, collapse = "|"), 
                          colnames(header), value = TRUE, ignore.case = TRUE) 
        return(chrom_col[1])
    } 
}
