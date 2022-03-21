#' VCF path
#' 
#' Construct the \code{save_path} to VCF subset 
#' extracted by \link[echotabix]{query_vcf}.
#' 
#' @param use_coord_prefix Add min/max genomic coordinates 
#' (e.g. "chr4-14737349-16737284") to the file name.
#' @inheritParams query_vcf
#' 
#' @family tabix functions
#' @export
#' @examples
#' path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
#'                     package = "echotabix")
#' locus_dir <- echodata::locus_dir
#' BST1 <- echodata::BST1
#' save_path <- echotabix::vcf_path(query_dat = query_dat,
#'                                  locus_dir = locus_dir,
#'                                  path = path)
vcf_path <- function(target_path, 
                     query_dat = NULL,
                     locus_dir = tempdir(),
                     subdir = "VCF",
                     query_chrom_col="CHR",
                     query_start_col = "POS",
                     query_end_col = query_start_col,
                     use_coord_prefix=TRUE,
                     whole_vcf = FALSE) {
    
    #### Make the basic file target_path ####
    vcf_name <- gsub(".vcf|.gz.|.bgz", "", basename(target_path))
    vcf_folder <-  file.path(locus_dir, subdir)  
    #### If query_dat not given, make a simpler save_path ####
    if(is.null(query_dat)){
        save_path <- file.path(vcf_folder, vcf_name)
    #### If query_dat is given, make a fancier save_path ####
    } else {
        #### Add chromosome-based prefix #####
        chrom_prefix <- if(query_chrom_col %in% colnames(query_dat)){
            chrom_prefix <- paste(
                unique(paste0("chr",gsub("chr", "", query_dat[[query_chrom_col]]))), 
                collapse = "-"
            )
        } else {""}
        
        #### Add a position-based prefix ####
        ## Only add this when the number of chromosomes is less than 2
        ## Otherwise, the prefix name will get super long 
        pos_prefix <- if(
            all(c(query_start_col, query_end_col, query_chrom_col) %in% colnames(query_dat)) && 
                         length(unique(query_dat[[query_chrom_col]]))<2
            ){
            paste(min(query_dat[[query_start_col]], na.rm = TRUE),
                  max(query_dat[[query_end_col]], na.rm = TRUE),sep="-")
        } else {""}
        #### Combine prefixes ####
        coord_prefix <- if(use_coord_prefix) {
            paste(chrom_prefix, pos_prefix, sep = "-")
        } else {""}
        #### Construct save_path name ####
        save_path <- file.path(
            vcf_folder,
            if (whole_vcf) {
                paste(basename(vcf_name), chrom_prefix, sep = ".")
            } else {
                paste(basename(locus_dir),
                      coord_prefix,
                      basename(vcf_name), sep = ".")
            }
        )
    } 
    #### Post-processing ####
    #### If there's no suffix, add one ####
    if (!any(endsWith(save_path, get_vcf_suffixes()))) {
        save_path <- paste0(save_path, ".vcf.bgz")
    }
    #### Make sure the dir exists ####
    dir.create(path = dirname(save_path), 
               recursive = TRUE, showWarnings = FALSE) 
    return(save_path)
}
