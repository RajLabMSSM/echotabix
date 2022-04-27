#' VCF path
#' 
#' Construct the \code{save_path} to VCF subset 
#' extracted by \link[echotabix]{query_vcf}.
#' 
#' @param locus_dir Locus-specific folder.
#' @param use_coord_prefix Add min/max genomic coordinates 
#' (e.g. "chr4-14737349-16737284") to the file name.
#' @param subdir Subdirectory to store VCF in. 
#' @param whole_vcf Whether to download the entire VCF (not just a subset). 
#' @inheritParams query_vcf
#' @inheritParams convert_and_query
#' @inheritParams construct_query
#' 
#' @family tabix functions
#' @export
#' @examples
#' target_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
#'                            package = "echodata")
#'locus_dir <- file.path(tempdir(), echodata::locus_dir)
#' query_granges <- echotabix::construct_query(query_dat=echodata::BST1)
#'
#' save_path <- echotabix::construct_vcf_path(query_granges = query_granges,
#'                                            locus_dir = locus_dir,
#'                                            target_path = target_path)
construct_vcf_path <- function(target_path, 
                               query_granges = NULL,
                               locus_dir = tempdir(),
                               subdir = "VCF",
                               use_coord_prefix=TRUE,
                               whole_vcf = FALSE) {
    
    #### Make the basic file target_path ####
    vcf_name <- gsub(paste(c(get_vcf_suffixes(),
                             "\\.gz.|\\.bgz"), collapse = "|"),
                     "", basename(target_path)) 
    vcf_folder <-  file.path(locus_dir, subdir)  
    #### If query_dat not given, make a simpler save_path ####
    if(is.null(query_granges)){
        save_path <- file.path(vcf_folder, vcf_name)
    #### If query_dat is given, make a fancier save_path ####
    } else {
        all_chroms <- unique(
            as.character(GenomicRanges::seqnames(query_granges))
        )
        min_pos <- as.character(GenomicRanges::start(query_granges))
        max_pos <- as.character(GenomicRanges::end(query_granges))
        #### Add chromosome-based prefix ##### 
        chrom_prefix <- paste(
            unique(paste0("chr",gsub("chr", "",all_chroms))), 
            collapse = "-"
        )  
        #### Add a position-based prefix ####
        ## Only add this when the number of chromosomes is less than 2
        ## Otherwise, the prefix name will get super long 
        pos_prefix <- if(length(all_chroms)<2) {
            paste(min_pos, max_pos,sep="-")
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
