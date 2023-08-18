#' Query tabular: conda
#' 
#' Uses a conda-based installation of tabix instead of compiled 
#' \code{C} code from \pkg{Rhtslib}.
#' 
#' @param force Overwrite existing index without asking.
#' @param preset gff, bed, sam, vcf.
#' @param skip_lines skip first INT lines [0].
#' @param print_header Print also the header lines.
#' @param use_regions_file Specify query coordinates by writing them to
#'  a temporary file and supplying the file path to the "-R" argument.
#' @inheritParams construct_query 
#' @inheritParams query_table 
#' 
#' @keywords internal
#' @importFrom data.table fread
#' @importFrom echoconda yaml_to_env find_packages cmd_print
query_table_conda <- function(## Target args
                              target_path,
                              target_index,
                              ## Query args
                              query_granges, 
                              ## Tabix args
                              force=FALSE,
                              preset=NULL,
                              skip_lines=NULL,
                              print_header=TRUE,
                              use_regions_file=TRUE,
                              ## Extra args
                              conda_env="echoR_mini",
                              verbose=TRUE){ 
    # devoptera::args2vars(query_table_conda)
    
    messager("Querying tabular tabix file using: conda",
             v=verbose)
    ### Set up conda echoR ####
    conda_env <- echoconda::yaml_to_env(yaml_path = conda_env,
                                        verbose = verbose)
    #### Construct query (if not already in GRanges format) ####
    query_granges <- construct_query(query_dat=query_granges,
                                     verbose = FALSE)
    #### Find executable ####
    tabix <- echoconda::find_packages(packages = "tabix",
                                     conda_env = conda_env,
                                     return_path = TRUE,
                                     verbose = verbose)
    #### Ensure chromosome format is correct #### 
    fqs <- fix_query_style(target_path=target_path,
                           target_index=target_index,
                           query_granges=query_granges,
                           return_header = TRUE,
                           verbose=verbose)
    query_granges <-  fqs$query_granges
    #### Construct query #### 
    #### Save coordinates as a bed file ####
    if(isTRUE(use_regions_file)){
        # Restrict to regions listed in the FILE. The FILE can be BED file 
        # (requires .bed, .bed.gz, .bed.bgz file name extension) or 
        # a TAB-delimited file with 
        # CHROM, POS, and, optionally, POS_TO columns, where positions
        # are 1-based and inclusive. 
        # When this option is in use, the input file may not be sorted. 
        messager("Writing query coordinates file.",v=verbose)
        coords <- granges_to_dt(gr = query_granges) |> 
            data.table::setnames(old = c("chr","start","end"), 
                                 new = c("CHROM","POS","POS_TO"), 
                                 skip_absent = TRUE) 
        coords_file <- tempfile(fileext = "coords.tsv")
        data.table::fwrite(coords,
                           col.names = FALSE,
                           file = coords_file, 
                           sep=" ") 
    } else {
        coords <- granges_to_string(gr = query_granges)
    }
   
    #### Run ####
    ## Confusingly, "-h" means " --print-header", not "help".
    ## Worse yet, "-H" means "--only-header", which is not intuitive at all.
    cmd <- paste(
        tabix[[1]],
        if(isTRUE(print_header)) "-h" else NULL,
        if(isTRUE(force)) "-f" else NULL,
        if(is.character(preset)) paste("-p",preset) else NULL,
        "-b",fqs$header$indexColumns[["start"]],
        "-e",fqs$header$indexColumns[["end"]],
        "-s",fqs$header$indexColumns[["seq"]],
        target_path,
        if(isTRUE(use_regions_file)) paste("-R",coords_file) else coords
        )
    echoconda::cmd_print(cmd, verbose = verbose, raw = TRUE)
    #### Read in results directly to R ####
    dat <- data.table::fread(cmd = cmd)
    return(dat)
}
