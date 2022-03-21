#' Download chain file for liftover
#' 
#' @param build_conversion Converting from what build to what? 
#' "hg38ToHg19" or "hg19ToHg38" 
#' @param save_dir Where is the chain file saved? Default is a temp directory
#' @param verbose extra messages printed? Default is TRUE
#' 
#' @source \href{https://hgdownload.cse.ucsc.edu/goldenpath/hg19/liftOver/}{
#' UCSC chain files}
#' 
#' @return Loaded chain file for liftover.
#' @family liftover functions
#' 
#' @keywords internal
#' @importFrom tools R_user_dir
#' @importFrom utils download.file
#' @importFrom R.utils gunzip
#' @importFrom rtracklayer import.chain
get_chain_file <- function(build_conversion = c("hg38ToHg19", "hg19ToHg38"), 
                           save_dir = tools::R_user_dir(package = "echotabix",
                                                        which = "cache"),
                           timeout= 60*5,
                           verbose = TRUE) {

    #### Define paths ####
    ucsc_ref <- tolower(strsplit(build_conversion, "To")[[1]][1])
    remote_path <- file.path(
        paste0(
            "ftp://hgdownload.cse.ucsc.edu/goldenPath/",ucsc_ref,
            "/liftOver"
        ),
        paste0(build_conversion[1], ".over.chain.gz")
    )
    local_path <- file.path(save_dir, basename(remote_path))
    local_path_gunzip <- gsub(".gz", "", local_path)
    ### Download chain file ####
    if (file.exists(local_path_gunzip)) {
        if (verbose) {
            messager("Using existing chain file.",
                     v=verbose)
        }
    } else {
        if (verbose) {
            messager("Downloading chain file from UCSC Genome Browser.",
                     v=verbose)
        }
        options(timeout = timeout)
        dir.create(save_dir,showWarnings = FALSE, recursive = TRUE)
        utils::download.file(
            url = remote_path,
            destfile = local_path
        )
        messager(local_path,v=verbose)
        local_path_gunzip <- R.utils::gunzip(local_path)
    }
    #### Import ####
    chain <- rtracklayer::import.chain(local_path_gunzip)
    return(chain)
}
