#' Tabix-index a file: conda
#' 
#' Tabix-index a tabular summary statistics file.
#' @inheritParams construct_query
#' @inheritParams index
index_conda <- function(bgz_file,
                        chrom_i,
                        start_i,
                        end_i,
                        comment_char,
                        skip_lines=0,
                        force_new=FALSE,
                        conda_env="echoR",
                        verbose=TRUE){   
    messager("echotabix:: Tabix-indexing file using conda.",v=verbose) 
    pkgs <- echoconda::find_packages(packages="tabix",
                                      conda_env=conda_env,
                                      verbose = verbose)
    tabix <- pkgs$path[[1]][1] 
    cmd2 <- paste(tabix,
                  # Force overwrite of .tbi index file
                  if(force_new) "-f" else NULL,
                  "-S",skip_lines, 
                  "-s",chrom_i,
                  "-b",start_i,
                  "-e",end_i,
                  "-c",comment_char,
                  bgz_file)
    echoconda::cmd_print(cmd2, verbose=verbose)
    system(cmd2) 
}
