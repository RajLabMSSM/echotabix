tabix_index <- function(bgz_file,
                        seq_i,
                        start_i,
                        end_i,
                        comment,
                        skip_lines=1,
                        force_new=FALSE,
                        conda_env="echoR",
                        verbose=TRUE){   
    
    pkgs <- echoconda::find_packages(packages="tabix",
                                      conda_env=conda_env,
                                      verbose = verbose)
    tabix <- pkgs$path[[1]][1]
    messager("echotabix:: Indexing tabix file.",v=verbose)
    cmd2 <- paste(tabix,
                  # Force overwrite of .tbi index file
                  if(force_new) "-f" else NULL,
                  "-S",skip_lines, 
                  "-s",seq_i,
                  "-b",start_i,
                  "-e",end_i,
                  "-c",comment,
                  bgz_file)
    echoconda::cmd_print(cmd2, verbose=verbose)
    system(cmd2) 
    return(bgz_file)
}
