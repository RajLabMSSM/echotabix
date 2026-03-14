#' Sort coordinates: bash
#' 
#' Support function for \link[echotabix]{sort_coordinates}.
#'  
#' @inheritParams sort_coordinates
#' @inheritParams convert
#' @inheritParams construct_query
#' @inheritParams construct_outputs
#' @keywords internal  
sort_coordinates_bash <- function(target_path, 
                                  chrom_col,
                                  start_col,
                                  end_col=start_col,
                                  comment_char=NULL,
                                  save_path=NULL, 
                                  outputs=c("command","path","data"),
                                  verbose=TRUE){ 
    
    messager("Sorting rows by coordinates via bash.",v=verbose)
    #### Check outputs arg ####
    outputs <- check_outputs(outputs = outputs, 
                             func = sort_coordinates_bash)
    #### If compressed, will need to text query with zgrep instead ####
    z_grep <- select_zgrep(target_path = target_path, 
                           verbose = verbose) 
    #### Infer comment_char arg from header ####
    comment_char <- infer_comment_char(target_path = target_path, 
                                       comment_char = comment_char,
                                       verbose = verbose) 
    cDict <-  echodata::column_dictionary(path = target_path)
    #### Validate column names exist in file header ####
    for(col_arg in list(c("chrom_col", chrom_col),
                        c("start_col", start_col),
                        c("end_col", end_col))){
        if(!col_arg[2] %in% names(cDict)){
            stop(col_arg[1], " '", col_arg[2],
                 "' not found in file header. Available columns: ",
                 paste(names(cDict), collapse = ", "))
        }
    }
    #### create save dir ####
    if(!is.null(save_path)){
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    }
    #### Avoid overwriting input file with shell redirect ####
    ## If save_path == target_path (after extension stripping), the redirect
    ## would truncate the input before grep can read it.
    use_tmp <- FALSE
    actual_save <- save_path
    if(!is.null(save_path) &&
       normalizePath(save_path, mustWork = FALSE) ==
       normalizePath(target_path, mustWork = FALSE)){
        save_path <- tempfile(fileext = "_sort_tmp.tsv")
        use_tmp <- TRUE
    }
    #### Construct command ####
    cmd <- paste("(",
                 #### Extract the header col and sort everything else ####
                 paste0(z_grep," ^",shQuote(comment_char)," ",target_path,"; ",
                        z_grep," -v ^",shQuote(comment_char)," ",target_path
                 ),
                 paste0("| sort -k",
                        cDict[[chrom_col]],",",
                        ## !!IMPORTANT!!: "n" is critical because otherwise
                        ## `sort` will order numbers
                        ## as: 1,12,18,4
                        ## However, sort only works if chromosome
                        ## is in numeric format.
                        cDict[[chrom_col]],"n"
                 ),
                 paste0("-k",
                        cDict[[start_col]],",",
                        cDict[[end_col]],"n"
                 ),
                 ")",
                 if(!is.null(save_path)) paste(">",save_path) else save_path
    )  
    cmd <- trimws(cmd)
    #### Return ####
    dat <- NULL;
    if(any(c("path","data") %in% outputs)){
        #### Only execute the command if other outputs are selected ####
        echoconda::cmd_print(cmd, verbose = verbose)
        system(cmd)
        #### Move temp file to actual destination if needed ####
        if(use_tmp && file.exists(save_path)){
            file.copy(save_path, actual_save, overwrite = TRUE)
            file.remove(save_path)
            save_path <- actual_save
        }
        #### Only read in data if selected in outputs ####
        if("data" %in% outputs){
            if(!is.null(save_path)){
                dat <- data.table::fread(save_path, nThread = 1)
            }
        }
    }
    out <- construct_outputs(outputs = outputs,
                             command = cmd,
                             path = save_path,
                             data = dat)
    return(out)
}
