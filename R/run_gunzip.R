#' Run gunzip
#' 
#' Decompress a file using \code{gunzip}.
#' 
#' @param path Path to file. 
#' @param method Method to gunzip \code{path} with:
#' \itemize{
#' \item{"conda": }{Finds gunzip binary in the specified 
#' \code{conda_env} and uses it.}
#' \item{"R.utils": }{Uses the \link[R.utils]{gunzip} function.}
#' }
#' @param outputs Which outputs to return in a list. 
#' Can be one or more of the following:
#' \itemize{
#' \item{"command" : }{The command run to gunzip the file.}
#' \item{"path" : }{Path to the gunzipped file.}
#' \item{"data" : }{The data imported from the gunzipped file.}
#' }
#' If only one valid option is selected, the item will be returned without
#' being embedded within a list.
#' @inheritParams convert
#' @inheritParams sort_coordinates
#' @inheritParams echoconda::find_packages
#' 
#' @export
#' @importFrom echodata is_local
#' @importFrom R.utils isCompressedFile
#' @examples 
#' dat <- echodata::BST1
#' tmp <- tempfile(fileext = ".csv.gz")
#' data.table::fwrite(dat, tmp)
#' out <- echotabix::run_gunzip(path=tmp)
run_gunzip <- function(path,  
                       conda_env="echoR_mini",
                       method = c("R.utils","conda"),
                       outputs = c("command","path","data"),
                       verbose=TRUE){
    
    method <- tolower(method)[1]
    #### Check that it's not already decompressed ####
    ext <- rev(strsplit(path,"[.]")[[1]])[1]
    #### Check compression type (only works for local files) ####
    if(echodata::is_local(path)){
        if(!R.utils::isCompressedFile(path, ext=ext)){
            messager("File is not compressed. Returning path.",v=verbose)
            return(path)
        } 
    } 
    #### Check outputs arg ####
    outputs <- check_outputs(outputs = outputs, 
                             func = run_gunzip)
    #### Run gunzip ####
    if(method=="conda"){
        out <- run_gunzip_conda(path=path, 
                                outputs=outputs,
                                conda_env=conda_env,
                                verbose=verbose)
    } else {
        out <- run_gunzip_rutils(path=path, 
                                 outputs=outputs,
                                 overwrite=TRUE,
                                 remove=FALSE,
                                 verbose=verbose)
    }
    return(out)
}
