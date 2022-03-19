is_tabix <- function(file_path) {
    # must meet all of these conditions in order to use a pre-existing tabix files
    file.exists(file_path) &
        (endsWith(file_path, ".gz") | endsWith(file_path, ".bgz")) &
        file.exists(paste0(file_path, ".tbi")) &
        file.size(file_path) > 0
}
