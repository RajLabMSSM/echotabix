remove_empty_tabix <- function(f,
                               min_size = 1,
                               verbose = TRUE) {
    if (file.exists(f)) {
        if (file.size(f) < 1) { # Less than (min_size) bytes
            messager("Removing empty tabix file and its index.",
                v = verbose
            )
            try({file.remove(paste0(f, "*"))}) # Remove both
        }
    }
}
