is_local <- function(path) {
    # ssh.utils::file.exists.remote(file = fullSS_tabix) # Doesn't work?
    file.exists(fullSS_tabix)
}
