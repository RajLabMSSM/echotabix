test_that("check_delimiter detects tab delimiter", {
    tf <- tempfile(fileext = ".tsv")
    dat <- data.frame(A = 1:3, B = 4:6)
    data.table::fwrite(dat, tf, sep = "\t")
    result <- echotabix::check_delimiter(path = tf, verbose = FALSE)
    expect_equal(result, "\t")
    unlink(tf)
})

test_that("check_delimiter detects comma delimiter", {
    tf <- tempfile(fileext = ".csv")
    dat <- data.frame(A = 1:3, B = 4:6)
    data.table::fwrite(dat, tf, sep = ",")
    result <- echotabix::check_delimiter(path = tf, verbose = FALSE)
    expect_equal(result, ",")
    unlink(tf)
})

test_that("check_delimiter errors for nonexistent file", {
    expect_error(
        echotabix::check_delimiter(path = "/nonexistent_file.tsv",
                                   verbose = FALSE),
        "File does not exist"
    )
})
