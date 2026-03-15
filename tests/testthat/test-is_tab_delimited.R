test_that("is_tab_delimited returns TRUE for tab-separated file", {
    tf <- tempfile(fileext = ".tsv")
    dat <- data.frame(X = 1:3, Y = 4:6)
    data.table::fwrite(dat, tf, sep = "\t")
    expect_true(echotabix:::is_tab_delimited(tf))
    unlink(tf)
})

test_that("is_tab_delimited returns FALSE for comma-separated file", {
    tf <- tempfile(fileext = ".csv")
    dat <- data.frame(X = 1:3, Y = 4:6)
    data.table::fwrite(dat, tf, sep = ",")
    expect_false(echotabix:::is_tab_delimited(tf))
    unlink(tf)
})
