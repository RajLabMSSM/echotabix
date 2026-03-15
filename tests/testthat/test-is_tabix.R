test_that("is_tabix returns FALSE for non-existent files", {
    expect_false(echotabix:::is_tabix("/nonexistent/file.tsv.bgz"))
})

test_that("is_tabix returns FALSE for file without .tbi index", {
    tf <- tempfile(fileext = ".tsv.bgz")
    writeLines("test", tf)
    expect_false(echotabix:::is_tabix(tf))
    unlink(tf)
})

test_that("is_tabix returns FALSE for empty file with index", {
    tf <- tempfile(fileext = ".tsv.bgz")
    file.create(tf)  # zero-size file
    file.create(paste0(tf, ".tbi"))
    expect_false(echotabix:::is_tabix(tf))
    unlink(c(tf, paste0(tf, ".tbi")))
})

test_that("is_tabix returns TRUE for valid tabix-like setup", {
    tf <- tempfile(fileext = ".tsv.bgz")
    writeLines("some data", tf)
    file.create(paste0(tf, ".tbi"))
    expect_true(echotabix:::is_tabix(tf))
    unlink(c(tf, paste0(tf, ".tbi")))
})

test_that("is_tabix works with multiple inputs", {
    tf1 <- tempfile(fileext = ".tsv.bgz")
    tf2 <- tempfile(fileext = ".csv.bgz")
    writeLines("data1", tf1)
    file.create(paste0(tf1, ".tbi"))
    # tf2 has no tbi
    writeLines("data2", tf2)

    result <- echotabix:::is_tabix(c(tf1, tf2))
    expect_length(result, 2)
    expect_true(result[1])
    expect_false(result[2])
    unlink(c(tf1, paste0(tf1, ".tbi"), tf2))
})
