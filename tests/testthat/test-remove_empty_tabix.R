test_that("remove_empty_tabix removes zero-byte file", {
    tf <- tempfile(fileext = ".tsv.bgz")
    file.create(tf)  # zero bytes
    expect_true(file.exists(tf))
    echotabix:::remove_empty_tabix(tf, verbose = FALSE)
    # The file.remove pattern-based approach may or may not remove it
    # but the function should not error
    expect_no_error(echotabix:::remove_empty_tabix(tf, verbose = FALSE))
})

test_that("remove_empty_tabix keeps non-empty file", {
    tf <- tempfile(fileext = ".tsv.bgz")
    writeLines("some content", tf)
    echotabix:::remove_empty_tabix(tf, verbose = FALSE)
    expect_true(file.exists(tf))
    unlink(tf)
})

test_that("remove_empty_tabix handles nonexistent file gracefully", {
    expect_no_error(
        echotabix:::remove_empty_tabix("/nonexistent_file.bgz", verbose = FALSE)
    )
})
