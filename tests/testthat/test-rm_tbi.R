test_that("rm_tbi removes .tbi files", {
    td <- file.path(tempdir(), "rm_tbi_test")
    dir.create(td, showWarnings = FALSE)
    # Create fake .tbi files
    f1 <- file.path(td, "data1.tsv.bgz.tbi")
    f2 <- file.path(td, "data2.vcf.bgz.tbi")
    f3 <- file.path(td, "keep_me.tsv")  # should NOT be removed
    file.create(f1, f2, f3)

    echotabix:::rm_tbi(path = td)

    expect_false(file.exists(f1))
    expect_false(file.exists(f2))
    expect_true(file.exists(f3))  # non-tbi file preserved
    unlink(td, recursive = TRUE)
})

test_that("rm_tbi does nothing when no .tbi files exist", {
    td <- file.path(tempdir(), "rm_tbi_empty_test")
    dir.create(td, showWarnings = FALSE)
    f <- file.path(td, "data.tsv")
    file.create(f)

    # Should not error
    expect_no_error(echotabix:::rm_tbi(path = td))
    expect_true(file.exists(f))
    unlink(td, recursive = TRUE)
})
