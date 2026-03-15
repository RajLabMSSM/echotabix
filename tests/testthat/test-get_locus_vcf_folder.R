test_that("get_locus_vcf_folder creates LD subfolder", {
    td <- file.path(tempdir(), "locus_test_dir")
    dir.create(td, showWarnings = FALSE)
    result <- echotabix:::get_locus_vcf_folder(locus_dir = td)
    expect_equal(result, file.path(td, "LD"))
    expect_true(dir.exists(result))
    unlink(td, recursive = TRUE)
})
