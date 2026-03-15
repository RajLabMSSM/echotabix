test_that("infer_chrom_type detects chr prefix from string", {
    result <- echotabix::infer_chrom_type(chrom = "chr1", verbose = FALSE)
    expect_true(result)
})

test_that("infer_chrom_type detects numeric chromosome", {
    result <- echotabix::infer_chrom_type(chrom = 1, verbose = FALSE)
    expect_false(result)
})

test_that("infer_chrom_type detects numeric string chromosome", {
    result <- echotabix::infer_chrom_type(chrom = "1", verbose = FALSE)
    expect_false(result)
})

test_that("infer_chrom_type with chrX", {
    result <- echotabix::infer_chrom_type(chrom = "chrX", verbose = FALSE)
    expect_true(result)
})

test_that("infer_chrom_type uses only first element of vector", {
    result <- echotabix::infer_chrom_type(
        chrom = c("chr1", "2", "chr3"), verbose = FALSE
    )
    expect_true(result)
})
