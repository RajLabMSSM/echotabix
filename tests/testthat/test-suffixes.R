test_that("suffixes returns all formats by default", {
    s <- echotabix:::suffixes()
    expect_type(s, "character")
    expect_true(length(s) > 0)
    expect_true(".tsv" %in% s)
    expect_true(".vcf" %in% s)
    expect_true(".vcf.gz" %in% s)
    expect_true(".tsv.bgz" %in% s)
})

test_that("suffixes with tabular=FALSE excludes plain tabular", {
    s <- echotabix:::suffixes(tabular = FALSE)
    expect_false(".tsv" %in% s)
    expect_false(".txt" %in% s)
    expect_false(".csv" %in% s)
})

test_that("suffixes with vcf=FALSE excludes plain vcf", {
    s <- echotabix:::suffixes(vcf = FALSE)
    expect_false(".vcf" %in% s)
    # Compressed VCF should still be present
    expect_true(".vcf.gz" %in% s)
})

test_that("suffixes with only tabular_compressed=TRUE", {
    s <- echotabix:::suffixes(tabular = FALSE, vcf = FALSE,
                              vcf_compressed = FALSE)
    expect_true(all(grepl("\\.(gz|bgz)$", s)))
    expect_true(any(grepl("\\.tsv\\.", s)))
})

test_that("suffixes with all FALSE returns empty", {
    s <- echotabix:::suffixes(tabular = FALSE, tabular_compressed = FALSE,
                              vcf = FALSE, vcf_compressed = FALSE)
    expect_length(s, 0)
})
