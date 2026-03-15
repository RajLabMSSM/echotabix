test_that("get_vcf_suffixes returns expected patterns", {
    s <- echotabix:::get_vcf_suffixes()
    expect_type(s, "character")
    expect_true(length(s) > 0)
    # Should contain both lower and upper case variants
    expect_true("\\.vcf.gz" %in% s)
    expect_true("\\.vcf.bgz" %in% s)
    expect_true("\\.vcf" %in% s)
    expect_true("\\.VCF" %in% s)
    expect_true("\\.VCF.GZ" %in% s)
})

test_that("get_vcf_suffixes includes consortium-specific formats", {
    s <- echotabix:::get_vcf_suffixes()
    # PGC-style suffixes like .vcf.tsv.gz
    expect_true(any(grepl("vcf.*tsv", s, ignore.case = TRUE)))
})
