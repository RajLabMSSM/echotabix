test_that("is_vcf detects VCF suffixes", {
    expect_true(echotabix:::is_vcf("data.vcf"))
    expect_true(echotabix:::is_vcf("data.vcf.gz"))
    expect_true(echotabix:::is_vcf("data.vcf.bgz"))
    expect_true(echotabix:::is_vcf("/path/to/file.vcf.tsv.gz"))
})

test_that("is_vcf rejects non-VCF files", {
    expect_false(echotabix:::is_vcf("data.tsv"))
    expect_false(echotabix:::is_vcf("data.csv"))
    expect_false(echotabix:::is_vcf("data.txt.gz"))
    expect_false(echotabix:::is_vcf("data.bed"))
})

test_that("is_vcf is case insensitive", {
    expect_true(echotabix:::is_vcf("data.VCF.GZ"))
    expect_true(echotabix:::is_vcf("data.VCF"))
})
