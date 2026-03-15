test_that("infer_tabix_format returns vcf for explicit vcf format", {
    result <- echotabix:::infer_tabix_format(format = "vcf",
                                             path = "dummy.tsv",
                                             verbose = FALSE)
    expect_equal(result, "vcf")
})

test_that("infer_tabix_format returns vcf for format='v'", {
    result <- echotabix:::infer_tabix_format(format = "v",
                                             path = "dummy.tsv",
                                             verbose = FALSE)
    expect_equal(result, "vcf")
})

test_that("infer_tabix_format returns table for explicit table format", {
    result <- echotabix:::infer_tabix_format(format = "table",
                                             path = "dummy.vcf",
                                             verbose = FALSE)
    expect_equal(result, "table")
})

test_that("infer_tabix_format returns table for format='tabular'", {
    result <- echotabix:::infer_tabix_format(format = "tabular",
                                             path = "dummy.vcf",
                                             verbose = FALSE)
    expect_equal(result, "table")
})

test_that("infer_tabix_format infers vcf from path", {
    result <- echotabix:::infer_tabix_format(format = NULL,
                                             path = "data.vcf.gz",
                                             verbose = FALSE)
    expect_equal(result, "vcf")
})

test_that("infer_tabix_format infers table from non-vcf path", {
    result <- echotabix:::infer_tabix_format(format = NULL,
                                             path = "data.tsv.gz",
                                             verbose = FALSE)
    expect_equal(result, "table")
})
