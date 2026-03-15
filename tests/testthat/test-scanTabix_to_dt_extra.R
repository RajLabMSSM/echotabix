test_that("scanTabix_to_dt converts simulated query results", {
    queries <- list(
        "chr1:100-500" = c("chr1\t150\trs1\t0.01", "chr1\t200\trs2\t0.05"),
        "chr2:100-300" = c("chr2\t120\trs3\t0.1")
    )
    header <- list(header = character(0), seqnames = c("chr1", "chr2"))

    result <- echotabix::scanTabix_to_dt(
        header = header,
        queries = queries,
        verbose = FALSE
    )
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 3)
    expect_true("query" %in% colnames(result))
})

test_that("scanTabix_to_dt handles single-row query result", {
    queries <- list(
        "chr1:100-500" = "chr1\t150\trs1\t0.01"
    )
    header <- list(header = character(0), seqnames = "chr1")

    result <- echotabix::scanTabix_to_dt(
        header = header,
        queries = queries,
        verbose = FALSE
    )
    expect_equal(nrow(result), 1)
})

test_that("scanTabix_to_dt without query names column", {
    queries <- list(
        "chr1:100-500" = c("chr1\t150\trs1", "chr1\t200\trs2")
    )
    header <- list(header = character(0), seqnames = "chr1")

    result <- echotabix::scanTabix_to_dt(
        header = header,
        queries = queries,
        add_query_names = FALSE,
        verbose = FALSE
    )
    expect_false("query" %in% colnames(result))
})

test_that("scanTabix_to_dt removes duplicate rows", {
    queries <- list(
        "chr1:100-500" = c("chr1\t150\trs1", "chr1\t150\trs1")
    )
    header <- list(header = character(0), seqnames = "chr1")

    result <- echotabix::scanTabix_to_dt(
        header = header,
        queries = queries,
        add_query_names = FALSE,
        verbose = FALSE
    )
    expect_equal(nrow(result), 1)
})

test_that("scanTabix_to_dt applies header column names", {
    queries <- list(
        "chr1:100-500" = c("chr1\t150\trs1", "chr1\t200\trs2")
    )
    header <- list(
        header = "CHR\tPOS\tSNP",
        seqnames = "chr1"
    )

    result <- echotabix::scanTabix_to_dt(
        header = header,
        queries = queries,
        add_query_names = FALSE,
        verbose = FALSE
    )
    expect_true(all(c("CHR", "POS", "SNP") %in% colnames(result)))
})
