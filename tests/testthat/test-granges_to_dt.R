test_that("granges_to_dt converts GRanges to data.table", {
    gr <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr2"),
        ranges = IRanges::IRanges(start = c(100, 200), end = c(500, 600))
    )
    result <- echotabix:::granges_to_dt(gr)
    expect_s3_class(result, "data.table")
    expect_equal(nrow(result), 2)
    expect_true("chr" %in% colnames(result))
    expect_true("start" %in% colnames(result))
    expect_true("end" %in% colnames(result))
    expect_equal(result$start, c(100L, 200L))
})

test_that("granges_to_dt with metadata columns (multi-row)", {
    gr <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr2"),
        ranges = IRanges::IRanges(start = c(100, 300), end = c(200, 400))
    )
    GenomicRanges::mcols(gr)$SNP <- c("rs123", "rs456")
    GenomicRanges::mcols(gr)$score <- c(0.5, 0.9)
    result <- echotabix:::granges_to_dt(gr)
    expect_true("SNP" %in% colnames(result))
    expect_true("score" %in% colnames(result))
    expect_equal(result$SNP, c("rs123", "rs456"))
    expect_equal(nrow(result), 2)
})

test_that("granges_to_dt returns NULL for NULL input", {
    expect_null(echotabix:::granges_to_dt(NULL))
})

test_that("granges_to_dt handles no metadata", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr5",
        ranges = IRanges::IRanges(start = 1000, end = 2000)
    )
    result <- echotabix:::granges_to_dt(gr)
    expect_equal(ncol(result), 3)  # chr, start, end only
})
