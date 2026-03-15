test_that("granges_to_string converts single range", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 500)
    )
    result <- echotabix::granges_to_string(gr, verbose = FALSE)
    expect_equal(result, "chr1:100-500")
})

test_that("granges_to_string converts multiple ranges", {
    gr <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr2"),
        ranges = IRanges::IRanges(start = c(100, 200), end = c(500, 600))
    )
    result <- echotabix::granges_to_string(gr, verbose = FALSE)
    expect_equal(result, "chr1:100-500,chr2:200-600")
})

test_that("granges_to_string custom separators", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 500)
    )
    result <- echotabix::granges_to_string(
        gr, pos_sep = "..", chrom_sep = "_",
        ranges_sep = ";", verbose = FALSE
    )
    expect_equal(result, "chr1_100..500")
})

test_that("granges_to_string deduplicates ranges", {
    gr <- GenomicRanges::GRanges(
        seqnames = c("chr1", "chr1"),
        ranges = IRanges::IRanges(start = c(100, 100), end = c(500, 500))
    )
    result <- echotabix::granges_to_string(gr, verbose = FALSE)
    # Duplicates should be removed
    expect_equal(result, "chr1:100-500")
})
