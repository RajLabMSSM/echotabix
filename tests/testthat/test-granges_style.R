test_that("granges_style converts to UCSC style", {
    gr <- GenomicRanges::GRanges(
        seqnames = "1",
        ranges = IRanges::IRanges(start = 100, end = 500)
    )
    result <- echotabix:::granges_style(gr, style = "UCSC")
    seqname <- as.character(GenomicRanges::seqnames(result))
    expect_equal(seqname, "chr1")
})

test_that("granges_style converts to NCBI style", {
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 500)
    )
    result <- echotabix:::granges_style(gr, style = "NCBI")
    seqname <- as.character(GenomicRanges::seqnames(result))
    expect_equal(seqname, "1")
})
