test_that("filter_table_snps filters by SNP column", {
    query_res <- data.table::data.table(
        SNP = c("rs1", "rs2", "rs3", "rs4"),
        P = c(0.01, 0.05, 0.1, 0.5)
    )
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 500)
    )
    GenomicRanges::mcols(gr)$SNP <- "rs1;rs3"

    result <- echotabix:::filter_table_snps(
        query_res = query_res,
        query_granges = gr,
        verbose = FALSE
    )
    expect_equal(nrow(result), 2)
    expect_true(all(result$SNP %in% c("rs1", "rs3")))
})

test_that("filter_table_snps warns when no SNP in query_res", {
    query_res <- data.table::data.table(
        VARIANT = c("rs1", "rs2"),
        P = c(0.01, 0.05)
    )
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 500)
    )
    GenomicRanges::mcols(gr)$SNP <- "rs1"

    msg <- capture_messages(
        result <- echotabix:::filter_table_snps(
            query_res = query_res,
            query_granges = gr,
            verbose = TRUE
        )
    )
    expect_true(any(grepl("Unable to run overlapping_only", msg)))
    # Returns unfiltered data
    expect_equal(nrow(result), 2)
})

test_that("filter_table_snps warns when no SNP in query_granges", {
    query_res <- data.table::data.table(
        SNP = c("rs1", "rs2"),
        P = c(0.01, 0.05)
    )
    gr <- GenomicRanges::GRanges(
        seqnames = "chr1",
        ranges = IRanges::IRanges(start = 100, end = 500)
    )
    # No SNP mcol set

    msg <- capture_messages(
        result <- echotabix:::filter_table_snps(
            query_res = query_res,
            query_granges = gr,
            verbose = TRUE
        )
    )
    expect_true(any(grepl("Unable to run overlapping_only", msg)))
})
