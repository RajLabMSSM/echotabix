test_that("query_table_rsamtools works", {

    testthat::skip_if_offline()
    testthat::skip_on_cran()
    query_granges <- GenomicRanges::GRanges(c("2:15000-16000","3:60000-61000"))
    # meta <- (phenomix::opengwas_meta()|>tail(3))[1]
    target_path <- "https://phenomix.dsi.ic.ac.uk/MAGMA_Files_Public/data/GWAS_munged//ubm-a-2712/ubm-a-2712.tsv.bgz"
    ## Skip if the remote server is unreachable
    testthat::skip_if(
        inherits(tryCatch(
            Rsamtools::headerTabix(
                Rsamtools::TabixFile(target_path)
            ),
            error = function(e) e
        ), "error"),
        message = "Remote tabix file is not accessible"
    )
    query_dt <- echotabix:::query_table_rsamtools(target_path=target_path,
                                                  query_granges=query_granges)
    testthat::expect_equal(nrow(query_dt),14)

})
