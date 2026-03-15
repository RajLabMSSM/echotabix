test_that("report_time prints elapsed time", {
    start <- Sys.time() - 5  # 5 seconds ago
    msg <- capture_messages(
        echotabix:::report_time(start = start, v = TRUE)
    )
    expect_true(length(msg) > 0)
})

test_that("report_time is silent when v=FALSE", {
    start <- Sys.time()
    msg <- capture_messages(
        echotabix:::report_time(start = start, v = FALSE)
    )
    expect_length(msg, 0)
})

test_that("report_time returns elapsed when requested", {
    start <- Sys.time() - 2
    result <- echotabix:::report_time(
        start = start, v = FALSE, return_time = TRUE
    )
    expect_s3_class(result, "difftime")
    expect_true(as.numeric(result, units = "secs") >= 1)
})
