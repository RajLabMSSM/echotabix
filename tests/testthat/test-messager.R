test_that("messager prints when v=TRUE", {
    msg <- capture_messages(echotabix:::messager("hello", "world", v = TRUE))
    expect_true(any(grepl("hello world", msg)))
})

test_that("messager is silent when v=FALSE", {
    msg <- capture_messages(echotabix:::messager("hello", v = FALSE))
    expect_length(msg, 0)
})
