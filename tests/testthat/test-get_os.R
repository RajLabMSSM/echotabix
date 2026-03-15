test_that("get_os returns valid OS string", {
    os <- echotabix:::get_os()
    expect_true(os %in% c("Windows", "Linux", "Mac"))
    expect_true(nchar(os) > 0)
})
