test_that("construct_outputs returns single item when only one requested", {
    result <- echotabix:::construct_outputs(
        outputs = "data",
        data = data.frame(x = 1),
        verbose = FALSE
    )
    expect_true(is.data.frame(result))
})

test_that("construct_outputs returns list when multiple requested", {
    result <- echotabix:::construct_outputs(
        outputs = c("command", "data"),
        command = "echo test",
        data = data.frame(x = 1),
        verbose = FALSE
    )
    expect_type(result, "list")
    expect_equal(result$command, "echo test")
    expect_true(is.data.frame(result$data))
    expect_null(result$path)
})

test_that("construct_outputs with all outputs", {
    result <- echotabix:::construct_outputs(
        outputs = c("command", "path", "data"),
        command = "cmd",
        path = "/some/path",
        data = data.frame(a = 1),
        verbose = FALSE
    )
    expect_type(result, "list")
    expect_equal(result$command, "cmd")
    expect_equal(result$path, "/some/path")
    expect_true(is.data.frame(result$data))
})

test_that("construct_outputs with path only", {
    result <- echotabix:::construct_outputs(
        outputs = "path",
        path = "/my/file.tsv",
        verbose = FALSE
    )
    expect_equal(result, "/my/file.tsv")
})
