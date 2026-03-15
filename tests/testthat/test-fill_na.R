test_that("fill_na replaces NAs with 0", {
    mat <- matrix(c(1, NA, NA, 1), nrow = 2,
                  dimnames = list(c("rs1", "rs2"), c("rs1", "rs2")))
    result <- echotabix:::fill_na(mat, fillNA = 0, verbose = FALSE)
    expect_equal(sum(is.na(result)), 0)
    expect_equal(result["rs1", "rs2"], 0)
})

test_that("fill_na removes unnamed rows/cols", {
    mat <- matrix(c(1, 0.5, 0.3, 0.5, 1, 0.2, 0.3, 0.2, 1),
                  nrow = 3,
                  dimnames = list(c("rs1", ".", "rs2"),
                                 c("rs1", ".", "rs2")))
    result <- echotabix:::fill_na(mat, verbose = FALSE)
    expect_false("." %in% rownames(result))
    expect_false("." %in% colnames(result))
})

test_that("fill_na removes duplicate SNPs", {
    mat <- matrix(c(1, 0.5, 1, 0.5, 1, 0.5, 1, 0.5, 1),
                  nrow = 3,
                  dimnames = list(c("rs1", "rs1", "rs2"),
                                 c("rs1", "rs1", "rs2")))
    result <- echotabix:::fill_na(mat, verbose = FALSE)
    expect_false(any(duplicated(rownames(result))))
    expect_false(any(duplicated(colnames(result))))
})

test_that("fill_na with fillNA=NULL preserves NAs", {
    mat <- matrix(c(1, NA, NA, 1), nrow = 2,
                  dimnames = list(c("rs1", "rs2"), c("rs1", "rs2")))
    result <- echotabix:::fill_na(mat, fillNA = NULL, verbose = FALSE)
    expect_equal(sum(is.na(result)), 2)
})
