test_that("construct_tabix_path adds .bgz suffix to plain file", {
    result <- echotabix::construct_tabix_path(target_path = "mydata.tsv")
    expect_true(endsWith(result, ".bgz"))
})

test_that("construct_tabix_path replaces .gz with .bgz", {
    result <- echotabix::construct_tabix_path(target_path = "mydata.tsv.gz")
    expect_true(endsWith(result, ".bgz"))
    expect_false(grepl("\\.gz\\.bgz$", result))
})

test_that("construct_tabix_path with vcf input", {
    result <- echotabix::construct_tabix_path(
        target_path = "mysumstatsfile.vcf.tsv.gz"
    )
    expect_true(endsWith(result, ".bgz"))
})

test_that("construct_tabix_path uses study_dir when provided", {
    result <- echotabix::construct_tabix_path(
        target_path = "/original/dir/data.tsv.gz",
        study_dir = "/new/study/dir"
    )
    expect_true(startsWith(result, "/new/study/dir"))
})

test_that("construct_tabix_path preserves directory when no study_dir", {
    result <- echotabix::construct_tabix_path(
        target_path = "/some/path/data.tsv"
    )
    expect_true(startsWith(result, "/some/path"))
})
