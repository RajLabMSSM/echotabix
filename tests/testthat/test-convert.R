test_that("convert works", {

    run_tests <- function(dat,
                          convert_methods=eval(formals(echotabix::convert)$convert_methods)
                          ){
        ## Detect position column name
        pos_col <- intersect(c("POS","BP"), colnames(dat))[1]
        if(is.na(pos_col)) testthat::skip("No position column (POS/BP)")
        tmp <- tempfile()
        data.table::fwrite(dat, tmp, sep="\t")
        #### Sorted data ####
        dat_sorted <- data.table::copy(dat)
        data.table::setkeyv(dat_sorted, c("CHR", pos_col))
        data.table::setkey(dat_sorted, NULL)

        tabix_files <- echotabix::convert(target_path = tmp,
                                          start_col = pos_col,
                                          convert_methods = convert_methods) ## <- main func
        testthat::expect_true(file.exists(tabix_files$path))
        testthat::expect_true(file.exists(tabix_files$index))

        dat2 <- echotabix::read_bgz(tabix_files$path,
                                    nrows = 1000)
        #### Return to normal for comparison ####
        if(grepl("chr",dat_sorted$CHR[1])) {
          dat_sorted[,CHR:=as.integer(gsub("chr","",CHR))]
          data.table::setkeyv(dat_sorted, c("CHR", pos_col))
          data.table::setkey(dat_sorted, NULL)
        }
        ## Coerce types for comparison
        if(is.character(dat2$CHR)) dat2[, CHR := as.integer(CHR)]
        if(is.character(dat_sorted$CHR)) dat_sorted[, CHR := as.integer(CHR)]
        testthat::expect_equal(head(dat_sorted,1000), dat2,
                               check.attributes = FALSE)
        ### Clean up ####
        file.remove(unlist(tabix_files))
        file.remove(tmp)
        return(dat2)
    }

    run_tests_method <- function(convert_methods){
      #### Locus subset ####
      dat_locus <- echodata::BST1[1:200,]
      dat2_locus <- run_tests(dat = dat_locus,
                              convert_methods = convert_methods)

      #### fullSS ####
      target_path <- echodata::example_fullSS()
      dat_all <- data.table::fread(target_path)
      ## Standardise position column to POS if needed
      if("BP" %in% colnames(dat_all) && !"POS" %in% colnames(dat_all)){
          data.table::setnames(dat_all, "BP", "POS")
      }
      dat2_all <- run_tests(dat = dat_all,
                            convert_methods = convert_methods)

      #### fullSS with "chr" prefix ####
      dat_all[,CHR:=paste0("chr",CHR)]
      dat3_all <- run_tests(dat = dat_all,
                            convert_methods = convert_methods)
      ### Cleanup ####
      try(file.remove(target_path), silent = TRUE)
    }

    #### ---- convert_methods combo 1 ---- #####
    run_tests_method(convert_methods = list(sort_coordinates="bash",
                                   run_bgzip="Rsamtools",
                                   index="Rsamtools"))

    #### ---- convert_methods combo 2 ---- #####
    run_tests_method(convert_methods = list(sort_coordinates="data.table",
                                   run_bgzip="Rsamtools",
                                   index="Rsamtools"))

    #### ---- convert_methods combo 3 ---- #####
    conda_available <- tryCatch(
        echoconda::env_exists(conda_env = "echoR_mini"),
        error = function(e) FALSE
    )
    if(conda_available){
        run_tests_method(convert_methods = list(sort_coordinates="bash",
                                       run_bgzip="conda",
                                       index="Rsamtools"))
    }

    #### ---- convert_methods combo 4 ---- #####
    if(conda_available){
        run_tests_method(convert_methods = list(sort_coordinates="bash",
                                       run_bgzip="Rsamtools",
                                       index="conda"))
    }

    #### ---- convert_methods combo 5 ---- #####
    if(requireNamespace("seqminer", quietly = TRUE)){
        run_tests_method(convert_methods = list(sort_coordinates="bash",
                                       run_bgzip="Rsamtools",
                                       index="seqminer"))
    }

    #### ---- convert_methods combo 6 ---- #####
    if(requireNamespace("seqminer", quietly = TRUE)){
        run_tests_method(convert_methods = list(sort_coordinates="data.table",
                                       run_bgzip="Rsamtools",
                                       index="seqminer"))
    }


    #### VCF format ####
    ## Verify the pre-indexed VCF can be read directly
    testthat::skip_if_not_installed("VariantAnnotation")
    vcf_path <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
                            package = "echodata")
    vcf <- VariantAnnotation::readVcf(vcf_path)
    testthat::expect_true(methods::is(vcf, "VCF"))
    testthat::expect_equal(nrow(vcf), 100)
    testthat::expect_gte(ncol(vcf), 9)
})
