test_that("check_convert_method works", {
  
    opts <- list(
        sort_coordinates=eval(formals(sort_coordinates)$method), 
        run_bgzip=eval(formals(run_bgzip)$method),
        index=eval(formals(index)$method)
    )
    #### No input ####
    method2 <- echotabix:::check_convert_methods()
    testthat::expect_equal(method2, opts)
    
    #### Some input ####
    method3 <- echotabix:::check_convert_methods(
        convert_methods = list(run_bgzip="Rsamtools",
                               index="seqminer"))
    testthat::expect_equal(method3, 
                           list(run_bgzip="rsamtools", ## All lowercase
                                index="seqminer",
                                sort_coordinates="bash"))
    
    #### All input ####
    method3 <- check_convert_methods(
        convert_methods = list(run_bgzip="Rsamtools",
                               index="seqminer"))
    testthat::expect_equal(method3, 
                           list(run_bgzip="rsamtools", ## All lowercase
                                index="seqminer",
                                sort_coordinates="bash"))
})
