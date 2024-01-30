
test_that("npn does same as {huge}",{
  
  mat_psi <- create_test_mat_psi()
  mat_sf <- create_test_mat_sf()
  
  expect_identical(transform_npn_shrinkage(mat_psi)$mat,
            huge::huge.npn(mat_psi, verbose = FALSE))
  
  expect_identical(transform_npn_shrinkage(mat_sf)$mat,
            huge::huge.npn(mat_sf, verbose = FALSE))
})


test_that("we can reuse parameters",{
  mat_sf <- create_test_mat_sf()
  
  bb <- transform_npn_shrinkage(mat_sf)
  aa <- transform_npn_shrinkage(mat_sf, bb$parameters)
  expect_identical(bb$mat, aa$mat)
  
  # with na: not an exact match, the huge function gives increasing ranks to each NA, I give them all the same rank
  mat_psi <- create_test_mat_psi()
  
  bb <- transform_npn_shrinkage(mat_psi)
  aa <- transform_npn_shrinkage(mat_psi, bb$parameters)
  
  expect_identical(bb$mat[!is.na(mat_psi)],
                   aa$mat[!is.na(mat_psi)])
  
  # to diagnose:
  # plot(aa$mat,bb$mat, col = c("black", "darkred")[is.na(mat_psi) +1])
})


test_that("we can transform a single row",{
  
  mat_psi_train <- create_test_mat_psi()
  mat_psi_test <- create_test_mat_psi()
  colnames(mat_psi_test) <- colnames(mat_psi_train)
  
  bb <- transform_npn_shrinkage(mat_psi_train)
  expect_no_condition(transform_npn_shrinkage(mat_psi_test[5,,drop=FALSE], bb$parameters)$mat)
  
  # visual check
  # aa <- transform_npn_shrinkage(mat_psi_test[5,,drop=FALSE], bb$parameters)$mat
  # aa[,1:3]
  # 
  # plot(log10(mat_psi_train), bb$mat)
  # points(log10(mat_psi_test[5,]), aa, col = 'red')
})

test_that("our parameters are taken into account",{
  rndm_mat_tests <- matrix(1:3, nrow = 1)
  expect_identical(transform_npn_shrinkage(rndm_mat_tests,
                                    parameters = list(reference_mat = matrix(rep(1:3, 3), nrow = 3),
                                                      sd_first_col = 1))$mat,
            matrix(c(qnorm(1/4), qnorm(2/4), qnorm(3/4)),
                   nrow = 1))
  
  expect_identical(transform_npn_shrinkage(rndm_mat_tests,
                                    parameters = list(reference_mat = matrix(rep(1:3, 3), nrow = 3),
                                                      sd_first_col = 2.3))$mat,
            matrix(c(qnorm(1/4), qnorm(2/4), qnorm(3/4))/2.3,
                   nrow = 1))
  
  
  expect_identical(transform_npn_shrinkage(rndm_mat_tests,
                                    parameters = list(reference_mat = matrix(rep(c(1,1,3), 3), nrow = 3),
                                                      sd_first_col = 1))$mat,
            matrix(c(qnorm(1.5/4), qnorm((1.5+(3-1.5)/2)/4), qnorm(3/4)),
                   nrow = 1))
})



test_that("Reverse rank projection as expected", {
  
  # ties dealt with
  x <- c(10,10,30)
  y <- c(10, 12)
  
  expect_equal(project_rank(y,x) |> rev_project_rank(x),
               y)
  
  # y fully inside x
  x <- runif(100,0,100)
  y <- runif(200, 0.1, 99.9)
  
  y[y <= min(x)] <- (y+min(x))[y <= min(x)]
  y[y >= max(x)] <- (2*max(x)-y)[y >= max(x)]
  
  expect_equal(project_rank(y,x) |> rev_project_rank(x),
               y)
  
  
  # y clipped outside of x
  x <- c(10,15,30)
  y <- c(9, 12, 32)
  
  expect_equal(project_rank(y,x) |> rev_project_rank(x),
               c(10,12,30))
  
})




test_that("Reverse NPN shrinkage reverses", {
  mat_sf <- create_test_mat_sf()
  
  expect_identical(mat_sf |> transform_npn_shrinkage() |> do.call(rev_npn_shrinkage, args = _),
                   mat_sf)
})













