test_that("NPN truncation simple case", {
  
  mat <- matrix(1:6, ncol = 2)
  
  
  expect_equal(huge::huge.npn(mat, npn.func = "truncation", verbose = FALSE),
               transform_npn_truncation(mat)$mat)
  
  mat <- create_test_mat_sf()
  
  expect_equal(huge::huge.npn(mat, npn.func = "truncation", verbose = FALSE),
               transform_npn_truncation(mat)$mat)
})



test_that("NPN truncation threshold parameter used",{
  
  mat <- matrix(1:6, ncol = 2)
  
  
  expect_equal(huge::huge.npn(mat, npn.func = "truncation", verbose = FALSE, npn.thresh = .1),
               transform_npn_truncation(mat, trunc.thresh = .1)$mat)
  
  mat <- create_test_mat_sf()
  
  expect_equal(huge::huge.npn(mat, npn.func = "truncation", verbose = FALSE, npn.thresh = .1),
               transform_npn_truncation(mat, trunc.thresh = .1)$mat)
  
})



test_that("NPN trunc column of 0",{
  mat <- matrix(c(1:6, 0,0,0), ncol = 3)
  
  # since we look at rank
  expect_identical(transform_npn_truncation(mat)$mat[,1],
                   transform_npn_truncation(mat)$mat[,1])
  
  expect_equal(transform_npn_truncation(mat)$mat[,3],
               qnorm(2/3)/0.8498083 |> rep(3),tolerance = 1e-7)
  
})



test_that("NPN trunc handles NA",{
  
  # simple case: NA at the end (highest rank)
  expect_identical(transform_npn_truncation(matrix(c(1,2, NA), ncol = 1))$mat,
                   transform_npn_truncation(matrix(c(1,2, 3), ncol = 1))$mat)
  
  
  expect_identical(transform_npn_truncation(matrix(c(NA,2,3), ncol = 1))$mat,
                   transform_npn_truncation(matrix(c(4, 2,3), ncol = 1))$mat)
  
  
  mat_psi <- create_test_mat_psi()
  
  expect_identical(transform_npn_truncation(mat_psi)$mat,
                   huge::huge.npn(mat_psi, npn.func = "truncation", verbose = FALSE))
  
})




test_that("NPN trunc can reuse parameters",{
  
  # no NA
  mat_sf <- create_test_mat_sf()
  
  bb <- transform_npn_truncation(mat_sf)
  aa <- transform_npn_truncation(mat_sf, parameters = bb$parameters)
  expect_identical(bb$mat, aa$mat)
  
  
  # with na: not an exact match, the huge function gives increasing ranks to each NA, I give them all the same rank
  mat_psi <- create_test_mat_psi()
  
  bb <- transform_npn_truncation(mat_psi)
  aa <- transform_npn_truncation(mat_psi, parameters = bb$parameters)
  
  expect_identical(bb$mat[!is.na(mat_psi)],
                   aa$mat[!is.na(mat_psi)])
  
  # to diagnose:
  # plot(aa$mat,bb$mat, col = c("black", "darkred")[is.na(mat_psi) +1])
})


test_that("NPN trunc can transform a single row",{
  
  mat_psi_train <- create_test_mat_psi()
  mat_psi_test <- create_test_mat_psi()
  colnames(mat_psi_test) <- colnames(mat_psi_train)
  
  bb <- transform_npn_truncation(mat_psi_train)
  expect_no_condition(transform_npn_truncation(mat_psi_test[5,,drop=FALSE],
                                               parameters = bb$parameters)$mat)
  
  # visual check
  # aa <- transform_npn_truncation(mat_psi_test[5,,drop=FALSE], parameters = bb$parameters)$mat
  # aa[,1:3]
  # 
  # plot(log10(mat_psi_train), bb$mat)
  # points(log10(mat_psi_test[5,]), aa, col = 'red')
})

test_that("NPN trunc our parameters are taken into account",{
  rndm_mat_tests <- matrix(1:3, nrow = 1)
  expect_identical(transform_npn_truncation(rndm_mat_tests,
                                            trunc.thresh = 0.1,
                                            parameters = list(
                                              reference_mat = matrix(rep(1:3, 3),
                                                                     nrow = 3),
                                              sd_first_col = 1
                                            ))$mat,
                   matrix(c(qnorm(1/3), qnorm(2/3), qnorm(.9)),
                          nrow = 1))
  
  expect_identical(transform_npn_truncation(rndm_mat_tests,
                                            trunc.thresh = 0.2,
                                           parameters = list(
                                             reference_mat = matrix(rep(1:3, 3), nrow = 3),
                                             sd_first_col = 2.3
                                           ))$mat,
                   matrix(c(qnorm(1/3), qnorm(2/3), qnorm(.8))/2.3,
                          nrow = 1))
  
  
  expect_identical(transform_npn_truncation(rndm_mat_tests,
                                            trunc.thresh = 0.1,
                                            parameters = list(
                                              reference_mat = matrix(rep(c(1,1,3), 3), nrow = 3),
                                              sd_first_col = 1
                                            ))$mat,
                   matrix(c(qnorm(1.5/3), qnorm((1.5+(3-1.5)/2)/3), qnorm(.9)),
                          nrow = 1))
})







