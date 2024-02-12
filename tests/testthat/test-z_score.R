test_that("Zscore basic example",{
  
  mat_psi <- matrix(c(1, 2, 3,
                      1.5, 2, 2.5), ncol = 2)
  
  expect_equal(transform_zscore(mat_psi)$mat,
               matrix(c(-1,0,1) |> rep(2), ncol = 2))
})

test_that("Zscore column of 0",{
  mat <- matrix(c(1,   2,  3,
                  1.5, 2, 2.5,
                  0,   0,  0), ncol = 3)
  
  # not filling NA: we keep the NaNs
  expect_identical(transform_zscore(mat)$mat,
                   matrix(c(-1,0,1,
                            -1,0,1,
                            NaN, NaN, NaN), ncol = 3))
  
  expect_identical(transform_zscore(mat, na = "center")$mat,
                   matrix(c(-1,0,1,
                            -1,0,1,
                            0,0,0), ncol = 3))
})


test_that("Zscore does same as scale()",{
  
  mat_psi <- create_test_mat_psi()
  
  expect_equal(transform_zscore(mat_psi, na = "keep")$mat,
               scale(mat_psi, center = TRUE, scale = TRUE),
               ignore_attr = TRUE)
})

test_that("Zscore handles NAs",{
  
  mat_psi <- matrix(c(1, 2, 3, NA,
                      1.5, NA, 2.5, 2), ncol = 2)
  
  expect_equal(transform_zscore(mat_psi, na = "keep")$mat,
               matrix(c(-1,0,1,NA,
                        -1,NA,1,0), ncol = 2))
  
  expect_equal(transform_zscore(mat_psi, na = "center")$mat,
               matrix(c(-1,0,1,0,
                        -1,0,1,0), ncol = 2))
})

test_that("Zscore reverse reverses",{
  mat_psi <- create_test_mat_psi()
  
  trans <- transform_zscore(mat_psi, na = "keep")
  expect_equal(mat_psi,
               reverse_transform_zscore(mat_trans = trans$mat,
                                        parameters = trans$parameters,
                                        na = "ignore"))
  
  
  # filling NAs should be equivalent to replacing by the column mean
  mat_psi_imputed <- apply(mat_psi, 2L, \(x) {x[is.na(x)] <- mean(x, na.rm = TRUE); x})
  
  expect_equal(mat_psi_imputed,
               mat_psi |> transform_zscore(na = "center") |> do.call(reverse_transform_zscore, args = _))
  
})

test_that("we can reuse parameters", {
  
  mat_psi <- create_test_mat_psi()
  
  bb <- transform_zscore(mat_psi, na = "keep")
  aa <- transform_zscore(mat_psi, bb$parameters, na = "keep")
  
  expect_equal(bb$mat, aa$mat)

  bb <- transform_zscore(mat_psi, na = "center")
  aa <- transform_zscore(mat_psi, bb$parameters, na = "center")
  
  expect_equal(bb$mat, aa$mat)
})



test_that("we can transform a single row",{
  mat_psi <- create_test_mat_psi()
  mat_psi_test <- create_test_mat_psi()
  
  bb <- transform_zscore(mat_psi, na = "keep")
  expect_no_condition(transform_zscore(mat_psi_test[5,,drop=FALSE],
                                       bb$parameters,
                                       na = "keep")$mat)
  
})


test_that("our parameters are taken into account",{
  rndm_mat_tests <- matrix(1:3, nrow = 1)
  expect_equal(transform_zscore(rndm_mat_tests,
                                parameters = list(means = 3:1,
                                                  sds = rep(1,3)))$mat,
               matrix(c(1-3, 2-2, 3-1), nrow = 1))
  
  expect_equal(transform_zscore(rndm_mat_tests,
                                parameters = list(means = rep(1,3),
                                                  sds = 1:3))$mat,
               matrix(c((1-1)/1, (2-1)/2, (3-1)/3), nrow = 1))
})


