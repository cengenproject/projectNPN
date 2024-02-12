test_that("NPN truncation simple case", {
  
  mat <- matrix(1:6, ncol = 2)
  
  
  expect_equal(huge::huge.npn(mat, npn.func = "truncation", verbose = FALSE),
               transform_npn_truncation(mat)$mat)
  
  mat <- create_test_mat_sf()
  
  expect_equal(huge::huge.npn(mat, npn.func = "truncation", verbose = FALSE),
               transform_npn_truncation(mat)$mat)
})


test_that("NPN truncation keeps names", {
  
  mat <- matrix(1:6, ncol = 2,
                dimnames = list(letters[1:3],
                                LETTERS[1:2]))
  
  expect_equal(dimnames(mat),
               dimnames(transform_npn_truncation(mat)$mat))
  
  
  mat <- create_test_mat_sf()
  
  expect_equal(dimnames(mat),
               dimnames(transform_npn_truncation(mat)$mat))
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


test_that("NPN trunc handles NA when 'refuse",{
  
  # Default: refuse NA
  expect_error(transform_npn_truncation(matrix(c(1,2, NA), ncol = 1)),
               "The matrix contains NA values.")
  
})

test_that("NPN trunc handles NA",{
  
  
  # simple case: NA at the end (highest rank)
  expect_identical(transform_npn_truncation(matrix(c(1,2, NA), ncol = 1),
                                            na = "last")$mat,
                   transform_npn_truncation(matrix(c(1,2, 3), ncol = 1),
                                            na = "last")$mat)
  
  
  expect_identical(transform_npn_truncation(matrix(c(NA,2,3), ncol = 1),
                                            na = "last")$mat,
                   transform_npn_truncation(matrix(c(4, 2,3), ncol = 1),
                                            na = "last")$mat)
  
  
  mat_psi <- create_test_mat_psi()
  
  # Whith NA last, same behavior as default huge NPN
  expect_identical(transform_npn_truncation(mat_psi,
                                            na = "last")$mat,
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
  
  bb <- transform_npn_truncation(mat_psi,
                                 na = "last")
  aa <- transform_npn_truncation(mat_psi,
                                 parameters = bb$parameters,
                                 na = "last")
  
  expect_identical(bb$mat[!is.na(mat_psi)],
                   aa$mat[!is.na(mat_psi)])
  
  # to diagnose:
  # plot(aa$mat,bb$mat, col = c("black", "darkred")[is.na(mat_psi) +1])
})


test_that("NPN trunc can transform a single row",{
  
  mat_psi_train <- create_test_mat_psi()
  mat_psi_test <- create_test_mat_psi()
  colnames(mat_psi_test) <- colnames(mat_psi_train)
  
  bb <- transform_npn_truncation(mat_psi_train,
                                 na = "last")
  expect_no_condition(transform_npn_truncation(mat_psi_test[5,,drop=FALSE],
                                               parameters = bb$parameters,
                                               na = "last")$mat)
  
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




test_that("Reverse NPN trunc reverses", {
  
  # Note: because of the truncation, the highest value can never be exactly recovered.
  # A workaround would be without truncation (threshold = 0), but since we're dividing
  # by nrow (and not nrow+1 as in shrinkage), the highest rank stays at 1, its qnorm is Inf,
  # and the sd_first_column is NaN
  # 
  # So here we test by removing the top value
  
  # in this example, the top value is on the last row
  mat <- matrix(1:6 * 1., ncol = 2,
                dimnames = list(letters[1:3],
                                LETTERS[1:2]))
  
  expect_identical((mat |>
                      transform_npn_truncation() |>
                      do.call(reverse_npn_truncation, args = _))[1:2,],
                   mat[1:2,])
  
  
  
  # for this example, we add a new highest value on the first row
  mat_sf <- create_test_mat_sf()
  
  mat_sf <- rbind(apply(mat_sf, 2L, max) + 1,
                  mat_sf)
  
  
  
  expect_identical((mat_sf |>
                      transform_npn_truncation() |>
                      do.call(reverse_npn_truncation, args = _))[-1,],
                   mat_sf[-1,])
})


test_that("Reverse NPN trunc refuses NA",{
  mat <- matrix(c(NA,2:6 * 1.), ncol = 2,
                dimnames = list(letters[1:3],
                                LETTERS[1:2]))
  
  
  
  expect_error(reverse_npn_truncation(mat),
               "The matrix contains NA values.")
  
})



test_that("Reverse NPN trunc projects new values", {
  
  # reference vector, ensure the first and last values are the only ones truncated
  vec_ref <- runif(100, -50, 50) |>
    c(rep(-100, 10), rep(100, 10)) |>
    sort()
  mat_ref <- matrix(vec_ref, ncol = 1)
  
  trans <- transform_npn_truncation(mat_ref)
  
  # new data: intercalate between reference values
  vec_trans <- trans$mat[11:109,1]
  vec_intercalated <- vec_trans[-length(vec_trans)] + diff(vec_trans)/2
  new_data_intercalated <- matrix(vec_intercalated, ncol = 1)
  
  raw_intercalated <- reverse_npn_truncation(new_data_intercalated,
                                         parameters = trans$parameters)
  
  # intercalated and reverse transformed should be in order
  expect_identical(order(raw_intercalated),
                   seq_along(raw_intercalated))
  
  # should still be intercalated
  expect_true( all(vec_intercalated < vec_trans[-1]) )
  expect_true( all(vec_intercalated > vec_trans[-length(vec_trans)]) )
  
})




