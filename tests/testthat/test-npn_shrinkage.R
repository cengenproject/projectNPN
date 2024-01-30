
# # test that npn does same as {huge}
# all.equal(transform_npn_shrinkage(mat_psi)$mat,
#           huge::huge.npn(mat_psi, verbose = FALSE))
# 
# all.equal(transform_npn_shrinkage(mat_sf)$mat,
#           huge::huge.npn(mat_sf, verbose = FALSE))
# 
# 
# 
# # test that we can reuse parameters
# bb <- transform_npn_shrinkage(mat_sf_train)
# aa <- transform_npn_shrinkage(mat_sf_train, bb$parameters)
# all.equal(bb$mat, aa$mat)
# # with na: not an exact match, the huge function gives incresing ranks to each NA, I give them all the same rank
# bb <- transform_npn_shrinkage(mat_psi_train)
# aa <- transform_npn_shrinkage(mat_psi_train, bb$parameters)
# all.equal(bb$mat, aa$mat)
# plot(aa$mat,bb$mat, col = c("black", "darkred")[is.na(mat_psi_train) +1])
# 
# 
# 
# # test that we can transform a single row
# bb <- transform_npn_shrinkage(mat_psi_train)
# aa <- transform_npn_shrinkage(mat_psi_test[5,,drop=FALSE], bb$parameters)$mat
# aa[,1:3]
# 
# plot(log10(mat_psi_train), bb$mat)
# points(log10(mat_psi_test[5,]), aa, col = 'red')
# 
# 
# # test that our parameters are taken into account
# rndm_mat_tests <- matrix(1:3, nrow = 1)
# all.equal(transform_npn_shrinkage(rndm_mat_tests,
#                                   parameters = list(reference_mat = matrix(rep(1:3, 3), nrow = 3),
#                                                     sd_first_col = 1))$mat,
#           matrix(c(qnorm(1/4), qnorm(2/4), qnorm(3/4)),
#                  nrow = 1))
# 
# all.equal(transform_npn_shrinkage(rndm_mat_tests,
#                                   parameters = list(reference_mat = matrix(rep(1:3, 3), nrow = 3),
#                                                     sd_first_col = 2.3))$mat,
#           matrix(c(qnorm(1/4), qnorm(2/4), qnorm(3/4))/2.3,
#                  nrow = 1))
# 
# 
# all.equal(transform_npn_shrinkage(rndm_mat_tests,
#                                   parameters = list(reference_mat = matrix(rep(c(1,1,3), 3), nrow = 3),
#                                                     sd_first_col = 1))$mat,
#           matrix(c(qnorm(1.5/4), qnorm((1.5+(3-1.5)/2)/4), qnorm(3/4)),
#                  nrow = 1))



y <- c(10, 12)
x <- c(10,10,30)
x <- runif(100,0,100)
y <- runif(200, -10, 110)

y <- 32

all.equal(project_rank(y,x) |> rev_project_rank(x),
          y)


