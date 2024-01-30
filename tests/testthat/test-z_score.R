# # test that Zscore does same as scale()
# all.equal(transform_zscore(mat_psi_train)$mat,
#           scale(mat_psi_train, center = TRUE, scale = TRUE),
#           check.attributes = FALSE)
# 
# # test that reverse reverses
# all.equal(mat_psi,
#           mat_psi |> transform_zscore() |> do.call(reverse_transform_zscore, args = _))
# 
# # test that we can reuse parameters
# bb <- transform_zscore(mat_psi_train)
# aa <- transform_zscore(mat_psi_train, bb$parameters)
# all.equal(bb$mat, aa$mat)
# 
# # test that we can transform a single row
# bb <- transform_zscore(mat_psi_train)
# aa <- transform_zscore(mat_psi_test[5,,drop=FALSE], bb$parameters)$mat
# aa
# 
# # test that our parameters are taken into account
# rndm_mat_tests <- matrix(1:3, nrow = 1)
# all.equal(transform_zscore(rndm_mat_tests,
#                            parameters = list(means = 3:1,
#                                              sds = rep(1,3)))$mat,
#           matrix(c(1-3, 2-2, 3-1), nrow = 1))
# 
# all.equal(transform_zscore(rndm_mat_tests,
#                            parameters = list(means = rep(1,3),
#                                              sds = 1:3))$mat,
#           matrix(c((1-1)/1, (2-1)/2, (3-1)/3), nrow = 1))

