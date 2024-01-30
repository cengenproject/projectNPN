

# NPN shrinkage ----







## Tests ----









rk <- project_rank(y,x)

x
rk

rk * diff(range(x)) + min(x)

r1 <- rk[[2]]







mat_trans <- matrix(nrow = nrow(mat),
                    ncol = ncol(mat))
for(col in seq_len(ncol(mat))){
  mat_trans[,col] <- project_rank(mat[,col], parameters$reference_mat[,col])
}
dimnames(mat_trans) <- dimnames(mat)
mat_trans <- mat_trans/(nrow(parameters$reference_mat) + 1)


mat_trans <- qnorm(mat_trans)

sd_first_col <- parameters$sd_first_col

mat_trans = mat_trans/sd_first_col





# ~~~~~~~~~~~ ----





# Z-score ----




##  Tests ----





