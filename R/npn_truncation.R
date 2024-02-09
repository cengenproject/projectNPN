

transform_npn_truncation <- function(mat, trunc.thresh = NULL, parameters = NULL){
  
  
  if (is.null(trunc.thresh)){
    
    n <- nrow(mat)
    
    trunc.thresh <- 1/(4 * (n^0.25) * sqrt(pi * log(n)))
    
    if(n == 1L) trunc.thresh <- 1/(4 * (n^0.25) * sqrt(pi * log(n+1)))
  }
    
  
  
  
  if(!is.null(parameters)){
    
    params_given <- TRUE
    
    stopifnot(identical( names(parameters),
                         c("reference_mat", "sd_first_col")
    ))
    stopifnot(identical( colnames(mat),
                         colnames(parameters$reference_mat)
    ))
    
  } else{
    params_given <- FALSE
    parameters <- list()
  }
  
  
  if(! params_given){
    
    mat_trans <- apply(mat, 2, rank)
    
    parameters$reference_mat <- mat
    
  } else{
    
    mat_trans <- matrix(nrow = nrow(mat),
                        ncol = ncol(mat))
    for(col in seq_len(ncol(mat))){
      mat_trans[,col] <- project_rank(mat[,col], parameters$reference_mat[,col])
    }
    dimnames(mat_trans) <- dimnames(mat)
    
  }
  
  mat_trans <- mat_trans/nrow(parameters$reference_mat)
  
  mat_trans <- qnorm(pmin(pmax(mat_trans, trunc.thresh), 
                          1 - trunc.thresh))
  
  
  if(! params_given){
    parameters$sd_first_col <- stats::sd(mat_trans[, 1])
  }
  
  mat_trans <- mat_trans/parameters$sd_first_col
  
  
  list(mat = mat_trans, parameters = parameters)
}
