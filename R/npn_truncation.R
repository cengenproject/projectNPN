

#' Transformation NPN by truncation
#' 
#' Reimplements `huge::huge.npn(npn.func = "truncation")`, but returning the parameters. Note that, if the new matrix has values close to limits of the reference range, they will be clipped.
#'
#'
#' @inheritSection transform_zscore NA handling
#' 
#' @param mat The matrix to transform.
#' @param trunc.thresh The threshold.
#' @param parameters If `NULL`, perform the transformation as `huge.npn()`; if not `NULL`, reuse the parameters to project `mat` onto the same scale.
#' @param na How to treat NAs in input
#'
#' @return A list with two elements: `mat` is the transformed matrix, `parameters` is a list of parameters that can be reused.
#' @export
#'
#' @examples
#' x <- matrix(c(1, 12, 20.5), ncol = 1)
#' y <- matrix(c(12.2, 18), ncol = 1)
#' x_transformed <- transform_npn_shrinkage(x)
#' y_transformed <- transform_npn_shrinkage(y, x_transformed$parameters)
transform_npn_truncation <- function(mat, parameters = NULL, trunc.thresh = NULL, na = c("refuse", "last")){
  
  na <- match.arg(na)
  if(na == "refuse" && any(is.na(mat))){
    stop("The matrix contains NA values.")
  }
  
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
  
  mat_trans <- stats::qnorm(pmin(pmax(mat_trans, trunc.thresh), 
                                 1 - trunc.thresh))
  
  
  if(! params_given){
    parameters$sd_first_col <- stats::sd(mat_trans[, 1])
  }
  
  mat_trans <- mat_trans/parameters$sd_first_col
  
  
  list(mat = mat_trans, parameters = parameters)
}






#' Reverse NPN (truncation) transformation
#'
#' Compute the reverse of the nonparanormal transformation (with truncation)
#' Note that, if the matrix to be reverse transformed has close to the limits of the reference parameters, they will be clipped.
#'
#'
#' @inheritSection transform_zscore NA handling
#' 
#' @param mat_trans Transformed matrix
#' @param parameters Parameters of the transformation
#' @param na How to treat NAs in input
#'
#' @return The matrix before transformation
#' @export
#'
#' @examples
#' # Reverse a previous transformation
#' my_mat <- matrix(rnorm(6, 10, 1), ncol = 2)
#' transformed_mat <- transform_npn_shrinkage(my_mat)
#' reverse_npn_truncation(transformed_mat$mat, transformed_mat$parameters)
#' 
#' # Given a matrix in NPN scale, project it back onto original scale
#' y <- matrix(c(-1, 1), ncol = 1)
#' reverse_npn_truncation(y, transformed_mat$parameters)
reverse_npn_truncation <- function(mat_trans, parameters = NULL, na = "refuse"){
  
  na <- match.arg(na)
  if(na == "refuse" && any(is.na(mat_trans))){
    stop("The matrix contains NA values.")
  }
  
  stopifnot(identical( names(parameters),
                       c("reference_mat", "sd_first_col")
  ))
  stopifnot(identical( colnames(mat_trans),
                       colnames(parameters$reference_mat)
  ))
  
  mat <- parameters$sd_first_col * mat_trans
  mat <- stats::pnorm(mat)
  mat <- mat * nrow(parameters$reference_mat)
  
  
  mat2 <- matrix(nrow = nrow(mat_trans),
                 ncol = ncol(mat_trans))
  for(col in seq_len(ncol(mat_trans))){
    mat2[,col] <- rev_project_rank(mat[,col], parameters$reference_mat[,col])
  }
  dimnames(mat2) <- dimnames(mat_trans)
  
  mat2
}








