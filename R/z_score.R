#' Z score transformation also returning the transformation parameters
#'
#' @section NA handling: 
#' 
#' The `na` argument specifies what to do when there are `NA` in the input matrix.
#' The default is "refuse": the function stops with an error; a suitable way
#' to deal with it is to remove `NA`s (e.g. by imputation) before passing the matrix
#' to the transformation. Other options incluse "keep", so the transformed matrix contains
#' `NA` where the input matrix did, "center" which gives an "average" value to the `NA`s,
#' or "last" which gives the highest possible value to the `NA`s (which is the default
#' behavior in the `{huge}` package).
#' 
#' @param mat Matrix to transform (transforming each column)
#' @param parameters If not `NULL`, project onto the scale defined by those parameters
#' @param na How to treat NAs in input
#'
#' @return A list with two components: the transformed matrix and the parameters
#' @export
#'
transform_zscore <- function(mat, parameters = NULL, na = c("refuse", "keep", "center")){
  
  
  na <- match.arg(na)
  if(na == "refuse" && any(is.na(mat))){
    stop("The matrix contains NA values.")
  }
  
  if(!is.null(parameters)){
    stopifnot(identical(names(parameters),
                        c("means", "sds")))
    params_given <- TRUE
  } else{
    parameters <- list()
    params_given <- FALSE
  }
  
  if(!params_given) parameters$means <- colMeans(mat, na.rm = TRUE)
  mat <- sweep(mat, 2L, parameters$means)
  
  
  if(!params_given) parameters$sds <- matrixStats::colSds(mat, na.rm = TRUE)
  mat <- sweep(mat, 2L, parameters$sds, `/`)
  
  if(na == "center"){
    mat[is.na(mat) | is.infinite(mat)] <- 0
  }
  
  list(mat = mat, parameters = parameters)
}

#' Reverse Z-score transformation
#'
#' @inheritSection transform_zscore NA handling
#'
#' @param mat_trans Transformed matrix
#' @param parameters The transformation parameters
#' @param na How to treat NAs in input
#'
#' @return The original matrix (untransformed)
#' @export
#'
reverse_transform_zscore <- function(mat_trans, parameters = NULL, na = c("refuse", "ignore")){
  
  na <- match.arg(na)
  if(na == "refuse" && any(is.na(mat_trans))){
    stop("The matrix contains NA values.")
  }
  
  stopifnot(identical(names(parameters),
                      c("means", "sds")))
  
  mat <- sweep(mat_trans, 2L, parameters$sds, "*")
  mat <- sweep(mat, 2L, parameters$means, "+")
  
  col_all_NaN <- apply(mat, 2L, \(col) all(is.nan(col)))
  mat[,col_all_NaN] <- 0
  
  mat
}
