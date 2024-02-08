#' Z score transformation also returning the transformation parameters
#'
#' @param mat Matrix to transform (transforming each column)
#' @param parameters If not `NULL`, project onto the scale defined by those parameters
#' @param fill_na if FALSE, NAs in input stay NA in output without affecting the other values. If TRUE, NAs are replaced by 0 (i.e. the column mean).
#'
#' @return A list with two components: the transformed matrix and the parameters
#' @export
#'
transform_zscore <- function(mat, parameters = NULL, fill_na = FALSE){
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
  
  if(fill_na) mat[is.na(mat)] <- 0
  
  list(mat = mat, parameters = parameters)
}

#' Reverse Z-score transformation
#'
#' @param mat_trans Transformed matrix
#' @param parameters The transformation parameters
#'
#' @return The original matrix (untransformed)
#' @export
#'
reverse_transform_zscore <- function(mat_trans, parameters = NULL){
  stopifnot(identical(names(parameters),
                      c("means", "sds")))
  
  mat <- sweep(mat_trans, 2L, parameters$sds, "*")
  mat <- sweep(mat, 2L, parameters$means, "+")
  
  col_all_NaN <- apply(mat, 2L, \(col) all(is.nan(col)))
  mat[,col_all_NaN] <- 0
  
  mat
}
