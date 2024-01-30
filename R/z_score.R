#' Z score transformation also returning the transformation parameters
#'
#' @param mat Matrix to transform (transforming each column)
#' @param parameters If not `NULL`, project onto the scale defined by those parameters
#'
#' @return A list with two components: the transformed matrix and the parameters
#' @export
#'
transform_zscore <- function(mat, parameters = NULL){
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
