
which_smallest_positive <- function(a, tol = 1e-10){
  a <- round(a, -log10(tol))
  if(all(a <= 0 | is.na(a))){
    positive_min <- 0
  } else{
    positive_min <- min(a[a>=0], na.rm = TRUE)
  }
  
  which(a == positive_min)[[1]]
}

transform_one_value <- function(y1, x){
  if(is.na(y1)) return(length(x[!is.na(x)]))
  
  if(y1 < min(x, na.rm = TRUE)){
    right_neighbor <- which_smallest_positive( -(y1 - x))
    rank_x <- rank(x)
    
    return(rank_x[right_neighbor])
  } else if(y1 > max(x, na.rm = TRUE)){
    left_neighbor <- which_smallest_positive(y1 - x)
    rank_x <- rank(x)
    
    return(rank_x[left_neighbor])
  }
  
  left_neighbor <- which_smallest_positive(y1 - x)
  right_neighbor <- which_smallest_positive( -(y1 - x))
  
  rank_x <- rank(x)
  
  if(x[right_neighbor] == x[left_neighbor]) return(rank_x[left_neighbor])
  
  rank_x[left_neighbor] + (y1 - x[left_neighbor])*(rank_x[right_neighbor] - rank_x[left_neighbor])/(x[right_neighbor] - x[left_neighbor])
}

project_rank <- function(vec_to_transform, original_vec){
  purrr::map_dbl(vec_to_transform, transform_one_value, original_vec)
}



#' Transformation NPN by shrunken ECDF
#' 
#' Reimplements `huge::huge.npn(npn.func = "shrinkage")`, but returning the parameters.
#' 
#' Note that, if the new matrix has values outside of the reference range, they will be clipped.
#' 
#' @param mat The matrix to transform.
#' @param parameters If `NULL`, perform the transformation as `huge.npn()`; if not `NULL`, reuse the parameters to project `mat` onto the same scale.
#'
#' @return A list with two elements: `mat_trans` is the transformed matrix, `parameters` is a list of parameters that can be reused.
#' @export
#'
#' 
#' 
#' @examples
#' x <- matrix(c(1, 12, 20.5), ncol = 1)
#' y <- matrix(c(12.2, 18), ncol = 1)
#' x_transformed <- transform_npn_shrinkage(x)
#' y_transformed <- transform_npn_shrinkage(y, x_transformed$parameters)
transform_npn_shrinkage <- function(mat, parameters = NULL){
  
  if(!is.null(parameters)){
    stopifnot(identical( names(parameters),
                         c("reference_mat", "sd_first_col")
    ))
    stopifnot(identical( colnames(mat),
                         colnames(parameters$reference_mat)
    ))
    params_given <- TRUE
  } else{
    parameters <- list()
    params_given <- FALSE
  }
  
  if(! params_given){
    mat_trans <- apply(mat, 2, rank)
    mat_trans <- mat_trans/(nrow(mat_trans) + 1)
  } else{
    mat_trans <- matrix(nrow = nrow(mat),
                        ncol = ncol(mat))
    for(col in seq_len(ncol(mat))){
      mat_trans[,col] <- project_rank(mat[,col], parameters$reference_mat[,col])
    }
    dimnames(mat_trans) <- dimnames(mat)
    mat_trans <- mat_trans/(nrow(parameters$reference_mat) + 1)
    
    # for export
    mat <- parameters$reference_mat
  }
  
  mat_trans <- stats::qnorm(mat_trans)
  
  if(params_given){
    sd_first_col <- parameters$sd_first_col
  } else{
    sd_first_col <- stats::sd(mat_trans[, 1])
  }
  mat_trans = mat_trans/sd_first_col
  
  list(mat = mat_trans, parameters = list(reference_mat = mat, sd_first_col = sd_first_col))
}



# Reverse ----


rev_transform_one_rank <- function(r1, x){
  rank_x <- rank(x)
  
  left_neighbor <- which_smallest_positive(r1 - rank_x)
  right_neighbor <- which_smallest_positive( -(r1 - rank_x))
  
  if(x[right_neighbor] == x[left_neighbor]) return(x[left_neighbor])
  
  x[left_neighbor] + (r1 - rank_x[left_neighbor])*(x[right_neighbor] - x[left_neighbor])/(rank_x[right_neighbor] - rank_x[left_neighbor])
}

rev_project_rank <- function(rk_y, x_reference){
  purrr::map_dbl(rk_y, rev_transform_one_rank, x_reference)
}



#' Reverse NPN (shrunken ECDF) transformation
#'
#' Compute the reverse of the nonparanormal transformation (with shrunken ECDF)
#' Note that, if the matrix to be reverse transformed has values outside of the reference parameters, they will be clipped.
#'
#' @param mat_trans Transformed matrix
#' @param parameters Parameters of the transformation
#'
#' @return The matrix before transformation
#' 
#' 
#' @export
#'
#' @examples
#' 
#' # Reverse a previous transformation
#' my_mat <- matrix(rnorm(6, 10, 1), ncol = 2)
#' transformed_mat <- transform_npn_shrinkage(my_mat)
#' rev_npn_shrinkage(transformed_mat$mat, transformed_mat$parameters)
#' 
#' # Given a matrix in NPN scale, project it back onto original scale
#' y <- matrix(c(-0.5, 0.2), ncol =1)
#' rev_npn_shrinkage(y, transformed_mat$parameters)
rev_npn_shrinkage <- function(mat_trans, parameters = NULL){
  
  
  stopifnot(identical( names(parameters),
                       c("reference_mat", "sd_first_col")
  ))
  stopifnot(identical( colnames(mat_trans),
                       colnames(parameters$reference_mat)
  ))
  
  mat <- parameters$sd_first_col * mat_trans
  mat <- stats::pnorm(mat)
  mat <- mat * (nrow(parameters$reference_mat) + 1)
  
  
  mat2 <- matrix(nrow = nrow(mat_trans),
                      ncol = ncol(mat_trans))
  for(col in seq_len(ncol(mat_trans))){
    mat2[,col] <- rev_project_rank(mat[,col], parameters$reference_mat[,col])
  }
  dimnames(mat2) <- dimnames(mat_trans)
  
  mat2
}

