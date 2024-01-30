
which_smallest_positive <- function(a){
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
  map_dbl(vec_to_transform, transform_one_value, original_vec)
}



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
  }
  
  mat_trans <- qnorm(mat_trans)
  
  if(params_given){
    sd_first_col <- parameters$sd_first_col
  } else{
    sd_first_col <- sd(mat_trans[, 1])
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
  map_dbl(rk_y, rev_transform_one_rank, x_reference)
}

