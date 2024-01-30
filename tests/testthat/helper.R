

create_test_sample_names <- function(n){
  nb_neuron_types <- floor(n/3)
  nb_char <- sample(2:4, size = nb_neuron_types, prob = c(.05,.75,.2), replace = TRUE)
  
  neur_names <- sapply(nb_char,
         \(k){
           sample(LETTERS, k) |> paste0(collapse = "")
         })
  
  paste0(sample(neur_names, n, replace = TRUE),
         "r",
         sample(1:500, n))
  
}


create_test_event_names <- function(n){
  paste0(sample(c("SE", "CE", "CI"), n, replace = TRUE),
         "_",
         sample(1:500, n))
}

create_test_sf_names <- function(n){
  sapply(1:n,
         \(k){
           sample(c(LETTERS, 0:9), 5) |>
             c(".") |>
             c(sample(0:9, 1)) |>
             c(sample(letters, 1))|>
             paste0(collapse = "")
         })
}


create_test_mat_sf <- function(){
  nb_samples <- 23
  nb_sf <- 363
  
  n <- nb_samples * nb_sf
  nb_zeros <- floor(.15 * n)
  
  # 20% of 0, then combine 2 log-normal LN(11, )
  mat <- matrix(rep(0,nb_zeros) |>
                  c(log1p(rlnorm(n - nb_zeros,11,10))) |>
                  sample() |>
                  round(3),
                nrow = nb_samples, ncol = nb_sf)
  dimnames(mat) <- list(create_test_sample_names(nb_samples),
                        create_test_sf_names(nb_sf))
  
  # at least 1 column all 0 (not first column, would be a problem)
  mat[,2] <- 0
  
  mat
}


# Return a matrix with values between 0 and 1, containing NAs
# for testing purpose
create_test_mat_psi <- function(){
  nb_samples <- 23
  nb_events <- 276
  
  nb_na <- floor(.1 * nb_samples*nb_events)
  
  mat <- matrix(runif(nb_samples * nb_events, 0, 1) |> round(3),
                nrow = nb_samples, ncol = nb_events)
  dimnames(mat) <- list(create_test_sample_names(nb_samples),
                        create_test_event_names(nb_events))
  
  
  pos_na <- expand.grid(i = seq_len(nb_samples),
                        j = seq_len(nb_events))
  
  pos_na <- pos_na[sample(nrow(pos_na), nb_na),]
  
  for(k in 1:nrow(pos_na)){
    
    mat[pos_na$i[k], pos_na$j[k]] <- NA
    
  }
  
  mat
  
}
