library(sampling)
library(tidyverse)

# n <- 24
N <- 1e4

# Warning: this code is very slow and inefficient

min_sample_clue <- function(n) {
  sims_clue <- data.frame(nrow=n, ncol=6)
  
  test <- map(1:n, function(x) tibble(srswor(1, 6)) )
  test <- t(bind_cols(test))
  
  return(min(colSums(test)))
}

gumbel_clue <- function(n,N) {
  return(round(mean(map_dbl(rep(n,N), min_sample_clue)),0))
}

# param x vector values for which probability of min
# should be returned
dcluegumbel_onen <- function(x,n,N) {
  min_distr <- map_dbl(rep(n,N), min_sample_clue)
  min_distr <- table(min_distr) / N
  
  return_vec <- rep(0,length(x))
  
  for(k in 1:length(x)) {
    temp_val_distr <- min_distr[as.character(x[k])]
    if(!is.na(temp_val_distr)) {
      return_vec[k] <- temp_val_distr
    } else {
      return_vec[k] <- 0
    }

  }
  
  return(return_vec)
}

# param x numeric value for which probability of min
# should be returned
dcluegumbel_onen_onex <- function(x,n,N) {
  min_distr <- map_dbl(rep(n,N), min_sample_clue)
  min_distr <- table(min_distr) / N
  
  return_val <- min_distr[as.character(x)]
  return_val <- return_val[[1]]
  
  if(is.na(return_val)) {
    return(0)
  } else {
    return(return_val[[1]])
  }
  
  return(0)
}


# Outputs a matrix of values of likelihood for values
# of n (in rows). in columns: prob of min having value k
dcluegumbel_gentable <- function(kVec, nVec, N) {
  # TODO
  
  return_table <- matrix(0, nrow=length(nVec), ncol=length(kVec))
  
  for(k in 1:length(nVec)) {
    print(k)
    return_table[k, ] <- dcluegumbel_onen(kVec, nVec[k], N)
  }
  
  return_table <- as.tibble(return_table)
  names(return_table) <- as.character(kVec)
  
  return(return_table)
}

# testVec <- dcluegumbel(1:10, 60, 1000)
testVec
# 
# testMat <- dcluegumbel_gentable(0:10, 0:2000, 1000)
# testMat

k_min <- 2

testMat <- dcluegumbel_gentable(k_min, 0:2000, 1000)
testMat

testVec <- rep(0,80)

for(k in 3:80) {
  print(k)
  val <- dcluegumbel_onen_onex(k_min, k, 1000)
  testVec[k] <- val
  print(val)
}

gumbelclue_2 <- c(testVec, rep(0, 1920))

# saveRDS(gumbelclue_2, file="clue/dcluegumbel_2.rds")