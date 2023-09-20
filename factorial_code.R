library(purrr)
library(microbenchmark)
library(ggplot2)

# 1. Factorial using for loop
Factorial_loop <- function(n){
  stopifnot(n >= 0)
  if(n==0){
  1
  }
  else if(n>=1){
    fact <- 1
    for(i in 1:n){
     fact <- fact * i
    }
    return(fact)
  }
}

# 2. Factorial using reduce()
Factorial_reduce <- function(n) {
  stopifnot(n >= 0)
  if(n==0){
    1
  }
  else if(n>=1){
  reduce(c(1:n), function(x,y){
    x*y
  })
    }
}

# 3. Factorial using recursion
Factorial_func <- function(n){
  stopifnot(n >= 0)
  if(n == 0){
    1
  } else {
    n * Factorial_func(n-1)
  }
}

# 4. Factorial using memoization
factTab <- c(1, rep(NA, 25))

Factorial_mem <- function(n){
  stopifnot(n >= 0)
  if(n == 0){
    1
  } else {
    if(!is.na(factTab[n])){
    factTab[n]
  } else {
      factTab[n-1] <<- Factorial_mem(n-1)
      n * factTab[n-1]
  }
  }
}

# To test speed performances, we use 'microbenchmark' for a sample of n = c(5,7,10)
testPerf <- function(n){
  test <- summary(microbenchmark(Factorial_loop(n), 
                                 Factorial_reduce(n),
                                 Factorial_func(n),
                                 Factorial_mem(n)))
  print(test)
  graph <- autoplot(microbenchmark(Factorial_loop(n), 
                                   Factorial_reduce(n),
                                   Factorial_func(n),
                                   Factorial_mem(n)))
  print(graph)
}


for(i in 2:4){
  print(testPerf(i))
}
