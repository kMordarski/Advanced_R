##############
# Decorators #
##############

rm(list = ls())

##############
# FIRST PART # 
##############

library(pryr)

f <- function(K = 1) {
  r <- c()
  # super inefficient loop
  for (k in 1:K) {
    r <- c(r, rnorm(1)) 
  }
}

`%time_info%` <- function(decorator, func){
  start_time <- Sys.time()
  func
  end_time <- Sys.time()
  
  wydruk <- paste("Time elapsed:", (end_time - start_time)[[1]], "s")
  
  print(wydruk)
}


decorator %time_info%
  f(10)

decorator %time_info%
  f(10^4)

###############
# SECOND PART # 
###############

rm(list = ls())

library(bit)

f <- function(y) {
  print(paste(x, " -- ", y))
}

### definition of the global variable
x <- "global"

g <- function() {
  x <- "local"
  f(y = "call from local (static)")
}

g()

`%dynamic_call%` <- function(decorator, fun){
  
  p <- substitute(fun)
  
  local_f <- get(p[[1]])
  
  environment(local_f) <- parent.frame()
  
  p[[1]] <- quote(local_f)
  
  eval(p, enclos = parent.frame())
}

g <- function() {
  x <- "local"
  decorator %dynamic_call%
    f(y = "call from local (dynamic)")
}

g()

