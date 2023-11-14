###########################
# Kacper Mordarski 101247 #
###########################

##################
# Final function #
##################

rm(list = ls())

library(stringr)
library(pryr)
library(nloptr)

# Final function

findMinimum <- function(startingPoint, targetFunction, constraints, vars, ...){

  # Function that transforms mathematical language into formulas interpretable by R
  
  transForm <- function(Form, Vars){ # Form and Vars are passed as a quote
    
    form <- deparse(Form)
    
    length_str <- c()
    
    for(i in 2:length(Vars)){
      
      length_str <- c(length_str, str_count(form, deparse(Vars[[i]])))
      
    }
    
    n_i <- max(length_str)
    
    for(k in 1:n_i){
      
      for(m in 1:(length(Vars)-1)){
        
        form <- sub(deparse(Vars[[m + 1]]), paste("x[", m, "]", sep = ""), form)
        
      }
    }
    return(form)
  }
  
  # Function that splits constraints concatenated by & 
  
  ConstrSplit <- function(x){ # x passed as a Quote
    
    if(!grepl("&", deparse(x))){
      
      return(c(x))
      
    }else{
      
      z <- strsplit(deparse(x), split = " & ")[[1]]
      
    }
    #### Returns vector of strings    
  }
  
  # Function that transforms constraints into form interpretable by nloptr package
  
  ReformConstr <- function(X){ # x passed as a vector
    
    NewConstr <- vector(mode = "character", length = length(X))
    
    for(n in 1:length(X)){
      
      if(grepl("<=", X[n])){
        
        NewConstr[n] <- paste(deparse(parse(text = X[n])[[1]][[2]]), " - (", deparse(parse(text = X[n])[[1]][[3]]), ")", sep = "")
        
      }else{
        
        NewConstr[n] <- paste(deparse(parse(text = X[n])[[1]][[3]]), " - (", deparse(parse(text = X[n])[[1]][[2]]), ")", sep = "")
      }
    }
    return(NewConstr) # Returns a vector
  }
  
  # Function that transforms vector of strings into a list of expressions
  
  MakingConstrFun <- function(VecOfChar, vars){
    
    B <- list()
    
    for(k in 1:length(VecOfChar)){
      
      B <- append(B, parse(text = VecOfChar[[k]])[[1]])
    } 
    return(B) ### Spits out list of expressions of constraints
  }
  
  # Function that returns a function handling constraints
  
  fun_creat <- function(b){ #b is a list of expressions
    
    emp <- c()
    
    zwrot <- function(x){}
    
    vector_zwrot <- "return(c("
    
    for(i in 1:length(b)){ # b contains constraints of type language
      
      body(zwrot)[[i+1]] <- parse(text = paste("g", i, " <- ", deparse(b[[i]]), sep = ""))[[1]]
      
      if(i == length(b)){
        
        vector_zwrot <- paste(vector_zwrot, "g", i, sep = "")
        
      }else{
        
        vector_zwrot <- paste(vector_zwrot, "g", i, ", ", sep = "")  
        
      }
    }
    
    vector_zwrot <- paste(vector_zwrot, "))")
    
    body(zwrot)[[length(b)+2]] <- parse(text = vector_zwrot)[[1]]
    
    return(zwrot) #returns a function, which returns vector of constraints
  }
  
  
  ### Capturing expressions of target function, variables and constraints
  
  targetFunctionUneval <- substitute(targetFunction)
  
  varsUneval <- substitute(vars)
  
  constraintsUneval <- substitute(constraints)
  
  ### transforming constraints and target function into form readable by nloptr
  
  VecOfConstrExp <- MakingConstrFun(ReformConstr(
                                      ConstrSplit(
                                        parse(text = transForm(constraintsUneval,
                                                               varsUneval))[[1]])))
  
  obj_form <- transForm(targetFunctionUneval, varsUneval)

  # Performing optimization with nloptr package
  
  res_test <- nloptr( x0 = startingPoint,
                      eval_f = make_function(alist(x = ), parse(text = obj_form)[[1]]),
                      eval_g_ineq = fun_creat(VecOfConstrExp),
                      opts = list("algorithm" = "NLOPT_LN_COBYLA",
                                  "xtol_rel" = 1e-8,
                                  "maxeval" = 1e4))
  
  # Creating list that stores optimization results
  
  e <- list()
  
  e$optimalPoint <- res_test$solution
  
  e$optimalValue <- res_test$objective
  
  return(e)
}

#############
# Example 1 #
#############
  
w <- findMinimum(
  startingPoint = c(5, 7),
  targetFunction = x + y,
  constraints = x >= 0 & y >= 0,
  vars = c(x, y)
)

w

#############
# Example 2 #
#############

w <- findMinimum(
  startingPoint = c(3, 8),
  targetFunction = x^2 + y^2,
  constraints = x + y >= 1,
  vars = c(x, y)
)

w

#############
# Example 3 #
#############

w <- findMinimum(
  startingPoint = c(-2, 0),
  targetFunction = x^4 + y^2,
  constraints = x^2 + y^2 >= 1,
  vars = c(x, y)
)

w



