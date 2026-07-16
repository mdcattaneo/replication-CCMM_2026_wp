################################################################################
# Attention Overload
# Simulation Replication File -- auxiliary functions
# Matias D. Cattaneo, Paul Cheung, Xinwei Ma, Yusufcan Masatlioglu
# HAOM
################################################################################

################################################################################
# load packages and source
################################################################################
library("ramchoice")

################################################################################
# This function generates choice probabilities for the HAOM 
# parameters are
#   indices:  indices of the alternatives in the choice problem
#   alpha:    parameter for logit attention
#   prefDist: distribution of heterogeneous preferences
#             Pr[>_{kj}] is the (k,j)-th entry of the matrix below
#             diagonal and upper triangular parts are not used
################################################################################

choiProbHAOM <- function(indices, alpha, prefDist) {
  mSize <- length(indices) # size of the choice problem
  
  if (mSize == 1) {
    return(list(attnFreq=1, probVec=1)) 
  } else {
    # determine population attention frequency
    # prob of attention on alternatives 1:k
    if (mSize == 2) {
      attnFreq <- rep(1, mSize)
    } else {
      # the zeros are for convenience
      attnFreq <- 1 - cumsum(c(0, 0, (2:(mSize-1))^alpha / sum((2:mSize)^alpha)))
    }
    
    # compute choice probabilities
    # the "1" in the first position is just a place-holder
    probVec <- c(1, rowSums(prefDist[indices[2:mSize], 1:indices[1], drop=FALSE]))
    probVec <- probVec * attnFreq
    probVec <- c(1 - sum(probVec[2:mSize]), probVec[2:mSize])
    return(list(attnFreq=attnFreq, probVec=probVec)) 
  }
}

################################################################################
# This function generates choice probabilities for choice problems of given size
# parameters are
#   uSize:    size of the grand set of alternatives
#   mSize:    size of the choice problem
#   alpha:    parameter for logit attention
#   prefDist: distribution of heterogeneous preferences
#             Pr[>_{kj}] is the (k,j)-th entry of the matrix below
#             diagonal and upper triangular parts are not used
################################################################################

genChoiceProbHAOM <- function(uSize, mSize, alpha, prefDist) {
  # all choice problems with mSize
  allMenus <- t(combn(uSize, mSize))
  
  # initialize
  menu <- prob <- matrix(0, nrow = nrow(allMenus), ncol = uSize)
  
  for (i in 1:nrow(allMenus)) {
    menu[i, allMenus[i, ]] <- 1
    prob_vec <- choiProbHAOM(indices=allMenus[i, ], alpha=alpha, prefDist=prefDist)$probVec
    prob[i, sort(allMenus[i, ])] <- prob_vec
  }
  
  return(list(menu=menu, prob=prob))
}

################################################################################
# This function generates choice data for the HAOM 
# parameters are
#   n:        effective sample size for each choice problem
#   uSize:    size of the grand set of alternatives
#   mSize:    size of the choice problem
#   alpha:    parameter for logit attention
#   prefDist: distribution of heterogeneous preferences
#             Pr[>_{kj}] is the (k,j)-th entry of the matrix below
#             diagonal and upper triangular parts are not used
################################################################################

genDataHAOM <- function(n, uSize, mSize, alpha, prefDist) {
  # all choice problems with mSize
  allMenus <- t(combn(uSize, mSize))
  
  # initialize
  menu <- choice <- matrix(0, nrow = n*nrow(allMenus), ncol = uSize) 
  
  # iteratively generate data
  for (i in 1:nrow(allMenus)) {
    for (j in 1:n) {
      menu[j+(i-1)*n, allMenus[i, ]] <- 1
      prob_vec <- choiProbHAOM(indices = allMenus[i, ], alpha = alpha, prefDist = prefDist)$probVec
      choice[j+(i-1)*n, sort(allMenus[i, ])[rmultinom(1, 1, prob_vec) == 1]] <- 1
    }
  }
  
  return(list(menu=menu, choice=choice))
}

################################################################################
# This function computes lower bounds of the preference distribution in HAOM
# parameters are
#   menu:     matrix of 0s and 1s, the collection of choice problems
#   choice:   matrix of 0s and 1s, the collection of choices
# The output is a matrix: the (k,j)-th entry is the lower bound for theta_{k j} 
#   diagonal and upper triangular parts are not used
################################################################################

revealPrefHAOM <- function(menu, choice) {
  # This function is from the ramchoice package
  # generate summary statistics
  sumStats <- sumData(menu, choice)
  
  # matrix for storing results
  result <- matrix(0, ncol=ncol(menu), nrow=ncol(menu)) 
  
  # enumerate all alternatives
  for (k in 2:nrow(result)) for (j in 1:(k-1)) {
    # find relevant choice problems
    #   containing alternative a_k
    #   and the first element is ranked at or higher than a_j 
    choiProbIndex <- (sumStats$sumMenu[, k] == 1) & (apply(sumStats$sumMenu[, 1:j, drop=FALSE], MARGIN = 1, FUN = sum) > 0)
    # obtain estimated choice probabilities
    choiProbTemp <- sumStats$sumProb[choiProbIndex, k]
    # standard error
    sigmaTemp <- sqrt(choiProbTemp * (1 - choiProbTemp) / sumStats$sumN[choiProbIndex])
    # lower bound with Bonferroni correction
    result[k, j] <- max(c(0, choiProbTemp - sigmaTemp * qnorm(1 - 0.05 / sum(choiProbIndex))))
  }
  
  return(result)
}

################################################################################
# This function computes theoretical lower bounds of the preference distribution 
#   in HAOM
# parameters are
#   menu:     matrix of 0s and 1s, the collection of choice problems
#   prob:     matrix of choice probabilities
# The output is a matrix: the (k,j)-th entry is the lower bound for theta_{k j}  
#   diagonal and upper triangular parts are not used
################################################################################

revealPrefLowerBoundHAOM <- function(menu, prob) {
  # matrix for storing results
  result <- matrix(0, ncol=ncol(menu), nrow=ncol(menu)) 
  
  # find theoretical lower bounds for theta
  for (k in 2:nrow(result)) for (j in 1:(k-1)) {
    # find relevant choice problems
    #   containing alternative a_k
    #   and the first element is ranked at or higher than a_j 
    choiProbIndex <- (menu[, k] == 1) & (apply(menu[, 1:j, drop=FALSE], MARGIN=1, FUN=sum) > 0)
    # obtain choice probabilities
    choiProbTemp <- prob[choiProbIndex, k]
    result[k, j] <- max(c(0, choiProbTemp))
  }
  
  return(result)
}

################################################################################
# This function extracts lower triangular part of a matrix and store them in
#   a vector. This is one by each row
################################################################################

lowerTriToVec <- function(x) {
  result <- c()
  for (i in 2:nrow(x)) {
    result <- c(result, x[i, 1:(i-1)])
  }
  return(result)
} 
