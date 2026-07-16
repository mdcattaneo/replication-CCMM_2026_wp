################################################################################
# Attention Overload
# Simulation Replication File -- auxiliary functions
# Matias D. Cattaneo, Paul Cheung, Xinwei Ma, Yusufcan Masatlioglu
# AOM
################################################################################

################################################################################
# This function generates choice data for the HAOM 
# parameters are
#   n:        effective sample size for each choice problem
#   uSize:    size of the grand set of alternatives
#   mSize:    size of the choice problem
#   alpha:    parameter for logit attention
################################################################################

genDataAOM <- function(n, uSize, mSize, alpha) {
  # determine population choice rule
  prob_vec <- rep(0, mSize)
  for (i in 1:mSize) { # enumerate over alternatives
    for (j in 1:(mSize-i+1)) { # enumerate over consideration set size
      temp1 <- factorial(mSize-i) / factorial(j-1) / factorial(mSize-i-j+1)
      temp2 <- j^alpha / 
        sum((1:mSize)^alpha * factorial(mSize) / factorial(1:mSize) / factorial((mSize-1):0))
      prob_vec[i] = prob_vec[i] + temp1 * temp2
    }
  }
  
  # initialize
  allMenus <- t(combn(uSize, mSize))
  menu <- choice <- matrix(0, nrow=n*nrow(allMenus), ncol=uSize) 
  
  for (i in 1:nrow(allMenus)) {
    for (j in 1:n) {
      menu[j+(i-1)*n, allMenus[i, ]] <- 1
      choice[j+(i-1)*n, sort(allMenus[i, ])[rmultinom(1, 1, prob_vec) == 1]] <- 1
    }
  }
  
  return(list(menu=menu, choice=choice))
}