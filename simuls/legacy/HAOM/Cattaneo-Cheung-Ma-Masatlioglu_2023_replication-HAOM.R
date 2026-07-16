################################################################################
# Attention Overload
# Simulation Replication File
# Matias D. Cattaneo, Paul Cheung, Xinwei Ma, Yusufcan Masatlioglu
# HAOM
################################################################################

################################################################################
# load packages and source
################################################################################

# The R code provided relies on the package "ramchoice", which is available on
#   CRAN. Run the following if it is not already installed.
#
#                 install.packages("ramchoice")

library("ramchoice")
source("Cattaneo-Cheung-Ma-Masatlioglu_2023_replication-HAOM--auxiliary.R")

################################################################################
# set seed
################################################################################

set.seed(42)

################################################################################
# simulation parameters
################################################################################

# size of the grand set 
uSize <- 6

# parameter for logit attention
alpha <- 2 # alpha parameter in the logit attention specification

# distribution of heterogeneous preference
# Pr[>_{kj}] is the (k,j)-th entry of the matrix below
# diagonal and upper triangular parts are not used
prefDist <- matrix(0.05, ncol=6, nrow=6)

# possible choice problems in simulated data
choiProblemsList <- list(6:2, 
                         6:3, 
                         6:4, 
                         6:5, 
                         c(6, 4:2), 
                         c(6, 3, 2), 
                         c(6, 2))

# change this number from 1-7 to generate the panels
simuIndex <- 1
choiProblems <- choiProblemsList[[simuIndex]]

# effective sample size
nList <- c(50, 100, 200)

# Monte Carlo repetitions
# takes about 1 hour
repe <- 2000

################################################################################
# simulation
################################################################################

Result <- matrix(NA, nrow = repe, ncol = uSize^2 * length(nList))

ptm <- proc.time()
for (i in 1:repe) {
  for (nIndex in 1:length(nList)) {
    n <- nList[nIndex]
    
    # generate data
    menu <- choice <- matrix(0, nrow=0, ncol=uSize)
    for (j in choiProblems) {
      temp <- genDataHAOM(n, uSize, j, alpha, prefDist)
      menu <- rbind(menu, temp$menu); choice <- rbind(choice, temp$choice)
    }
    
    # computing lower bounds
    temp <- revealPrefHAOM(menu, choice)
    
    # vec() the matrix into a long vector and save the results
    Result[i,  (nIndex-1)*36+(1:36)] <- c(temp)
  }
}

################################################################################
# output to csv file
################################################################################

fname <- paste("Result-", simuIndex, "-", ".txt", sep="")
write.table(Result, file=fname, sep=",", row.names=FALSE, col.names=FALSE)
proc.time() - ptm
