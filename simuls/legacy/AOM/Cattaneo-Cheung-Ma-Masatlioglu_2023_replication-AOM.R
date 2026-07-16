################################################################################
# Attention Overload
# Simulation Replication File
# Matias D. Cattaneo, Paul Cheung, Xinwei Ma, Yusufcan Masatlioglu
# AOM
################################################################################

################################################################################
# load package
################################################################################

# The R code provided relies on the package "ramchoice", which is available on
#   CRAN. Run the following if it is not already installed.
#
#                 install.packages("ramchoice")

library("ramchoice")
source("Cattaneo-Cheung-Ma-Masatlioglu_2023_replication-AOM--auxiliary.R")

################################################################################
# set seed
################################################################################

set.seed(42)

################################################################################
# varying parameters
################################################################################

# effective sample size
nList <- c(50, 100, 200)

alpha <- 2 # the parameter in the logit attention rule specification

# list of preferences
prefList <- matrix(c(1, 2, 3, 4, 5, 6, 
                     2, 3, 4, 5, 6, 1, 
                     1, 2, 6, 5, 4, 3, 
                     1, 6, 5, 4, 3, 2  
                     ), ncol=6, byrow=T)

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

################################################################################
# additional pars
################################################################################

repe   <- 2000 # number of Monte Carlo repetitions
repeSS <- 2000 # number of simulations for critical values

################################################################################
# simulation
################################################################################

Result <- matrix(NA, nrow = repe, ncol = nrow(prefList) * length(nList))

ptm <- proc.time()
for (i in 1:repe) {
  for (nIndex in 1:length(nList)) {
    n <- nList[nIndex]
    # generate data
    menu <- choice <- matrix(0, nrow=0, ncol=6)
    for (j in choiProblems) {
      temp <- genDataAOM(n, 6, j, alpha)
      menu <- rbind(menu, temp$menu); choice <- rbind(choice, temp$choice)
    }
    
    # testing
    temp <- rAtte(menu, choice, pref_list = prefList, method = "GMS",
                  nCritSimu = repeSS,
                  AOM = TRUE, 
                  RAM = FALSE, 
                  attBinary = 1)
    Result[i, (nIndex-1)*nrow(prefList) + (1:nrow(prefList)) ] <- (temp$critVal$GMS[, 2] < temp$Tstat) * 1
  }
}

################################################################################
# output
################################################################################

fname <- paste("Result-", simuIndex, "-", ".txt", sep="")
write.table(Result, file=fname, sep=",", row.names=FALSE, col.names=FALSE)
proc.time() - ptm

################################################################################
# load simulation results, and report
################################################################################

round(matrix(colMeans(read.table(paste("Result-", simuIndex, "-", ".txt", sep=""), sep=",")), nrow=4), 3)

# output for simuIndex <- 1
# pref          n=50    n=100   n=200
# 1>2>3>4>5>6   0.014   0.006   0.004
# 2>3>4>5>6>1   0.159   0.260   0.468
# 1>2>6>5>4>3   0.038   0.064   0.136
# 1>6>5>4>3>2   0.080   0.139   0.344
