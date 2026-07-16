################################################################################
# Attention Overload
# Simulation Replication File -- plotting
# Matias D. Cattaneo, Paul Cheung, Xinwei Ma, Yusufcan Masatlioglu
# HAOM
################################################################################

################################################################################
# load packages and source
################################################################################

library("ramchoice")
library("tikzDevice")
source("Cattaneo-Cheung-Ma-Masatlioglu_2023_replication-HAOM--auxiliary.R")


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

################################################################################
# load simulation results
# compute 95 percentiles of the estimated lower bounds across simulations 
################################################################################

simuResults <- read.csv(paste("Result-", simuIndex, "-.txt", sep=""), header=FALSE)
simuResults <- matrix(apply(simuResults, MARGIN=2, FUN=function(x) {quantile(x, 0.95)}), nrow=uSize)

################################################################################
# extract theta_{k j} from the matrix of preference distributions
################################################################################

# distribution of heterogeneous preference
# Pr[>_{kj}] is the (k,j)-th entry of the matrix below
# diagonal and upper triangular parts are not used
prefDist <- matrix(0.05, ncol=6, nrow=6)

# true theta_{k j}
trueTheta <- c()
for (k in 2:nrow(prefDist)) {
  trueTheta <- c(trueTheta, cumsum(prefDist[k, 1:(k-1)]))
}

################################################################################
# theoretical lower bounds for theta_{k j} suggested by HAOM
################################################################################

menu <- prob <- matrix(0, nrow=0, ncol=6)
for (j in choiProblems) {
  temp <- genChoiceProbHAOM(6, j, 2, matrix(0.05, ncol=6, nrow=6))
  menu <- rbind(menu, temp$menu); prob <- rbind(prob, temp$prob)
}
trueLowerBound <- lowerTriToVec(revealPrefLowerBoundHAOM(menu, prob))

################################################################################
# plot
################################################################################

tikz(paste("fig", simuIndex, ".tex", sep=""),standAlone=TRUE, height=4.5, width=9)

# true theta_{k j}
plot(1:length(trueTheta), trueTheta, ylim=c(0, 0.3),
     main="", 
     ylab="", 
     xlab="", 
     xaxt="n", 
     pch=16, col=rgb(0.8, 0, 0, 1))

# theoretical lower bounds for theta_{k j}
points(1:length(trueTheta), trueLowerBound, 
       pch=3, cex=1.5, col=rgb(0.8, 0, 0, 1))

# lower bounds for different effective sample sizes
temp3 <- lowerTriToVec(simuResults[, 0*uSize + (1:uSize)])
points(1:length(trueTheta), temp3, 
       col=rgb(0, 1, 0.8, 1), pch=18, cex=0.9)

temp3 <- lowerTriToVec(simuResults[, 1*uSize + (1:uSize)])
points(1:length(trueTheta), temp3, 
       col=rgb(0, 0.6, 1, 1), pch=18, cex=0.9)

temp3 <- lowerTriToVec(simuResults[, 2*uSize + (1:uSize)])
points(1:length(trueTheta), temp3, 
       col=rgb(0, 0, 1, 1), pch=18, cex=0.9)

# add legends
points(3.8, 0.29, pch=16, col=rgb(0.8, 0, 0, 1))
text(1, 0.29,  "$\\theta_{kj} = \\sum_{\\ell \\leq j}\\tau(\\succ_{k\\ell}):$", pos=4)

points(5.5, 0.29, pch=3, cex=1.5, col=rgb(0.8, 0, 0, 1))
text(4.5, 0.29,  "$\\underline{\\theta}_{kj}:$", pos=4)

text(1, 0.24,  "$\\widehat{\\underline{\\theta}}_{k j}:$", pos=4)

points(2.1, 0.24, col=rgb(0, 1, 0.8, 1), pch=18, cex=0.9)
text(2.2, 0.24, "$N_S=50 $", pos=4)

points(3.6, 0.24, col=rgb(0, 0.6, 1, 1), pch=18, cex=0.9)
text(3.7, 0.24, "$N_S=100$", pos=4)

points(5.1, 0.24, col=rgb(0, 0, 1, 1), pch=18, cex=0.9)
text(5.2, 0.24, "$N_S=200$", pos=4)

# add x axis tick labels
axis(side=1, at=1:15, lwd.ticks=0, lwd=0, padj=-1, 
     labels = c("$\\succ_{21}$", 
                "$\\succ_{31}$",
                "$\\succ_{32}$",
                "$\\succ_{41}$",
                "$\\succ_{42}$",
                "$\\succ_{43}$",
                "$\\succ_{51}$",
                "$\\succ_{52}$",
                "$\\succ_{53}$",
                "$\\succ_{54}$",
                "$\\succ_{61}$",
                "$\\succ_{62}$",
                "$\\succ_{63}$",
                "$\\succ_{64}$",
                "$\\succ_{65}$"))
dev.off()


