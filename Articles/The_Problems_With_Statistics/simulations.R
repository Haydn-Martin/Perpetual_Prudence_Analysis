library(ggplot2)
theme_set(theme_minimal())
library(somebm)
set.seed(336)
library("reshape2")
require(scales)  

#For resetting par - dev.off()
par(family = "serif")
options(scipen = 0.5)


# Dice Rolls
  
  x <- runif(10, 0, 12)
  
  dice_list <- c(1, 5, 2, 8, 12, 10)
  
  mean(dice_list)
  
  
  n1_trials <- 100000
  
  t1_vec = vector(length = n1_trials)
  
  for (i in 0:n1_trials){
    t1_vec[i] <- dice_list[sample(1:6, 1)]
  }
  
  mean(t1_vec)

# Normal Estimation
  
  mew <- 1000
  sigma <- 100
  
  trials <- 1000
  
  hist(rnorm(trials, mew, sigma), freq = F, col = "#0c9300",
       ylab = "Density", xlab = "Observation",
       main = "1,000 Trials")


# Normal Plus Poisson

  mean(100*rnorm(10000, 0.01, 1) + 10*rpois(10000, 0.01))
  
  hist(100*rnorm(100, 0.01, 1) + 10*rpois(100, 0.01))
  
  100*0.01 + 100*0.01 
  
  trials <- 10000
  
  mean(100*rnorm(trials, 0.01, 1) + 100*rpois(trials, 0.01))
  
# Log Normal  
  
  lmew <- 0
  lsig <- 0.25
  
  hist(rlnorm(100, lmew, lsig))

  eg <- rlnorm(100, lmew, lsig)
  
  sam_mean <- mean(eg)
  
  sqrt((sum((eg - sam_mean)^2))/99)
  
  #sample mean = 1.020702, sample sdev = 0.2393436
  
  hist(eg, freq = F, col = "#0c9300",
       ylab = "Density", xlab = "Observation",
       main = "100 Trials")

  hist(rlnorm(100, lmew, lsig), freq = F, col = "#0c9300",
       ylab = "Density", xlab = "Observation",
       main = "100 Trials")
  
  #calculate probability of getting a result of 3 - 10 sigmas!
  