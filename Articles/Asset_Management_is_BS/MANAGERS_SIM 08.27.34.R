# Sim 1 - fifty/fifty

n_firms <- 1686

fifty <- matrix(nrow = 10, ncol = n_firms)

n_trials <- 1000

death_count <- vector(length = n_trials)

for (k in 1:n_trials) {

  for (i in 1:n_firms) {
    
    for(j in 1:10){
    
    fifty[j, i] <- sample(c(0,1), size = 1)
    
    if(j > 3) {
      if(fifty[j-1, i] == 0 & fifty[j-2, i] == 0 & fifty[j-3, i] == 0) {
  
      fifty[j, i] <- 0
      
      }
    }
    }
    
    if(fifty[8, i] == 0 & fifty[9, i] == 0 & fifty[10, i] == 0) {
      
      death_count[k] <- death_count[k] + 1 
      
    }
    
    }
}

death_count

mean(n_firms - death_count)

# fifty <- as.data.frame(t(fifty))

# write.csv(fifty, "~/Documents/Personal/Blog/Perpetual Prudence/Asset Management is bs/fifty.csv")

# Sim 2 - out-performance

n_firms <- 1686

mew <- 0
sd <- 15

n_trials <- 1000

returns <- matrix(nrow = 10, ncol = n_firms)

tot_return <- matrix(data = 1, nrow = n_trials, ncol = n_firms)

surv_mean_vec <- vector(length = n_trials)
surv_median_vec <- vector(length = n_trials)
surv_max_vec <- vector(length = n_trials)
surv_min_vec <- vector(length = n_trials)
annualised_ret <- vector(length = n_trials)


for (k in 1:n_trials) {
  
  for (i in 1:n_firms) {
    
    for(j in 1:10){
      
      returns[j, i] <- rnorm(1, mean = mew, sd = sd)
      
      if(j > 3) {
        if(returns[j-1, i] < mew & returns[j-2, i] < mew & returns[j-3, i] < mew) {
          
          returns[j, i] <- 0
          
        }
      }
      
      tot_return[k, i] <- tot_return[k, i]*(1+returns[j, i]/100)
      
    }
    
      if(returns[10, i] < mew & returns[9, i] < mew & returns[8, i] < mew){
        
        tot_return[k, i] <- 0}
      
      }
    
surv_returns <- vector(length = length(tot_return[k, ]) - sum(tot_return[k, ] == 0))

l <- 1

for (d in 1:n_firms) {

if(tot_return[k, d] != 0) {
  
surv_returns[l] <- tot_return[k, d]

l <- l + 1

}

}
  
surv_mean_vec[k] <- mean(surv_returns)
surv_median_vec[k] <- median(surv_returns)
surv_max_vec[k] <- max(surv_returns)
surv_min_vec[k] <- min(surv_returns)
annualised_ret[k] <- (1 + mean(surv_returns)) ^ (1/10) - 1

}

mean(surv_mean_vec)
mean(surv_median_vec)
mean(surv_max_vec)
mean(annualised_ret)
