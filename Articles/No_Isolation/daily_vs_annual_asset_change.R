prob_up <- 0.51
prob_down <- 1-prob_up

start_w <- 1
trials <- 100000

end_wealth <- vector(length=trials)
count_year_up <- 0

for (j in 1:trials) {
  
  eoy_w <- vector(length=365)
  eoy_w[1] <- start_w
  
  for (i in 2:365) {
    
    if (runif(1, 0, 1) > prob_up) {
      eoy_w <- eoy_w-0.001
    } else {
      eoy_w <- eoy_w+0.001
    }
  }
  
  end_wealth[j] <- eoy_w[365]
  
  if (end_wealth[j] > 0.0) {
    
    count_year_up <- count_year_up+1 
  }
    
}

hist(end_wealth)
mean(end_wealth)

count_year_up
count_year_up/trials

