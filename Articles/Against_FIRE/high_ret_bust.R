# 30:1 leverage
# Returns ~N(0.1, 0.05), smack 30x on that


start_w <- 1000000
life_time <- 20
eoy_w <- vector(length = life_time)

trials <- 100000
term_vec <- vector(length = trials)
count_0 <- 0

mean <- 0.01
sd <- 0.02
lev <- 30

for (j in 1:trials) {
  
  eoy_w[1] <- start_w
  
  for (i in 2:life_time) {
    
    eoy_w[i] <- (eoy_w[i-1] * (1+(lev*rnorm(1, mean, sd))))
    
    if (eoy_w[i] < 0.0) {
      eoy_w[i] <- 0.0
    }
    
  }
  
  term_vec[j] <- eoy_w[life_time]
  
  if (term_vec[j] == 0.0) {
    count_0 <- count_0+1
  }
  
}



count_0

