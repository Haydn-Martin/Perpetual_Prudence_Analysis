

# For resetting par - dev.off()

par(family = "serif")
options(scipen = 0.5)

# Defining mean and sd of toy assets

stock_mean <- 0.1
stock_sd <- 0.2

bond_mean <- 0.05
bond_sd <- 0.025

# Chars

x <- seq(from = -0.8,to = 0.8, length.out = 10000)
plot(x, dnorm(x, bond_mean, bond_sd), type = 'l', col = "#0c9300", main = "Low-Risk Returns", ylab = "f(x)", xlab = "x")


# Bond function

trials <- 100000
term_values = vector(length = trials)

for (j in 1:trials) {
    
  bond_val <- vector(length = 50)
  bond_val[1] <- 0
  
  for (i in 2:50) {
    bond_val[i] <- bond_val[i-1]*((1+rnorm(1,bond_mean,bond_sd))) + 1000
  }
  term_values[j] <- bond_val[50]
}

hist(term_values)
mean(term_values)
median(term_values)
quantile(term_values, 0.05)
quantile(term_values, 0.01)

# Stock function

trials <- 100000
term_values = vector(length = trials)

for (j in 1:trials) {
  
  stock_val <- vector(length = 50)
  stock_val[1] <- 0
  
  for (i in 2:50) {
    stock_val[i] <- stock_val[i-1]*((1+rnorm(1,stock_mean,stock_sd))) + 1000
  }
  term_values[j] <- stock_val[50]
}

hist(term_values)
mean(term_values)
median(term_values)
quantile(term_values, 0.05)
quantile(term_values, 0.01)

  # Chart 

  plot(term_values, type = 'h', lwd = 5,
     col = "#0c9300", main = "Normal Returns", ylab = "Return (%)", xlab = "Trial")
  
  hist(, freq = T, col = "#0c9300", 
       ylab = "Trials", xlab = "Wealth (£M)", main = "Laplace Terminal Values")
  
  x <- seq(from = -0.5,to = 0.6, length.out = 10000)
  plot(x, dnorm(x, mean, sd), type = 'l', col = "#0c9300", main = "Normal Distribution", ylab = "f(x)", xlab = "x")



# Ladder function

trials <- 10000
term_values = vector(length = trials)

for (j in 1:trials) {
  
  stock_val <- vector(length = 50)
  bond_val <- vector(length = 50)
  
  stock_val[1] <- 0
  bond_val[1] <- 0
  
  for (i in 2:50) {
    if (i < 26) {
      bond_val[i] <- bond_val[i-1]*(1+rnorm(1,bond_mean,bond_sd))
      stock_val[i] <- stock_val[i-1]*(1+rnorm(1,stock_mean,stock_sd)) + 1000
    } else {
      bond_val[i] <- bond_val[i-1]*(1+rnorm(1,bond_mean,bond_sd)) + 1000
      stock_val[i] <- stock_val[i-1]*(1+rnorm(1,stock_mean,stock_sd))
    }
  }
  term_values[j] <- stock_val[50]+bond_val[50]
}

hist(term_values)
mean(term_values)
median(term_values)
quantile(term_values, 0.05)
quantile(term_values, 0.01)

# Lifecycle function

trials <- 10000
term_values = vector(length = trials)

for (j in 1:trials) {

  bs_target <- seq(0.2, 0.8, length.out = 50) 
  bs_real <- vector(length = 50)
  bs_real[1] <- 0.2
  
  stock_val <- vector(length = 50)
  bond_val <- vector(length = 50)
  
  stock_val[1] <- 0
  bond_val[1] <- 0
  
  for (i in 2:50) {
    
    bond_val[i] <- bond_val[i-1]*(1+rnorm(1,bond_mean,bond_sd)) + 500
    stock_val[i] <- stock_val[i-1]*(1+rnorm(1,stock_mean,stock_sd)) + 500
    
    if ((bond_val[i]/(bond_val[i]+stock_val[i])) > bs_target[i]) {
      while ((bond_val[i]/(bond_val[i]+stock_val[i])) - bs_target[i] > 0.001) {
        bond_val[i] <- bond_val[i] - 10
        stock_val[i] <- stock_val[i] + 10
      }
    } else {
      while (bs_target[i] - (bond_val[i]/(bond_val[i]+stock_val[i])) > 0.001) {
        bond_val[i] <- bond_val[i] + 10
        stock_val[i] <- stock_val[i] - 10
      }
    }
    bs_real[i] <- bond_val[i]/(bond_val[i]+stock_val[i])
  }
  term_values[j] <- stock_val[50]+bond_val[50]
}

hist(term_values)
mean(term_values)
median(term_values)
quantile(term_values, 0.05)
quantile(term_values, 0.01)







### Increasing income ###

inc_vec <- seq(500, 1500, length.out = 50)
length(inc_vec)
inc_vec

# Defining mean and sd of toy assets

stock_mean <- 0.1
stock_sd <- 0.2

bond_mean <- 0.05
bond_sd <- 0.025

# Bond function

trials <- 100000
term_values = vector(length = trials)

for (j in 1:trials) {
  
  bond_val <- vector(length = 50)
  bond_val[1] <- 0
  
  for (i in 2:50) {
    bond_val[i] <- bond_val[i-1]*((1+rnorm(1,bond_mean,bond_sd))) + inc_vec[i]
  }
  term_values[j] <- bond_val[50]
}

hist(term_values)
mean(term_values)
median(term_values)
quantile(term_values, 0.05)
quantile(term_values, 0.01)

# Stock function

trials <- 100000
term_values = vector(length = trials)

for (j in 1:trials) {
  
  stock_val <- vector(length = 50)
  stock_val[1] <- 0
  
  for (i in 2:50) {
    stock_val[i] <- stock_val[i-1]*((1+rnorm(1,stock_mean,stock_sd))) + + inc_vec[i]
  }
  term_values[j] <- stock_val[50]
}

hist(term_values)
mean(term_values)
median(term_values)
quantile(term_values, 0.05)
quantile(term_values, 0.01)

# Ladder function

trials <- 10000
term_values = vector(length = trials)

for (j in 1:trials) {
  
  stock_val <- vector(length = 50)
  bond_val <- vector(length = 50)
  
  stock_val[1] <- 0
  bond_val[1] <- 0
  
  for (i in 2:50) {
    if (i < 26) {
      bond_val[i] <- bond_val[i-1]*(1+rnorm(1,bond_mean,bond_sd))
      stock_val[i] <- stock_val[i-1]*(1+rnorm(1,stock_mean,stock_sd)) + inc_vec[i]
    } else {
      bond_val[i] <- bond_val[i-1]*(1+rnorm(1,bond_mean,bond_sd)) + inc_vec[i]
      stock_val[i] <- stock_val[i-1]*(1+rnorm(1,stock_mean,stock_sd))
    }
  }
  term_values[j] <- stock_val[50]+bond_val[50]
}

hist(term_values)
mean(term_values)
median(term_values)
quantile(term_values, 0.05)
quantile(term_values, 0.01)

# Lifecycle function

trials <- 10000
term_values = vector(length = trials)

for (j in 1:trials) {
  
  bs_target <- seq(0.2, 0.8, length.out = 50) 
  bs_real <- vector(length = 50)
  bs_real[1] <- 0.2
  
  stock_val <- vector(length = 50)
  bond_val <- vector(length = 50)
  
  stock_val[1] <- 0
  bond_val[1] <- 0
  
  for (i in 2:50) {
    
    bond_val[i] <- bond_val[i-1]*(1+rnorm(1,bond_mean,bond_sd)) + inc_vec[i]/2
    stock_val[i] <- stock_val[i-1]*(1+rnorm(1,stock_mean,stock_sd)) + inc_vec[i]/2
    
    if ((bond_val[i]/(bond_val[i]+stock_val[i])) > bs_target[i]) {
      while ((bond_val[i]/(bond_val[i]+stock_val[i])) - bs_target[i] > 0.001) {
        bond_val[i] <- bond_val[i] - 10
        stock_val[i] <- stock_val[i] + 10
      }
    } else {
      while (bs_target[i] - (bond_val[i]/(bond_val[i]+stock_val[i])) > 0.001) {
        bond_val[i] <- bond_val[i] + 10
        stock_val[i] <- stock_val[i] - 10
      }
    }
    bs_real[i] <- bond_val[i]/(bond_val[i]+stock_val[i])
  }
  term_values[j] <- stock_val[50]+bond_val[50]
}

hist(term_values)
mean(term_values)
median(term_values)
quantile(term_values, 0.05)
quantile(term_values, 0.01)




