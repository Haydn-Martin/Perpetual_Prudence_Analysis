library(ggplot2)
theme_set(theme_minimal())
library(somebm)
set.seed(336)
library("reshape2")
require(scales)

library(moments)
library(Pareto)
library(rmutil)
library(graphics)

# For resetting par - dev.off()

par(family = "serif")
options(scipen = 0.5)



### Constant Amount of Withdrawal Uniform Return ###

# Min amount livable, maintain this lifestyle, otherwise go back to work

# Return constant from ~U(-5,15)
# Withdraw at start of period income for the year

#input parameters

wth <- 0.04
start_w <- 1000000
wth_am <- wth * start_w
life_time <- 65

ret_dist <- seq(-0.05,0.15,0.02)
ret_dist
term_val <- 1:11

for (j in 1:length(term_val)) {
  
  eoy_w <- vector(length = life_time)
  eoy_w[1] <- start_w - wth_am
  
  for (i in 2:life_time) {
    
    eoy_w[i] <- (eoy_w[i-1] * (1+ret_dist[j])) - wth_am
    
  }
  
  term_val[j] <- eoy_w[life_time]
  
}

term_val

# INSERT TABLE WITH TERMINAL VALUES
# Ignore negative values - add 0 - lose money


### Constant Amount of Withdrawal Normal Return ###

# Normal dist

sd <- 0.1
mean <- 0.07

x <- seq(from = -0.5,to = 0.6, length.out = 10000)
plot(x, dnorm(x, mean, sd), type = 'l', col = "#0c9300", main = "Normal Distribution", ylab = "f(x)", xlab = "x")
abline(v = 0.05, col="#cc0000", lwd=2, lty=1)

plot(1:30, rnorm(30, mean, sd), type = 'h', lwd = 5,
     col = "#0c9300", main = "Normal Returns", ylab = "Return (%)", xlab = "Trial")

# Return variable from ~N(0.05,0.1)
# Withdraw at start of period income for the year

#input parameters

wth <- 0.04
start_w <- 1000000
wth_am <- wth * start_w
life_time <- 65

trials <- 100
all_ret <- matrix(nrow = life_time, ncol = trials)
term_vec <- vector(length = trials)

for (j in 1:trials) {
  
  eoy_w <- vector(length = life_time)
  eoy_w[1] <- start_w - wth_am
  
  for (i in 2:life_time) {
    
    eoy_w[i] <- (eoy_w[i-1] * (1+rnorm(1, mean, sd))) - wth_am
    
    if (eoy_w[i] < 0) {
      eoy_w[i] <- 0
    }
    
  }
  
  all_ret[, j] <- eoy_w
  term_vec[j] <- all_ret[life_time, j] 
  
}


# INSERT CHART SHOWING ALL SIMULATION PATHS

test <- stack(as.data.frame(all_ret/1000000))

chart_data <- data.frame(x=rep(1:life_time, trials),
                         value=test['values'],
                         variable=test['ind'])

head(chart_data)


ggplot(data=chart_data, aes(x=x, y=values)) +
  geom_line(aes(group = ind)) +
  geom_hline(yintercept = 0, color = '#cc0000') +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(family = "serif", colour = "black")) +
  labs(y = "Wealth (£M)", x = "FIRE Year") +
  theme(axis.text.x = element_text(family = "serif", colour = "black")) +
  theme(axis.text.y = element_text(family = "serif", colour = "black")) +
  theme(axis.title.x = element_text(family = "serif", colour = "black", size = 12)) +
  theme(axis.title.y = element_text(family = "serif", colour = "black", size = 12)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black", size = 0.1)) +
  theme(axis.line = element_line(colour = "black", size = 0.3)) +
  theme(axis.ticks = element_line(colour = "black")) +
  scale_y_continuous(limits = c(0,20))



### Constant Amount of Laplace Return ###

meanlap <- 0.07 # Where the dist. is centred
scalelap <- 0.07 # Changes the density values (shape stays the same)

x <- seq(from = -0.5,to = 0.6, length.out = 10000)
plot(x, dlaplace(x, meanlap, scalelap), type = 'l',
     col = "#0c9300", main = "Laplace Distribution", ylab = "f(x)", xlab = "x")
abline(v = 0.05, col="#cc0000", lwd=2, lty=1)

plot(1:30, rlaplace(30, meanlap, scalelap), type = 'h', lwd = 5,
     col = "#0c9300", main = "Laplace Returns", ylab = "Return (%)", xlab = "Trial")

# Put side-by-side with FTSE 100 returns!

# Return variable from ~LAP(0.05,0.075)
# Withdraw at start of period income for the year

#input parameters

wth <- 0.04
start_w <- 1000000
wth_am <- wth * start_w
life_time <- 65

trials <- 100
all_ret <- matrix(nrow = life_time, ncol = trials)
term_vec <- vector(length = trials)

for (j in 1:trials) {
  
  eoy_w <- vector(length = life_time)
  eoy_w[1] <- start_w - wth_am
  
  for (i in 2:life_time) {
    
    eoy_w[i] <- (eoy_w[i-1] * (1+rlaplace(1, meanlap, scalelap))) - wth_am
    
  }
  
  all_ret[, j] <- eoy_w
  term_vec[j] <- all_ret[life_time, j] 
  
}

hist(term_vec)
hist(term_vec/1000000, freq = T, col = "#0c9300", 
     ylab = "Trials", xlab = "Wealth (£M)", main = "Laplace Terminal Values")
