library(pracma)
library(quantmod)
library(pacman)
library(PerformanceAnalytics)
library(xts)
library(base)
library(ggplot2)
library("reshape2")
require(scales)
library(somebm)
#install.packages("ggeasy")
library(ggeasy)

theme_set(theme_minimal())
set.seed(336)

#For resetting par - dev.off()

par(family = "serif")

options(scipen = 0.5)


# Importing FTSE100 daily

ftse <- na.omit(data.frame(getSymbols.yahoo("^FTSE", auto.assign = F)))

# Getting close price

colnames(ftse) <- c("Open", "High", "Low", "Close",  "Volume", "Adjusted")
close_price <- ftse["Close"]
head(close_price)

# Making desired DF

close_price <- matrix(unlist(close_price))
typeof(close_price)

dates <- row.names(ftse['Close'])
dates <- as.Date(dates)

close_price <- data.frame(close_price)
typeof(close_price)

close_price$Days <- dates
head(close_price)

colnames(close_price) <- c('Price', 'Day')

# Plotting

ggplot(data = close_price, mapping = aes(x = Day, y = Price)) +
  geom_line()

# Selecting dates

# Normal dates

  min_date <- as.Date('2010-01-01')
  
  max_date <- as.Date('2020-02-24')
  
  good_timeline <- subset(close_price, min_date < Day & max_date > Day, select = c('Price', 'Day'))
  
  ggplot(data = good_timeline, mapping = aes(x = Day, y = Price)) +
    geom_line()
  
  # Getting returns
  
  ret <- Delt(unlist(good_timeline['Price']))
  ret <- ret[2:nrow(ret)]
  
  hist(ret,freq = F, col = "#0c9300",
       ylab = "Density", xlab = "Observation",
       main = "FTSE 100 Normal Price Return")
  
  mean(ret)
  
  sqrt((sum((ret - mean(ret))^2))/(length(ret)-1))
  
  # GET MEAN AND SD AND NUMBER OF OBSERVATIONS
  
  # OBS = 2555
  # SAMPLE MEAN = 0.0001595058
  # SAMPLE SD = 0.009290259
  
  # Looks roughly gaussian - roughly symetric with a couple of 4-sigma events
  # that should are unexpected and some "outliers" that can be ignored...

  # Skewed dates
  
  min_date <- as.Date('2010-01-01')
  
  max_date <- as.Date('2020-03-24')
  
  bad_timeline <- subset(close_price, min_date < Day & max_date > Day, select = c('Price', 'Day'))
  
  ggplot(data = bad_timeline, mapping = aes(x = Day, y = Price)) +
    geom_line()
  
  # Getting returns
  
  ret <- Delt(unlist(bad_timeline['Price']))
  ret <- ret[2:nrow(ret)]
  
  hist(ret, freq = F, col = "#0c9300",
       ylab = "Density", xlab = "Observation",
       main = "FTSE 100 Non-Normal Price Return")
  
  # Here we have a 10-sigma event --> 1 in 524,900,000,000,000,000,000 days
  # can't dismiss outliers because have huge effect on portfolio

# Finding min and max

min_date <- as.Date(close_price[close_price['Price'] == min(close_price['Price'])][2])
max_date <- as.Date(close_price[close_price['Price'] == max(close_price['Price'])][2])

min_date
max_date

# Good timeline

good_timeline <- subset(close_price, min_date < Day & max_date > Day, select = c('Price', 'Day'))

ggplot(data = good_timeline, mapping = aes(x = Day, y = Price)) +
  geom_line()

# Bad timeline

bad_timeline <- subset(close_price, Day < as.Date("2016-03-30"), select = c('Price', 'Day'))

# Nice formatting

# Good timeline

ggplot(data = good_timeline, mapping = aes(x = Day, y = Price)) +
  geom_line(colour="#0c9300") +
  theme(legend.position = "none") +
  labs(y = "Price", x = "Date", title = "FTSE 100 Goes Up") +
  theme_bw() +
  ggeasy::easy_center_title() +
  theme(title = element_text(family = "serif", colour = "black", size = 20)) +
  theme(axis.text.x = element_text(family = "serif", colour = "black")) +
  theme(axis.text.y = element_text(family = "serif", colour = "black")) +
  theme(axis.title.x = element_text(family = "serif", colour = "black", size = 12)) +
  theme(axis.title.y = element_text(family = "serif", colour = "black", size = 12)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(panel.grid.major.y = element_line(colour = "black", size = 0.1)) +
  theme(axis.line = element_line(colour = "black", size = 0.3)) +
  theme(axis.ticks = element_line(colour = "black"))
# scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
# scale_y_continuous(expand = c(0, 0))

# Bad timeline

ggplot(data = bad_timeline, mapping = aes(x = Day, y = Price)) +
  geom_line(colour="#0c9300") +
  theme(legend.position = "none") +
  labs(y = "Price", x = "Date", title = "FTSE 100 Goes Nowhere") +
  theme_bw() +
  ggeasy::easy_center_title() +
  theme(title = element_text(family = "serif", colour = "black", size = 20)) +
  theme(axis.text.x = element_text(family = "serif", colour = "black")) +
  theme(axis.text.y = element_text(family = "serif", colour = "black")) +
  theme(axis.title.x = element_text(family = "serif", colour = "black", size = 12)) +
  theme(axis.title.y = element_text(family = "serif", colour = "black", size = 12)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(panel.grid.major.y = element_line(colour = "black", size = 0.1)) +
  theme(axis.line = element_line(colour = "black", size = 0.3)) +
  theme(axis.ticks = element_line(colour = "black"))
# scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
# scale_y_continuous(expand = c(0, 0))
