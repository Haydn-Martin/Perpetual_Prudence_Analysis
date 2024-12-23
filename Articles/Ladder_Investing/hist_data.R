# Imports
library(dplyr)
library(glue)

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

###################
### UK Analysis ###
###################


# Import the data
df <- read.csv('/Users/hmartin6/Documents/Personal/Blog/Perpetual Prudence/Articles/ladder_investing/ann_data_clean.csv',
               header = TRUE,
               )

# Checking
head(df)
tail(df)

# Renaming columns
colnames(df) <- c('year', 'uk_stock_ret', 'uk_bond_ret', 'us_stock_ret', 'us_bond_ret')

country <- 'uk'

# Isolating UK data a
df <- df[c('year', glue('{country}_stock_ret'), glue('{country}_bond_ret'))]
head(df)

# Getting dates where all data available
bond_min <- min(which(!is.na(df[glue('{country}_bond_ret')])))
stock_min <- min(which(!is.na(df[glue('{country}_stock_ret')])))

df <- tail(df, length(df) - max(bond_min, stock_min) - 2)

rownames(df) <- 1:nrow(df)

# Now we have a lovely clean data set to work with :)

# Lets say we want to accumilate the most capital in 50 years, for example.
# 2 approaches:
#   - Ladder, in which invest in risky assets then switch to safe
#     assets, never selling.
#   - Life cycle, in which we haave a ratio which adjusts over time
#     and we rebalance to meet this ratio

# First let's see what the returns look like

chart_df <- df

colnames(chart_df) <- c("Year", "Stocks", "Bonds")

chart_df["Stocks"] <- chart_df["Stocks"]*100
chart_df["Bonds"] <- chart_df["Bonds"]*100

chart_data <- melt(chart_df, id = "Year") 

  # Chart
  
  head(chart_data)
  
  ggplot(data = chart_data, aes(x = Year, y = value, color = variable)) +
    geom_line() + scale_colour_manual(values = c("#0c9300","#cc0000")) +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(family = "serif", colour = "black", size = 10)) +
    labs(y = "Annual Return (%)", x = "Year") +
    theme(axis.text.x = element_text(family = "serif", colour = "black", size = 10)) +
    theme(axis.text.y = element_text(family = "serif", colour = "black", size = 10)) +
    theme(axis.title.x = element_text(family = "serif", colour = "black", size = 14)) +
    theme(axis.title.y = element_text(family = "serif", colour = "black", size = 14)) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.grid.major.y = element_line(colour = "black", size = 0.1)) +
    theme(axis.line = element_line(colour = "black", size = 0.3)) +
    theme(axis.ticks = element_line(colour = "black"))
  # scale_y_continuous(label=comma)


# What about the value of £1 invested at the start of the period?
stock_val <- vector(length = nrow(df))
bond_val <- vector(length = nrow(df))
stock_val[1] <- 1
bond_val[1] <- 1

for (i in 2:nrow(df)) {
  bond_val[i] <- bond_val[i-1]*(1+df[i, 3])
  stock_val[i] <- stock_val[i-1]*(1+df[i, 2])
}
matplot(cbind(stock_val, bond_val), type = c("l"), pch=1, col = 1:2)

# Generic functions for calculating portfolio value

lad_ret <- function (df) {
  
  stock_val <- vector(length = 50)
  bond_val <- vector(length = 50)
  
  stock_val[1] <- 0
  bond_val[1] <- 0
  
  for (i in 2:50) {
    
    if (i < 26) {
      bond_val[i] <- bond_val[i-1]*(1+df[i, 3])
      stock_val[i] <- stock_val[i-1]*(1+df[i, 2]) + 1000
    } else {
      bond_val[i] <- bond_val[i-1]*(1+df[i, 3]) + 1000
      stock_val[i] <- stock_val[i-1]*(1+df[i, 2])
    }
  }
  return(bond_val[50]+stock_val[50])
}

life_ret <- function (df) {
  
  bs_target <- seq(0.2, 0.8, length.out = 50)
  
  stock_val <- vector(length = 50)
  bond_val <- vector(length = 50)
  
  stock_val[1] <- 0.8
  bond_val[1] <- 0.2
  
  for (i in 2:50) {
    
    #initially contribute evenly
    bond_val[i] <- bond_val[i-1]*(1+df[i, 3]) + 500
    stock_val[i] <- stock_val[i-1]*(1+df[i, 2]) + 500
  
    #then adjust contribution and buy/sell based on ratio
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
  }
  return(bond_val[50]+stock_val[50])
}

bond_ret <- function (df) {
  bond_val <- vector(length = 50)
  bond_val[1] <- 0
  
  for (i in 2:50) {
    bond_val[i] <- bond_val[i-1]*(1+df[, 3]) + 1000
  }
  return(bond_val[50])
}

stock_ret <- function (df) {
  stock_val <- vector(length = 50)
  stock_val[1] <- 0
  
  for (i in 2:50) {
    stock_val[i] <- stock_val[i-1]*(1+df[i, 2]) + 1000
  }
  return(stock_val[50])
}  


# Testing the functions

  df_test <- df[21:70, 1:3]

  # Ladder function

  stock_val <- vector(length = 50)
  bond_val <- vector(length = 50)
  
  stock_val[1] <- 0
  bond_val[1] <- 0
  
  for (i in 2:50) {
    
    if (i < 26) {
      bond_val[i] <- bond_val[i-1]*(1+df_test[i, 3])
      stock_val[i] <- stock_val[i-1]*(1+df_test[i, 2]) + 1000
    } else {
      bond_val[i] <- bond_val[i-1]*(1+df_test[i, 3]) + 1000
      stock_val[i] <- stock_val[i-1]*(1+df_test[i, 2])
    }
  }
  
  bond_val
  stock_val
  matplot(cbind(stock_val, bond_val), type = c("l"), pch=1, col = 1:2)
  
    # Chart
    
    chart_df <- cbind(df[21:70, 1], stock_val, bond_val)
    chart_df <- as.data.frame(chart_df)
    
    colnames(chart_df) <- c("Year", "Stocks", "Bonds")
    
    chart_df["Stocks"] <- chart_df["Stocks"]/1000
    chart_df["Bonds"] <- chart_df["Bonds"]/1000
    
    chart_data <- melt(chart_df, id = "Year") 
    
    head(chart_data)
    
    ggplot(data = chart_data, aes(x = Year, y = value, color = variable)) +
      geom_line() + scale_colour_manual(values = c("#0c9300","#cc0000")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(family = "serif", colour = "black", size = 10)) +
      labs(y = "Asset Terminal Value (£000s)", x = "Year") +
      theme(axis.text.x = element_text(family = "serif", colour = "black", size = 10)) +
      theme(axis.text.y = element_text(family = "serif", colour = "black", size = 10)) +
      theme(axis.title.x = element_text(family = "serif", colour = "black", size = 14)) +
      theme(axis.title.y = element_text(family = "serif", colour = "black", size = 14)) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(panel.grid.major.y = element_line(colour = "black", size = 0.1)) +
      theme(axis.line = element_line(colour = "black", size = 0.3)) +
      theme(axis.ticks = element_line(colour = "black"))
    # scale_y_continuous(label=comma)

  # Lifecycle function
  
  df_test <- df[21:70, 1:3]
  
  bs_target <- seq(0.2, 0.8, length.out = 50) 
  bs_real <- vector(length = 50)
  bs_real[1] <- 0.2
  
  stock_val <- vector(length = 50)
  bond_val <- vector(length = 50)
  
  stock_val[1] <- 0
  bond_val[1] <- 0
  
  for (i in 2:50) {
    
    bond_val[i] <- bond_val[i-1]*(1+df_test[i, 3]) + 500
    stock_val[i] <- stock_val[i-1]*(1+df_test[i, 2]) + 500
    
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
  
  ## PLOT THE PATH OF STOCK AND BONDS
  ## PLOT THE PATH OF DESIRED RATIO AND ACTUAL RATIO
  
  stock_val
  bond_val
  matplot(cbind(bs_real, bs_target), type = c("l"), pch=1, col = 1:2)
  matplot(cbind(stock_val, bond_val), type = c("l"), pch=1, col = 1:2)
  
    # Chart
    
    chart_df <- cbind(df[21:70, 1], stock_val, bond_val)
    chart_df <- as.data.frame(chart_df)
    
    colnames(chart_df) <- c("Year", "Stocks", "Bonds")
    
    chart_df["Stocks"] <- chart_df["Stocks"]/1000
    chart_df["Bonds"] <- chart_df["Bonds"]/1000
    
    chart_data <- melt(chart_df, id = "Year") 
    
    head(chart_data)
    
    ggplot(data = chart_data, aes(x = Year, y = value, color = variable)) +
      geom_line() + scale_colour_manual(values = c("#0c9300","#cc0000")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(family = "serif", colour = "black", size = 10)) +
      labs(y = "Asset Terminal Value (£000s)", x = "Year") +
      theme(axis.text.x = element_text(family = "serif", colour = "black", size = 10)) +
      theme(axis.text.y = element_text(family = "serif", colour = "black", size = 10)) +
      theme(axis.title.x = element_text(family = "serif", colour = "black", size = 14)) +
      theme(axis.title.y = element_text(family = "serif", colour = "black", size = 14)) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(panel.grid.major.y = element_line(colour = "black", size = 0.1)) +
      theme(axis.line = element_line(colour = "black", size = 0.3)) +
      theme(axis.ticks = element_line(colour = "black"))
    # scale_y_continuous(label=comma)
  
  # Bond function
  
  bond_val <- vector(length = 50)
  bond_val[1] <- 0
  
  for (i in 2:50) {
    bond_val[i] <- bond_val[i-1]*(1+df[, 3]) + 1000
  }
  bond_val[50]
  
  # Stock function
  
  stock_val <- vector(length = 50)
  stock_val[1] <- 0
  
  for (i in 2:50) {
    stock_val[i] <- stock_val[i-1]*(1+df[i, 2]) + 1000
  }
  stock_val[50]

# Simulating the outputs
  
  # First, we look get terminal values from sequential 50Y periods
  
  # Ladder
  
  tv_len <- nrow(df) - 50
  term_values <- vector(length = tv_len)
  
  for (i in 1:tv_len) {
    
    ind_start <- i
    ind_end <- 49 + i
    
    term_values[i] <- lad_ret(df[ind_start:ind_end,])
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  scatter.smooth(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  quantile(term_values, 0.001)
  quantile(term_values, 0.0001)
    
  # Life
  
  tv_len <- nrow(df) - 50
  term_values <- vector(length = tv_len)
  
  for (i in 1:tv_len) {
    
    ind_start <- i
    ind_end <- 49 + i
    
    term_values[i] <- life_ret(df[ind_start:ind_end,])
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  scatter.smooth(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  # Bond
  
  tv_len <- nrow(df) - 50
  term_values <- vector(length = tv_len)
  
  for (i in 1:tv_len) {
    
    ind_start <- i
    ind_end <- 49 + i
    
    term_values[i] <- bond_ret(df[ind_start:ind_end,])
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  scatter.smooth(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  # Stock
  
  tv_len <- nrow(df) - 50
  term_values <- vector(length = tv_len)
  
  for (i in 1:tv_len) {
    
    ind_start <- i
    ind_end <- 49 + i
    
    term_values[i] <- stock_ret(df[ind_start:ind_end,])
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  scatter.smooth(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  quantile(term_values, 0.001)
  quantile(term_values, 0.0001)
  
  
  

## CALCULATE OTHER STATS, LOOK AT DISTRIBUTIONS, LOWER QUARTULES, ETC. AND MAKE NICE CHARTS (WITH DATE ON X AXIS)
  
  # Second, we draw with replacement from a year and simulate a lot of returns
  
  # Ladder
  
  df_len <- nrow(df)
  trials <- 10000
  term_values <- vector(length = trials)
  
  for (l in 1:trials){
    
    sample_df <- matrix(nrow = 50, ncol = 3)
    
    for (i in 1:50) {
      row_num <- sample(1:df_len, 1)
      for (j in 1:3) {
      sample_df[i,j] <- df[row_num,j]
      }
    }
  
    sample_df_test <- as.data.frame(sample_df)
    colnames(sample_df_test) <- colnames(df)
    
    term_values[l] <- lad_ret(sample_df_test)
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  mad(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  quantile(term_values, 0.001)
  quantile(term_values, 0.0001)
  
  
  # Life
  
  df_len <- nrow(df)
  trials <- 100000
  term_values <- vector(length = trials)
  
  for (l in 1:trials){
    
    sample_df <- matrix(nrow = 50, ncol = 3)
    
    for (i in 1:50) {
      row_num <- sample(1:df_len, 1)
      for (j in 1:3) {
        sample_df[i,j] <- df[row_num,j]
      }
    }
    
    sample_df_test <- as.data.frame(sample_df)
    colnames(sample_df_test) <- colnames(df)
    
    term_values[l] <- life_ret(sample_df_test)
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  mad(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  # Bond
  
  df_len <- nrow(df)
  trials <- 100000
  term_values <- vector(length = trials)
  
  for (l in 1:trials){
    
    sample_df <- matrix(nrow = 50, ncol = 3)
    
    for (i in 1:50) {
      row_num <- sample(1:df_len, 1)
      for (j in 1:3) {
        sample_df[i,j] <- df[row_num,j]
      }
    }
    
    sample_df_test <- as.data.frame(sample_df)
    colnames(sample_df_test) <- colnames(df)
    
    term_values[l] <- bond_ret(sample_df_test)
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  mad(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  # Stock
  
  df_len <- nrow(df)
  trials <- 10000
  term_values <- vector(length = trials)
  
  for (l in 1:trials){
    
    sample_df <- matrix(nrow = 50, ncol = 3)
    
    for (i in 1:50) {
      row_num <- sample(1:df_len, 1)
      for (j in 1:3) {
        sample_df[i,j] <- df[row_num,j]
      }
    }
    
    sample_df_test <- as.data.frame(sample_df)
    colnames(sample_df_test) <- colnames(df)
    
    term_values[l] <- stock_ret(sample_df_test)
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  mad(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  quantile(term_values, 0.001)
  quantile(term_values, 0.0001)
  
  
  ## SIM WITH 100K - MAKE CHARTS AND THAT - LOOK AT VOL OF RETURNS
  
  
###################
### US Analysis ###
###################

# Import the data again
df <- read.csv('/Users/hmartin6/Documents/Personal/Blog/Perpetual Prudence/Articles/ladder_investing/ann_data_clean.csv',
               header = TRUE,
)

# Checking
head(df)
tail(df)

# Renaming columns
colnames(df) <- c('year', 'uk_stock_ret', 'uk_bond_ret', 'us_stock_ret', 'us_bond_ret')
  
country <- 'us'
  
# Isolating US data a
df <- df[c('year', glue('{country}_stock_ret'), glue('{country}_bond_ret'))]
head(df)

# Getting dates where all data available
bond_min <- min(which(!is.na(df[glue('{country}_bond_ret')])))
stock_min <- min(which(!is.na(df[glue('{country}_stock_ret')])))

df <- tail(df, length(df) - max(bond_min, stock_min) - 2)

rownames(df) <- 1:nrow(df)

# First let's see what the returns look like
matplot(cbind(df[glue('{country}_stock_ret')], df[glue('{country}_bond_ret')]), type = c("l"), pch=1, col = 1:2)
# CALC AVG AND STDV


# What about the value of $1 invested at the start of the period?
stock_val <- vector(length = nrow(df))
bond_val <- vector(length = nrow(df))
stock_val[1] <- 1
bond_val[1] <- 1

for (i in 2:nrow(df)) {
  bond_val[i] <- bond_val[i-1]*(1+df[i, 3])
  stock_val[i] <- stock_val[i-1]*(1+df[i, 2])
}
matplot(cbind(stock_val, bond_val), type = c("l"), pch=1, col = 1:2)
  
# Simulating the outputs

  # First, we look get terminal values from sequential 50Y periods
  
  # Ladder
  
  tv_len <- nrow(df) - 50
  term_values <- vector(length = tv_len)
  
  for (i in 1:tv_len) {
    
    ind_start <- i
    ind_end <- 49 + i
    
    term_values[i] <- lad_ret(df[ind_start:ind_end,])
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  scatter.smooth(term_values)
  
  # Life
  
  tv_len <- nrow(df) - 50
  term_values <- vector(length = tv_len)
  
  for (i in 1:tv_len) {
    
    ind_start <- i
    ind_end <- 49 + i
    
    term_values[i] <- life_ret(df[ind_start:ind_end,])
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  scatter.smooth(term_values)
  
  # Bond
  
  tv_len <- nrow(df) - 50
  term_values <- vector(length = tv_len)
  
  for (i in 1:tv_len) {
    
    ind_start <- i
    ind_end <- 49 + i
    
    term_values[i] <- bond_ret(df[ind_start:ind_end,])
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  scatter.smooth(term_values)
  
  # Stock
  
  tv_len <- nrow(df) - 50
  term_values <- vector(length = tv_len)
  
  for (i in 1:tv_len) {
    
    ind_start <- i
    ind_end <- 49 + i
    
    term_values[i] <- stock_ret(df[ind_start:ind_end,])
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  scatter.smooth(term_values)
  
  
  ## CALCULATE OTHER STATS, LOOK AT DISTRIBUTIONS, LOWER QUARTULES, ETC. AND MAKE NICE CHARTS (WITH DATE ON X AXIS)

# Second, we draw with replacement from a year and simulate a lot of returns

  # Ladder
  
  df_len <- nrow(df)
  trials <- 100000
  term_values <- vector(length = trials)
  
  for (l in 1:trials){
    
    sample_df <- matrix(nrow = 50, ncol = 3)
    
    for (i in 1:50) {
      row_num <- sample(1:df_len, 1)
      for (j in 1:3) {
        sample_df[i,j] <- df[row_num,j]
      }
    }
    
    sample_df_test <- as.data.frame(sample_df)
    colnames(sample_df_test) <- colnames(df)
    
    term_values[l] <- lad_ret(sample_df_test)
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  # Life
  
  df_len <- nrow(df)
  trials <- 100000
  term_values <- vector(length = trials)
  
  for (l in 1:trials){
    
    sample_df <- matrix(nrow = 50, ncol = 3)
    
    for (i in 1:50) {
      row_num <- sample(1:df_len, 1)
      for (j in 1:3) {
        sample_df[i,j] <- df[row_num,j]
      }
    }
    
    sample_df_test <- as.data.frame(sample_df)
    colnames(sample_df_test) <- colnames(df)
    
    term_values[l] <- life_ret(sample_df_test)
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  # Bond
  
  df_len <- nrow(df)
  trials <- 100000
  term_values <- vector(length = trials)
  
  for (l in 1:trials){
    
    sample_df <- matrix(nrow = 50, ncol = 3)
    
    for (i in 1:50) {
      row_num <- sample(1:df_len, 1)
      for (j in 1:3) {
        sample_df[i,j] <- df[row_num,j]
      }
    }
    
    sample_df_test <- as.data.frame(sample_df)
    colnames(sample_df_test) <- colnames(df)
    
    term_values[l] <- bond_ret(sample_df_test)
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  # Stock
  
  df_len <- nrow(df)
  trials <- 100000
  term_values <- vector(length = trials)
  
  for (l in 1:trials){
    
    sample_df <- matrix(nrow = 50, ncol = 3)
    
    for (i in 1:50) {
      row_num <- sample(1:df_len, 1)
      for (j in 1:3) {
        sample_df[i,j] <- df[row_num,j]
      }
    }
    
    sample_df_test <- as.data.frame(sample_df)
    colnames(sample_df_test) <- colnames(df)
    
    term_values[l] <- stock_ret(sample_df_test)
  }
  
  hist(term_values)
  mean(term_values)
  median(term_values)
  quantile(term_values, 0.05)
  quantile(term_values, 0.01)
  
  ## SIM WITH 100K - MAKE CHARTS AND THAT - LOOK AT VOL OF RETURNS
