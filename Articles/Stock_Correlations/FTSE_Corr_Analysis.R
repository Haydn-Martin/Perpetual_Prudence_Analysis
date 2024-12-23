# Set Perpetual Prudence Themes
  
  theme_set(theme_minimal())
  set.seed(336)
  
  #For resetting par - dev.off()
  
  par(family = "serif")
  options(scipen = 0.5)
  
# Import required packages
  
  #install.packages('quantmod')
  #install.packages("devtools")
  library(quantmod)
  #library(tidyverse)
  library(glue)
  library(zoo)
  library(dbplyr)
  #install.packages('magrittr')
  library(magrittr)
  
  
# List of companies to look at: https://www.fidelity.co.uk/shares/ftse-100/
# https://dailypik.com/uk100-companies/

### Daily Correlation ###
  
corr_freq = "Daily"
  
company_ticks <- c('AZN.L', 'SHEL.L', 'HSBA.L', 'ULVR.L')

for (ticker in company_ticks) {
  
  #get data and transform
  df <- na.omit(data.frame(getSymbols.yahoo(ticker, auto.assign = FALSE)))[paste(ticker, '.Close', sep='')]
  #get return df
  ret_df <- data.frame(dailyReturn(as.xts(df))) #CHANGE FREQUENCY
  #add date columns and set indexes
  df <- cbind(Date=rownames(df), df)
  rownames(df) <- 1:nrow(df)
  ret_df <- cbind(Date=rownames(ret_df), ret_df)
  rownames(ret_df) <- 1:nrow(ret_df)
  #merge that shit
  df <- merge(df, ret_df, by="Date", all.x=TRUE)
  #rename return column
  colnames(df)[3] = paste(ticker, glue('.{corr_freq}Ret'), sep='')
  #assign to close_price_X
  assign(paste0("price_ret_", ticker), df)
  
}

# Cross correlation

  # Join
  cov_mat <- price_ret_AZN.L
    for (pr_df in list(price_ret_HSBA.L, price_ret_SHEL.L, price_ret_ULVR.L)) {
      cov_mat <- merge(cov_mat, pr_df, by="Date", all=FALSE)
    }
  cov_mat
  # Select return columns
  ret_name_list <- vector(length=4)
  for (i in 1:4) {
    ret_name_list[i] <- glue('{company_ticks[i]}.{corr_freq}Ret')
  }
  # Get covariance
  ret_cov <- cov_mat[ret_name_list]
  
  cor(ret_cov[3], ret_cov[4], method = 'pearson')
  # Select price columns
  price_name_list <- vector(length=4)
  for (i in 1:4) {
    price_name_list[i] <- glue('{company_ticks[i]}.Close')
  }
  price_cov <- cov_mat[price_name_list]
  cor(price_cov[1], price_cov[4], method = 'pearson')
  
  #PLOT 4 LINES ON A CHART WITH SOME EXAMPLES OF CORR
  cor(price_cov[1], price_cov[4], method = 'pearson')
  
  # Rolling correlation
  
  plot(rollapply(price_cov, width = 90, function(x) cor(x[,1],x[,4]), by.column = FALSE),
       type = 'l', col = "#0c9300", ylab = "Correlation", xlab = "Correlation Window", main = "Daily Price Rolling Correlation")
  
  plot(rollapply(price_cov, width = 90, function(x) cor(x[,1],x[,4]), by.column = FALSE),
       type = 'l', col = "#0c9300", axes=FALSE, ylab = "Correlation", xlab = "", main = "90-Day Rolling Daily Price Correlation")
  Axis(side=2, labels=TRUE)
  
  '''
  
  # GET ROLLING CORRELATION, JOIN TO DATE, NICELY FORMAT IN GGPLOT
  
  roll_corr <- rollapply(price_cov, width = 90, function(x) cor(x[,1],x[,4]), by.column = FALSE)
  length(roll_corr)
  nrow(cov_mat["Date"])
  nrow(cov_mat["Date"]) - length(roll_corr)
  
  cov_mat_df <- as.data.frame(cov_mat)
  roll_dates <- tail(cov_mat_df, length(roll_corr))["Date"]
  roll_dates <- as.data.frame(roll_dates)
  
  roll_dates %>%
    mutate(Date=as.Date(Date, format = "%Y-%m-%d"))
  roll_dates %<>%
    mutate(Date = as.Date(Date, format= "%Y-%m-%d"))
  
  year <- as.numeric(format(roll_dates,'%Y'))
  
  
  ggplot(data = good_timeline, mapping = aes(x = Day, y = Price)) +
    geom_line()
  
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
  
  '''
  
### Monthly Correlation ###
  
  corr_freq = "Monthly"
  
  company_ticks <- c('AZN.L', 'SHEL.L', 'HSBA.L', 'ULVR.L')
  
  for (ticker in company_ticks) {
    
    #get data and transform
    df <- na.omit(data.frame(getSymbols.yahoo(ticker, auto.assign = FALSE)))[paste(ticker, '.Close', sep='')]
    #get return df
    ret_df <- data.frame(monthlyReturn(as.xts(df))) #CHANGE FREQUENCY
    #add date columns and set indexes
    df <- cbind(Date=rownames(df), df)
    rownames(df) <- 1:nrow(df)
    ret_df <- cbind(Date=rownames(ret_df), ret_df)
    rownames(ret_df) <- 1:nrow(ret_df)
    #merge that shit
    df <- merge(df, ret_df, by="Date", all.x=TRUE)
    #rename return column
    colnames(df)[3] <-  paste(ticker, glue('.{corr_freq}Ret'), sep='')
    #drop NAs
    df <- na.omit(df)
    #assign to close_price_X
    assign(paste0("price_ret_", ticker), df)
  }
  
  # Cross correlation
  
  # Join
  cov_mat <- price_ret_AZN.L
  for (pr_df in list(price_ret_HSBA.L, price_ret_SHEL.L, price_ret_ULVR.L)) {
    cov_mat <- merge(cov_mat, pr_df, by="Date", all=FALSE)
  }
  cov_mat
  # Select return columns
  ret_name_list <- vector(length=4)
  for (i in 1:4) {
    ret_name_list[i] <- glue('{company_ticks[i]}.{corr_freq}Ret')
  }
  # Get covariance
  ret_cov <- cov_mat[ret_name_list]
  
  cor(ret_cov[2], ret_cov[4], method = 'pearson')
  # Select price columns
  price_name_list <- vector(length=4)
  for (i in 1:4) {
    price_name_list[i] <- glue('{company_ticks[i]}.Close')
  }
  price_cov <- cov_mat[price_name_list]
  
  cor(price_cov[4], price_cov[3], method = 'pearson')
  
  #PLOT 4 LINES ON A CHART WITH SOME EXAMPLES OF CORR
  
  # Rolling correlation
  plot(rollapply(price_cov, width = 120, function(x) cor(x[,1],x[,4]), by.column = FALSE),
       type = 'l', col = "#0c9300", ylab = "Correlation", xlab = "Correlation Window", main = "Daily Price Rolling Correlation")
  
  plot(rollapply(price_cov, width = 120, function(x) cor(x[,1],x[,4]), by.column = FALSE),
       type = 'l', col = "#0c9300", axes=FALSE, ylab = "Correlation", xlab = "", main = "10-Year Rolling Monthly Price Correlation")
  Axis(side=2, labels=TRUE)
  