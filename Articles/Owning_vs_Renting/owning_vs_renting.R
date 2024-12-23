library(ggplot2)
theme_set(theme_minimal())
library(somebm)
set.seed(336)
library("reshape2")
require(scales)  

#For resetting par - dev.off()

par(family = "serif")

options(scipen = 0.5)


owning_excess <- function(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem){
  
  # Extra input variables
  
    # Stamp Duty
    
  if(val < 125000){
    
    stamp <- 0
    
  } else { 
    
    if (val < 250000) {
      
      stamp <- 0.02 * (val - 125000)
      
    } else {
      
      if (val < 925000) {
        
        stamp <- 0.02 * (250000 - 125000) + 0.05 * (val - 250000)
        
      } else {
        
        if (val < 1500000) {
          
          stamp <- 0.02 * (250000 - 125000) + 0.05 * (925000 - 250000) + 0.1 * (val - 925000)
          
        } else {
          
          if (1500000 <= val) {
            
            stamp <- 0.02 * (250000 - 125000) + 0.05 * (925000 - 250000) + 0.1 * (1500000 - 925000) + 0.12 * (val - 1500000)
            
          }}}}}  
  
  deposit <- (1-ltv) * val
  mort <- val - deposit
  
  var_term <- tot_term - fix_term
  var_rate <- svr_prem + ir
  
  # Variables for calculations
  
  ret_month <- (1 + ret) ^ (1/12) - 1
  hpg_month <- (1 + hpg) ^ (1/12) - 1
  
  tot_term_month <- tot_term * 12
  fix_term_month <- fix_term * 12
  var_term_month <- var_term * 12
  
  fix_rate_month <- fix_rate / 12
  var_rate_month <- var_rate / 12
  
  mort_post_fix <- mort * (1 + fix_rate_month) ^ fix_term_month - fix_pay * (((1+ fix_rate_month) ^ fix_term_month - 1) / fix_rate_month)
  
  var_pay <- (mort_post_fix * var_rate_month * (1 + var_rate_month) ^ var_term_month) / ((1 + var_rate_month) ^ var_term_month - 1)
  
  # Generating inflation series
  
  inf_vec <- vector(length = tot_term_month)
  
  for (i in 0:tot_term){
    
    for(f in 1:12){
      
      inf_vec[f + i * 12] <- (1 + inf) ^ i
      
    }  
    
  }
  
  # Generating monthly figures for final calculations
  
  rent_paid <- vector(length = tot_term_month+1)
  inv_account <- vector(length = tot_term_month+1)  
  
  tot_mort_payments <- vector(length = tot_term_month+1)
  int_payments <- vector(length = tot_term_month+1)
  prin_payments <- vector(length = tot_term_month+1)
  loan_value <- vector(length = tot_term_month+1)
  
  house_value <- vector(length = tot_term_month+1)
  other_expenses <- vector(length = tot_term_month+1)
  
  for (i in 1:(tot_term_month+1)) {
    
    # Mortgage payments   
    
     # Initial values
    
      if (fix_term_month > 0) {
      
      tot_mort_payments[1] <- fix_pay  
      int_payments[1] <- mort * fix_rate_month
      prin_payments[1] <- tot_mort_payments[1] - int_payments[1]
      loan_value[1] <- mort - prin_payments[1]
        
      } else {
        
      tot_mort_payments[1] <- var_pay  
      int_payments[1] <- mort * var_rate_month
      prin_payments[1] <- tot_mort_payments[1] - int_payments[1]
      loan_value[1] <- mort - prin_payments[1]
      
      }
    
      # Other values
      
      if (i < fix_term_month) {
        
        tot_mort_payments[i+1] <- fix_pay
        int_payments[i+1] <- fix_rate_month * loan_value[i]
        prin_payments[i+1] <- tot_mort_payments[i+1] - int_payments[i+1]
        loan_value[i+1] <- loan_value[i] - prin_payments[i+1]
        
      } else {
        
        tot_mort_payments[i+1] <- var_pay  
        int_payments[i+1] <- var_rate_month * loan_value[i]
        prin_payments[i+1] <- tot_mort_payments[i+1] - int_payments[i+1]
        loan_value[i+1] <- loan_value[i] - prin_payments[i+1]
        
      }
    
    # Other info
    
    rent_paid[1] <- rent * inf_vec[1]
    rent_paid[i+1] <- rent * inf_vec[i+1]
    
    house_value[1] <- val * (1 + hpg_month)
    house_value[i+1] <- house_value[i] * (1 + hpg_month)
    
    other_expenses[1] <- house_value[1] * oc / 12
    other_expenses[i+1] <- house_value[i+1] * oc / 12
    
    inv_account[1] <- (uc + deposit + stamp) * (1 + ret_month) + (tot_mort_payments[1] + other_expenses[1] - rent_paid[1]) * res
    inv_account[i+1] <- inv_account[i] * (1 + ret_month) + (tot_mort_payments[i+1] + other_expenses[i+1] - rent_paid[i+1]) * res

    }

  # Output  
  
  owning_excess_percent <- (house_value[tot_term_month] - inv_account[tot_term_month]) / house_value[tot_term_month]
  owning_excess_percent
  
  }
  
# Output test

owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)

# Generating charts for variables

  # Inflation

  inf_ser <- seq(0,0.2,0.01)
  owning_excess_inf <- vector(length = length(inf_ser))
  
  for (i in 1:length(inf_ser)) {
    
    owning_excess_inf[i] <- owning_excess(inf = inf_ser[i], ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_inf
  
  plot(inf_ser, owning_excess_inf, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Inflation")

  # Inflation (with other rates dependent)
  
  inf_ser <- seq(0,0.2,0.01)
  owning_excess_inf <- vector(length = length(inf_ser))
  
  for (i in 1:length(inf_ser)) {
    
    owning_excess_inf[i] <- owning_excess(inf = inf_ser[i], ret = inf_ser[i] + 0.04, ir = inf_ser[i], hpg = inf_ser[i] + 0.01, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_inf
  
  plot(inf_ser, owning_excess_inf, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Inflation With Spread")
  
  # Return
  
  ret_ser <- seq(0,0.2,0.01)
  owning_excess_ret <- vector(length = length(ret_ser))
  
  for (i in 1:length(ret_ser)) {
    
    owning_excess_ret[i] <- owning_excess(inf = 0.02, ret = ret_ser[i], ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_ret
  
  plot(ret_ser, owning_excess_ret, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Portfolio Return")
  
  # Interest Rate
  
  ir_ser <- seq(0,0.2,0.01)
  owning_excess_ir <- vector(length = length(ir_ser))
  
  for (i in 1:length(ir_ser)) {
    
    owning_excess_ir[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = ir_ser[i], hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_ir
  
  plot(ir_ser, owning_excess_ir, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Interest Rate")
  
  # Interest Rate (with other growth rates as spread)
  
  ir_ser <- seq(0,0.2,0.01)
  owning_excess_ir <- vector(length = length(ir_ser))
  
  for (i in 1:length(ir_ser)) {
    
    owning_excess_ir[i] <- owning_excess(inf = 0.02, ret = ir_ser[i] + 0.04, ir = ir_ser[i], hpg = ir_ser[i] + 0.01, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_ir
  
  plot(ir_ser, owning_excess_ir, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Interest Rate With Spread")
  
  # House Price Growth
  
  hpg_ser <- seq(0,0.2,0.01)
  owning_excess_hpg <- vector(length = length(hpg_ser))
  
  for (i in 1:length(hpg_ser)) {
    
    owning_excess_hpg[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = hpg_ser[i], rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_hpg
  
  plot(hpg_ser, owning_excess_hpg, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "House Price Growth")
  
  # Rent
  
  rent_ser <- seq(100,10000,100)
  owning_excess_rent <- vector(length = length(rent_ser))
  
  for (i in 1:length(rent_ser)) {
    
    owning_excess_rent[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = rent_ser[i], res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_rent
  
  plot(rent_ser, owning_excess_rent, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Rent")
  
  # Renter Saving
  
  rent_sav_ser <- seq(0,1,0.01)
  owning_excess_rent_sav <- vector(length = length(rent_sav_ser))
  
  for (i in 1:length(rent_sav_ser)) {
    
    owning_excess_rent_sav[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = rent_sav_ser[i], val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_rent_sav
  
  plot(rent_sav_ser, owning_excess_rent_sav, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Renter Saving")
  
  # House Value
  
  val_ser <- seq(100000,2000000,50000)
  owning_excess_val <- vector(length = length(val_ser))
  
  for (i in 1:length(val_ser)) {
    
    owning_excess_val[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = val_ser[i], uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_val
  
  plot(val_ser, owning_excess_val, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "House Value")
  
  # House Value Vs. Rent
  
  val_rent_ser <- seq(100000,2000000,50000)
  owning_excess_val_rent <- vector(length = length(val_rent_ser))
  
  for (i in 1:length(val_rent_ser)) {
    
    owning_excess_val_rent[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = (1885/635000) * val_ser[i], res = 1, val = val_ser[i], uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_val_rent
  
  plot(val_rent_ser, owning_excess_val_rent, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Proportional House Value")
  
  # Upfront Costs
  
  uc_ser <- seq(0,50000,1000)
  owning_excess_uc <- vector(length = length(uc_ser))
  
  for (i in 1:length(uc_ser)) {
    
    owning_excess_uc[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = uc_ser[i], oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_uc
  
  plot(uc_ser, owning_excess_uc, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Upfront Costs")
  
  # Ongoing Costs
  
  oc_ser <- seq(0,0.05,0.005)
  owning_excess_oc <- vector(length = length(oc_ser))
  
  for (i in 1:length(oc_ser)) {
    
    owning_excess_oc[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = oc_ser[i], ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_oc
  
  plot(oc_ser, owning_excess_oc, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Ongoing Costs")
  
  # LTV
  
  ltv_ser <- seq(0,1,0.01)
  owning_excess_ltv <- vector(length = length(ltv_ser))
  
  for (i in 1:length(ltv_ser)) {
    
    owning_excess_ltv[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = ltv_ser[i], tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_ltv
  
  plot(ltv_ser, owning_excess_ltv, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "LTV")
  
  # Term (with fixed term being half)
  
  term_ser <- seq(1,30,1)
  owning_excess_term <- vector(length = length(term_ser))
  
  for (i in 1:length(term_ser)) {
    
    owning_excess_term[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = term_ser[i], fix_term = 0.5 * term_ser[i], fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_term
  
  plot(term_ser, owning_excess_term, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Term")
  
  # Fixed Term
  
  term_fix_ser <- seq(0,20,1)
  owning_excess_term_fix <- vector(length = length(term_fix_ser))
  
  for (i in 1:length(term_fix_ser)) {
    
    owning_excess_term_fix[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = term_fix_ser[i], fix_rate = 0.0269, fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_term_fix
  
  plot(term_fix_ser, owning_excess_term_fix, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Fixed Term")
  
  # Fixed Rate
  
  rate_fix_ser <- seq(0,0.25,0.005)
  owning_excess_rate_fix <- vector(length = length(rate_fix_ser))
  
  for (i in 1:length(rate_fix_ser)) {
    
    owning_excess_rate_fix[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = rate_fix_ser[i], fix_pay = 2568, svr_prem = 0.0159)
    
  }
  
  owning_excess_rate_fix
  
  plot(rate_fix_ser, owning_excess_rate_fix, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Fixed Rate")
  
  # Fixed Payments
  
  pay_fix_ser <- seq(100,5000,100)
  owning_excess_pay_fix <- vector(length = length(pay_fix_ser))
  
  for (i in 1:length(pay_fix_ser)) {
    
    owning_excess_pay_fix[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 10, fix_rate = 0.0269, fix_pay = pay_fix_ser[i], svr_prem = 0.0159)
    
  }
  
  owning_excess_pay_fix
  
  plot(pay_fix_ser, owning_excess_pay_fix, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Fixed Payment")
  
  # Variable Rate
  
  rate_var_ser <- seq(0,0.25,0.005)
  owning_excess_rate_var <- vector(length = length(rate_var_ser))
  
  for (i in 1:length(rate_var_ser)) {
    
    owning_excess_rate_var[i] <- owning_excess(inf = 0.02, ret = 0.06, ir = 0.02, hpg = 0.03, rent = 1885, res = 1, val = 635000, uc = 4250, oc = 0.01, ltv = 0.75, tot_term = 20, fix_term = 0, fix_rate = 0.0269, fix_pay = 2568, svr_prem = rate_var_ser[i])
    
  }
  
  owning_excess_rate_var
  
  plot(rate_var_ser, owning_excess_rate_var, type = 'l', col = "#0c9300", ylab = "", xlab = "", main = "Variable Rate")
  
# Stochastic analysis

  # All Variables
    
  trials <- 100000
  
  owning_excess_stoch_all <- vector(length = trials)
    
  for (n in 1:trials) {
    
    # Economic variables
    
    inf <- runif(1, 0, 0.2)
    ret <- runif(1, 0, 0.2)
    ir <- runif(1, 0, 0.2)
    hpg <-  runif(1, 0, 0.2)
    
    # Renter variables
    
    rent <- runif(1, 0, 10000)
    res <- runif(1, 0, 1)
    
    # Housing variables
    
    val <- runif(1, 100000, 2000000)
    uc <- runif(1, 0, 50000)
    oc <- runif(1, 0, 0.05)
    
    # Mortgage variables
    
    ltv <- runif(1, 0, 1)
    tot_term <- sample.int(n = 30, size = 1)
    fix_term <- sample.int(n = tot_term, size = 1)
    fix_rate <- runif(1, 0, 0.25)
    fix_pay <- runif(1, 0, 5000)
    svr_prem <- runif(1, 0, 0.25)
    
    # Generating POE
    
    owning_excess_stoch_all[n] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
    
  }
  
  mean(owning_excess_stoch_all)
  median(owning_excess_stoch_all)
  min(owning_excess_stoch_all)
  max(owning_excess_stoch_all)
  
  hist(owning_excess_stoch_all, freq = F, col = "#0c9300", ylab = "", xlab = "", main = "All Variables Stochastic")
  
  # All variables adjusted
  
  trials <- 100000
  
  owning_excess_stoch_all_adj <- vector(length = trials)
  
  for (n in 1:trials) {
    
    # Economic variables
    
    inf <- runif(1, 0, 0.2)
    ret <- inf + 0.025
    ir <- inf
    hpg <-  inf + 0.025
    
    # Housing variables
    
    val <- runif(1, 100000, 2000000)
    uc <- runif(1, 0, 50000)
    oc <- runif(1, 0, 0.05)
    
    # Renter variables
    
    rent <- (1885/635000) * val
    res <- runif(1, 0, 1)
    
    # Mortgage variables
    
    ltv <- runif(1, 0, 1)
    tot_term <- sample.int(n = 30, size = 1)
    fix_term <- sample.int(n = tot_term, size = 1)
    fix_rate <- runif(1, 0, 0.25)
    fix_pay <- runif(1, 0, 5000)
    svr_prem <- runif(1, 0, 0.1)
    
    # Generating POE
    
    owning_excess_stoch_all_adj[n] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
    
  }
  
  mean(owning_excess_stoch_all_adj)
  median(owning_excess_stoch_all_adj)
  min(owning_excess_stoch_all_adj)
  max(owning_excess_stoch_all_adj)
  
  hist(owning_excess_stoch_all_adj, freq = F, col = "#0c9300", ylab = "", xlab = "", main = "All Variables Stochastic (Adjusted)")
  
  # All variables adjusted - just unkown
  
  trials <- 100000
  
  owning_excess_stoch_all_adj_un <- vector(length = trials)
  
  for (n in 1:trials) {
    
    # Economic variables
    
    inf <- runif(1, 0, 0.2)
    ret <- inf + 0.025
    ir <- inf
    hpg <-  inf + 0.025
    
    # Housing variables
    
    val <- 635000
    uc <- 4250
    oc <- runif(1, 0, 0.05)
    
    # Renter variables
    
    rent <- 1885
    res <- runif(1, 0, 1)
    
    # Mortgage variables
    
    ltv <- 0.75
    tot_term <- 20
    fix_term <- 10
    fix_rate <- 0.0269
    fix_pay <- 2568
    svr_prem <- runif(1, 0, 0.1)
    
    # Generating POE
    
    owning_excess_stoch_all_adj_un[n] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
    
  }
  
  mean(owning_excess_stoch_all_adj_un)
  median(owning_excess_stoch_all_adj_un)
  min(owning_excess_stoch_all_adj_un)
  max(owning_excess_stoch_all_adj_un)
  
  hist(owning_excess_stoch_all_adj_un, freq = F, col = "#0c9300", ylab = "", xlab = "", main = "Unknown Stochastic")
  
  # Economic variables
  
  trials <- 100000
  
  owning_excess_stoch_all_adj_jec <- vector(length = trials)
  
  for (n in 1:trials) {
    
    # Economic variables
    
    inf <- runif(1, 0, 0.2)
    ret <- runif(1, 0, 0.2)
    ir <- runif(1, 0, 0.2)
    hpg <-  runif(1, 0, 0.2)
    
    # Housing variables
    
    val <- 635000
    uc <- 4250
    oc <- 0.01
    
    # Renter variables
    
    rent <- 1885
    res <- 0.8
    
    # Mortgage variables
    
    ltv <- 0.75
    tot_term <- 20
    fix_term <- 10
    fix_rate <- 0.0269
    fix_pay <- 2568
    svr_prem <- 0.025
    
    # Generating POE
    
    owning_excess_stoch_all_adj_jec[n] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
    
  }
  
  mean(owning_excess_stoch_all_adj_jec)
  median(owning_excess_stoch_all_adj_jec)
  min(owning_excess_stoch_all_adj_jec)
  max(owning_excess_stoch_all_adj_jec)
  
  hist(owning_excess_stoch_all_adj_jec, freq = F, col = "#0c9300", ylab = "", xlab = "", main = "Economic Variables Stochastic")
  
  # Economic variables as inflation spread
  
  trials <- 100000
  
  owning_excess_stoch_all_adj_jec <- vector(length = trials)
  
  for (n in 1:trials) {
    
    # Economic variables
    
    inf <- runif(1, 0, 0.2)
    ret <- inf + 0.025
    ir <- inf
    hpg <-  inf + 0.025
    
    # Housing variables
    
    val <- 635000
    uc <- 4250
    oc <- 0.01
    
    # Renter variables
    
    rent <- 1885
    res <- 0.8
    
    # Mortgage variables
    
    ltv <- 0.75
    tot_term <- 20
    fix_term <- 10
    fix_rate <- 0.0269
    fix_pay <- 2568
    svr_prem <- 0.025
    
    # Generating POE
    
    owning_excess_stoch_all_adj_jec[n] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
    
  }
  
  mean(owning_excess_stoch_all_adj_jec)
  median(owning_excess_stoch_all_adj_jec)
  min(owning_excess_stoch_all_adj_jec)
  max(owning_excess_stoch_all_adj_jec)
  
  hist(owning_excess_stoch_all_adj_jec, freq = F, col = "#0c9300", ylab = "", xlab = "", main = "Inflation Spread")
  
  # Economic variables return spread
  
  trials <- 100000
  
  owning_excess_stoch_all_adj_jec <- vector(length = trials)
  
  for (n in 1:trials) {
    
    # Economic variables
    
    inf <- runif(1, 0, 0.2)
    ret <- inf + 0.05
    ir <- inf
    hpg <- inf
    
    # Housing variables
    
    val <- 635000
    uc <- 4250
    oc <- 0.01
    
    # Renter variables
    
    rent <- 1885
    res <- 0.8
    
    # Mortgage variables
    
    ltv <- 0.75
    tot_term <- 20
    fix_term <- 10
    fix_rate <- 0.0269
    fix_pay <- 2568
    svr_prem <- 0.025
    
    # Generating POE
    
    owning_excess_stoch_all_adj_jec[n] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
    
  }
  
  mean(owning_excess_stoch_all_adj_jec)
  median(owning_excess_stoch_all_adj_jec)
  min(owning_excess_stoch_all_adj_jec)
  max(owning_excess_stoch_all_adj_jec)
  
  hist(owning_excess_stoch_all_adj_jec, freq = F, col = "#0c9300", ylab = "", xlab = "", main = "Return Spread")
  
  # Economic variables disconnected high inflation
  
  trials <- 100000
  
  owning_excess_stoch_all_adj_jec <- vector(length = trials)
  
  for (n in 1:trials) {
    
    # Economic variables
    
    inf <- 0.2
    ret <- runif(1, 0, 0.2)
    ir <- runif(1, 0, 0.2)
    hpg <- runif(1, 0, 0.2)
    
    # Housing variables
    
    val <- 635000
    uc <- 4250
    oc <- 0.01
    
    # Renter variables
    
    rent <- 1885
    res <- 0.8
    
    # Mortgage variables
    
    ltv <- 0.75
    tot_term <- 20
    fix_term <- 10
    fix_rate <- 0.0269
    fix_pay <- 2568
    svr_prem <- 0.025
    
    # Generating POE
    
    owning_excess_stoch_all_adj_jec[n] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
    
  }
  
  mean(owning_excess_stoch_all_adj_jec)
  median(owning_excess_stoch_all_adj_jec)
  min(owning_excess_stoch_all_adj_jec)
  max(owning_excess_stoch_all_adj_jec)
  
  hist(owning_excess_stoch_all_adj_jec, freq = F, col = "#0c9300", ylab = "", xlab = "", main = "Disconnected High Inflation")
  
  # Economic variables disconnected high ir
  
  trials <- 100000
  
  owning_excess_stoch_all_adj_jec <- vector(length = trials)
  
  for (n in 1:trials) {
    
    # Economic variables
    
    inf <- runif(1, 0, 0.2)
    ret <- inf + 0.025
    ir <- 0.2
    hpg <- inf + 0.025
    
    # Housing variables
    
    val <- 635000
    uc <- 4250
    oc <- 0.01
    
    # Renter variables
    
    rent <- 1885
    res <- 0.8
    
    # Mortgage variables
    
    ltv <- 0.75
    tot_term <- 20
    fix_term <- 10
    fix_rate <- 0.0269
    fix_pay <- 2568
    svr_prem <- 0.025
    
    # Generating POE
    
    owning_excess_stoch_all_adj_jec[n] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
    
  }
  
  mean(owning_excess_stoch_all_adj_jec)
  median(owning_excess_stoch_all_adj_jec)
  min(owning_excess_stoch_all_adj_jec)
  max(owning_excess_stoch_all_adj_jec)
  
  hist(owning_excess_stoch_all_adj_jec, freq = F, col = "#0c9300", ylab = "", xlab = "", main = "Disconnected High IR")
  
  
  # Making a return chart
  
    # Economic variables function of stochastic economic variables - returns
    
    trials <- 1000
    
    owning_excess_stoch_ret <- matrix(nrow = trials, ncol = length(ret_ser))
    
    owning_excess_stoch_inf_av <- vector(length = length(ret_ser))
    
    for (j in 1:length(ret_ser)) {
    
    for (n in 1:trials) {
      
      # Economic variables
      
      inf <- runif(1, 0, 0.2)
      ret <- ret_ser[j]
      ir <- runif(1, 0, 0.2)
      hpg <- runif(1, 0, 0.2)
      
      # Housing variables
      
      val <- 635000
      uc <- 4250
      oc <- 0.01
      
      # Renter variables
      
      rent <- 1885
      res <- 0.8
      
      # Mortgage variables
      
      ltv <- 0.75
      tot_term <- 20
      fix_term <- 10
      fix_rate <- 0.0269
      fix_pay <- 2568
      svr_prem <- 0.025
      
      # Generating POE
      
      owning_excess_stoch_ret[n, j] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
      
    }
      owning_excess_stoch_inf_av[j] <- mean(owning_excess_stoch_ret[, j])
      
      }
    
    plot(ret_ser, owning_excess_stoch_inf_av, type = 'l')
    
    # Economic variables function of stochastic economic variables - ir
    
    trials <- 1000
    
    owning_excess_stoch_ir <- matrix(nrow = trials, ncol = length(ir_ser))
    
    owning_excess_stoch_ir_av <- vector(length = length(ir_ser))
    
    for (j in 1:length(ir_ser)) {
      
      for (n in 1:trials) {
        
        # Economic variables
        
        inf <- runif(1, 0, 0.2)
        ret <- runif(1, 0, 0.2)
        ir <- ir_ser[j]
        hpg <- runif(1, 0, 0.2)
        
        # Housing variables
        
        val <- 635000
        uc <- 4250
        oc <- 0.01
        
        # Renter variables
        
        rent <- 1885
        res <- 0.8
        
        # Mortgage variables
        
        ltv <- 0.75
        tot_term <- 20
        fix_term <- 10
        fix_rate <- 0.0269
        fix_pay <- 2568
        svr_prem <- 0.025
        
        # Generating POE
        
        owning_excess_stoch_ir[n, j] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
        
      }
      owning_excess_stoch_ir_av[j] <- mean(owning_excess_stoch_ir[, j])
      
    }
    
    plot(ir_ser, owning_excess_stoch_ir_av, type = 'l')
    
    
    # Economic variables function of stochastic economic variables - hpg
    
    trials <- 1000
    
    owning_excess_stoch_hpg <- matrix(nrow = trials, ncol = length(hpg_ser))
    
    owning_excess_stoch_hpg_av <- vector(length = length(hpg_ser))
    
    for (j in 1:length(hpg_ser)) {
      
      for (n in 1:trials) {
        
        # Economic variables
        
        inf <- runif(1, 0, 0.2)
        ret <- runif(1, 0, 0.2)
        ir <- runif(1, 0, 0.2)
        hpg <- hpg_ser[j]
        
        # Housing variables
        
        val <- 635000
        uc <- 4250
        oc <- 0.01
        
        # Renter variables
        
        rent <- 1885
        res <- 0.8
        
        # Mortgage variables
        
        ltv <- 0.75
        tot_term <- 20
        fix_term <- 10
        fix_rate <- 0.0269
        fix_pay <- 2568
        svr_prem <- 0.025
        
        # Generating POE
        
        owning_excess_stoch_hpg[n, j] <- owning_excess(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem)
        
      }
      
      owning_excess_stoch_hpg_av[j] <- mean(owning_excess_stoch_hpg[, j])
      
    }
    
    plot(ir_ser, owning_excess_stoch_hpg_av, type = 'l')
    
    # Comparing the three
    
    # Making it easier to work with in ggplot
    
    test <- cbind(owning_excess_stoch_hpg_av, owning_excess_stoch_ir_av, owning_excess_stoch_inf_av)
    test <- as.data.frame(test)
    
    three_ratio_comp_n_alt <- cbind(ret_ser, test)
    colnames(three_ratio_comp_n_alt) <- c("Rate", "HPG", "IR", "PR")
    three_ratio_comp_n_alt
    
    three_ratio_comp_n_long_alt <- melt(three_ratio_comp_n_alt, id = "Rate")  # convert to long format
    three_ratio_comp_n_long_alt
    
    ggplot(data = three_ratio_comp_n_long_alt, aes(x = Rate, y = value, color = variable)) +
      geom_line() + scale_colour_manual(values = c("#0c9300","#cc0000","#007bf0")) +
      theme(legend.title = element_blank()) +
      theme(legend.text = element_text(family = "serif", colour = "black")) +
      labs(y = "POE", x = "Rate") +
      theme(axis.text.x = element_text(family = "serif", colour = "black")) +
      theme(axis.text.y = element_text(family = "serif", colour = "black")) +
      theme(axis.title.x = element_text(family = "serif", colour = "black", size = 12)) +
      theme(axis.title.y = element_text(family = "serif", colour = "black", size = 12)) +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(panel.grid.major.y = element_line(colour = "black", size = 0.1)) +
      theme(axis.line = element_line(colour = "black", size = 0.3)) +
      theme(axis.ticks = element_line(colour = "black"))
      # scale_y_continuous(label=comma)
