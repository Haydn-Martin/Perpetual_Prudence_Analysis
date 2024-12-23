
# Owner Excess Function for UK home owners vs. renters

# Variable definitions:

# inf = annual inflation
# ret = CAGR of portfolio
# ir = annual national bank interest rate 
# hpg = CAGR of house prices
# rent = monthly rent
# res = renter excess saved (what % of the monthly excess the renter enjoys that they save)
# val = house value
# uc = upfront cost of owning
# oc = ongoing cost of ownership as a proportion of house value
# ltv = loan-to-value ratio
# tot_term = total mortgage term length (in years)
# fix_term = total fixed rate mortgage term length (in years)
# fix_rate = annual fixed mortgage rate (monthly rate * 12)
# fix_pay = fixed monthly payment
# svr_prem = excess over the interest rate applied to variable interest rate


owning_excess <- function(inf, ret, ir, hpg, rent, res, val, uc, oc, ltv, tot_term, fix_term, fix_rate, fix_pay, svr_prem){
  
  # Extra input variables
  # Used for calculations, the same for all home owners/renters
  
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
  
  deposit <- (1-ltv) * val # house deposit
  mort <- val - deposit # mortgage value
  var_term <- tot_term - fix_term # variable term
  var_rate <- svr_prem + ir # variable rate
  
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
