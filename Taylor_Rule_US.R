#------------------US Forward Looking Taylor Rule Model----------------------------------

library(fredr)
library(ggplot2)
library(taskscheduleR)
library(dplyr)
library(tidyr)
library(mFilter)
library(quantmod)
library(readr)


rm(list = ls())

#import data
data <- read_csv("dataset.csv")
cpi <-  data$CPIAUCSL_PC1
ffr <- data$FEDFUNDS
exp_inf <- data$EXPINF1YR
rgdp <- data$GDPC1

# Data transformations
l_r_gdp <- log(rgdp)  
realgdp_hp <- hpfilter(l_r_gdp, freq = 1600)   # apply HP filter to log GDP
l_realgdp_trend <- realgdp_hp$trend            # Trend log GDP

# Parameters
g1 <- 0.7
g2 <- 3
g3 <- 1
inf_target <- 2.4

# Construct gaps and real rate
inf_gap <- exp_inf - inf_target           
out_gap <- l_r_gdp - l_realgdp_trend
real_policy_rate <- ffr - exp_inf

# HP filter for real policy rate
real_nominal_rate_hp <- hpfilter(real_policy_rate, freq = 1600) 
real_nominal_rate_trend <- real_nominal_rate_hp$trend  

# Lagged FFR
ffr_lagged <- stats::lag(ffr, k = 1)   # 1-period lag

# Forward-looking Taylor Rule
tr_rate <- (g1 * ffr_lagged) + 
  (1 - g1) * (real_nominal_rate_trend + exp_inf + g2 * inf_gap + g3 * out_gap)

tail(tr_rate)



