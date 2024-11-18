#------------------US Taylor Rule Model----------------------------------

library(fredr)
library(ggplot2)
library(taskscheduleR)
library(dplyr)
library(tidyr)
library(mFilter)
library(quantmod)
library(readr)


setwd("")
rm(list = ls())

#import data
data <- read_csv("dataset.csv")
cpi <-  data$CPIAUCSL_PC1
ffr <- data$FEDFUNDS
exp_inf <- data$EXPINF1YR
rgdp <- data$GDPC1

# Data transformations
l_r_gdp <- log(rgdp)  # Log of GDP
realgdp_hp <- hpfilter(rgdp, freq = 1600)
l_realgdp_trend <- log(realgdp_hp$trend)  # Log of Trend GDP

#Parameters and variables
g1 = 0.8
g2 = 3
g3 = 1
inf_target = 2.4
nom_rate = real_nominal_rate_trend+exp_inf
inf_gap = exp_inf - inf_target           
out_gap = l_r_gdp - l_realgdp_trend
real_policy_rate = ffr - exp_inf
real_nominal_rate_hp <- hpfilter(real_policy_rate, freq = 1600) 
real_nominal_rate_trend <- real_nominal_rate_hp$trend  # Log of Trend GDP
ffr_lagged <- stats::lag(ffr, k = -1) 

#Forward-looking Taylor Rule
tr_rate <- (g1 * ffr_lagged) + (1 - g1) * ((real_nominal_rate_trend + exp_inf + g2 * (inf_gap) + g3 * (out_gap)))
tail(tr_rate)




