#------------3 Regressions-----------

install.packages("lmtest")
install.packages("sandwich")
library("lmtest")
library("sandwich")

library(read.csv)
data <- read.csv("20201216.csv")

df = data.frame(data)
print(df)

print_by_year <- function(df, fyear, sic) {
  df = df[order(df$fyear), ]
  df = df[df$SIC2 == sic,]
  print(df[df$fyear==fyear,])
}

#dataframe_for_regression

df_reg=print_by_year(df, 2018 , 54)

#variables

y = df_reg$ta_att
x1 = df_reg$recipro_att
x2 = df_reg$revrec_att
x3 = df_reg$ppe_att
x4 = df_reg$roa
x5 = df_reg$tat_attt

#regression_MODELA

regA = lm(y ~ x1 + x2 + x3)

summary(regA)

#export regression results to excel csv file

capture.output(summary(regB), file="regA1988.csv")

#regression_MODELB

regB = lm(y ~ x1 + x2 + x3 + x4)

summary(regB)

#export regression results to excel csv file

capture.output(summary(regB), file="regA1988.csv")

#regression_MODELC 

regC = lm(y ~ x1 + x2 + x3 + x4 + x5)

summary(regC)

#export regression results to excel csv file
capture.output(summary(regC), file="regC1988.csv")


