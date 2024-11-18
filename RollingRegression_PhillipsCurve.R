#Phillips curve rolling regression with a window size of 20 quarters and time-series plot of beta coefficient

# Load necessary libraries
library(tidyverse)
library(readxl)
library(ggplot2)

# Import dataset
data <- read_excel(""C:/Users/ishma/Downloads/pc_data.xlsx")

# Dependent and independent variables
log_prices <- log(data$prices) 
UNEMP <- data$UNEMP

# Initialize results in DataFrame
results <- data.frame(Time = numeric(),
                      Beta = numeric(),
                      Lower.95 = numeric(),
                      Upper.95 = numeric())

# Rolling regression loop with window size of 20
window_size <- 20
n <- nrow(data)

for (i in 1:(n - window_size + 1)) {
  # Subset data for the rolling window
  subset_data <- data[i:(i + window_size - 1), ]
  OLS_model <- lm(log_prices ~ UNEMP, data = subset_data)
  
  # beta coefficient and confidence intervals
  beta <- coef(OLS_model)["UNEMP"]
  confint_vals <- confint(OLS_model, "UNEMP", level = 0.95)
  
  # Appending results
  results <- rbind(results, data.frame(Time = i + window_size - 1,
                                       Beta = beta,
                                       Lower.95 = confint_vals[1],
                                       Upper.95 = confint_vals[2]))
}

# Coefficient plots
ggplot(results, aes(x = Time)) +
  geom_line(aes(y = Beta, color = "Beta Coefficient"), size = 1) +
  geom_ribbon(aes(ymin = Lower.95, ymax = Upper.95), fill = "black", alpha = 0.2) +
  scale_color_manual(name = "Legend", values = c("Beta Coefficient" = "red")) +
  ggtitle("Rolling Regression: Beta Coefficient with 95% Confidence Bounds") +
  xlab("Time") +
  ylab("Beta Coefficient") +
  theme_minimal()
