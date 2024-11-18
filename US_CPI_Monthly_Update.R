#--------RCode for monthly and yearly CPI level and growth rates, by CPI components-----------

# libraries
library(fredr)
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)

# FRED API key
fredr_set_key("497386db517c7bc2487e16bc06de700a")

# Define CPI series to fetch
series_ids <- c("CPIAUCSL", "CUSR0000SAN","CPIUFDSL", "CUSR0000SETB01", "CUUR0000SEGB", 
                "CUSR0000SAD", "CUSR0000SETA02", "CUUR0000SETA01", "CUUR0000SETC",
                "CUSR0000SAS", "CUUR0000SEHA", "CPRPTT02GBM661N", "CUUR0000SEHD",
                "CUUR0000SETG", "CUUR0000SEGC", "CUSR0000SAM2", "CUSR0000SEHF02",
                "CPILFESL", "CUUR0000SA0L2", "TRMMEANCPIM158SFRBCLE", 
                "CUSR0000SASLE", "CUUR0000SASL2RS")

# Fetch data for all series
cpi_data_list <- lapply(series_ids, function(series_id) {
  fredr(
    series_id = series_id, 
    observation_start = as.Date("2000-01-01"),
    observation_end = Sys.Date(),
    frequency = "m"
  )
})

# Combine data into dataframe
cpi_data_combined <- bind_rows(cpi_data_list)

# Reshape dataframe
cpi_data_wide <- cpi_data_combined %>%
  select(date, series_id, value) %>%
  pivot_wider(names_from = series_id, values_from = value)

# MoM and YoY
cpi_data_with_growth <- cpi_data_wide %>%
  arrange(date) %>%
  mutate(across(-date, list(
    monthly_growth = ~ (./lag(.) - 1) * 100,
    yearly_growth = ~ (./lag(., 12) - 1) * 100
  ), .names = "{.col}_{.fn}"))

# Split series into seprate dataframes for Excel sheets
level_data <- cpi_data_wide
monthly_growth <- cpi_data_with_growth %>%
  select(date, ends_with("_monthly_growth"))
yearly_growth <- cpi_data_with_growth %>%
  select(date, ends_with("_yearly_growth"))

# Write to xlsx file with multiple sheets
output_file <- "C:/Users/ishma/Downloads/RUpdates/CPI_US.xlsx"
wb <- createWorkbook()
addWorksheet(wb, "Level Data")
addWorksheet(wb, "Monthly Growth Rates")
addWorksheet(wb, "Yearly Growth Rates")
writeData(wb, "Level Data", level_data)
writeData(wb, "Monthly Growth Rates", monthly_growth)
writeData(wb, "Yearly Growth Rates", yearly_growth)
saveWorkbook(wb, output_file, overwrite = TRUE)
