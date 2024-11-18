#------------------US CPI AND COMPONENTS MONTHLY UPDATE CODE----------------------------------

#Install packages
#install.packages("fredr")
#install.packages("ggplot2")
#install.packages("taskscheduleR")
#install.packages("dplyr")

#Import Packages
library(fredr)
library(ggplot2)
library(taskscheduleR)
library(dplyr)
library(tidyr)

#FRED API
fredr_set_key("497386db517c7bc2487e16bc06de700a")

# Define the CPI series to fetch
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

# Combine the data into one dataframe
cpi_data_combined <- bind_rows(cpi_data_list)

# Reshape the dataframe
cpi_data_wide <- cpi_data_combined %>%
  select(date, series_id, value) %>%
  pivot_wider(names_from = series_id, values_from = value)

# Save the final data to a CSV file
write.csv(cpi_data_wide, "C:/Users/ishma/Downloads/RUpdates/CPI_US.csv", row.names = FALSE)
