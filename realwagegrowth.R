# Load the packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)
library(RColorBrewer)

# Load wage data
wage_data <- read_excel("63450Table2ato9a.xlsx", sheet = "Data1")
melb_wage_data <- wage_data[,6]
melb_wage_data$Month <- wage_data[1]
melb_wage_data <- melb_wage_data[10:35,]

# Extract the date values and convert to Date type
data_dates <- melb_wage_data[[2]] %>%
  pull(...1) %>%
  as.numeric() %>%
  as.Date(origin = "1899-12-30")

# Add back the correctly formatted date column
melb_wage_data$Date <- data_dates

# Reorder to have Date first
melb_wage_data <- melb_wage_data %>%
  select(Date, everything())

melb_wage_data <- melb_wage_data[8:26,1:2] #start 2005

# Rename the columns
colnames(melb_wage_data)[2] <- "WPI"

# Load CPI data
CPI_data <- read_excel("640103.xlsx", sheet = "Data1")

melb_CPI_data <- CPI_data[,25]
melb_CPI_data$Month <- CPI_data[1]
melb_CPI_data <- melb_CPI_data[10:312,]
melb_CPI_data <- melb_CPI_data[228:303,]

# Extract the date values and convert to Date type
data_dates <- melb_CPI_data[[2]] %>%
  pull(...1) %>%
  as.numeric() %>%
  as.Date(origin = "1899-12-30")

# Add back the correctly formatted date column
melb_CPI_data$Date <- data_dates

# Reorder to have Date first
melb_CPI_data <- melb_CPI_data %>%
  select(Date, everything())

# Filter for June of each year
melb_CPI_data <- melb_CPI_data %>%
  filter(format(Date, "%m") == "06")

# Rename the columns
colnames(melb_CPI_data)[2] <- "CPI"
melb_CPI_data <- melb_CPI_data[,1:2]

# Combine the data
combined_data <- melb_wage_data %>%
  inner_join(melb_CPI_data, by = "Date")

# Convert columns to numeric
safe_numeric <- function(x) {
  as.numeric(gsub(",", "", x))
}

combined_data <- combined_data %>%
  mutate(
    WPI = safe_numeric(WPI),
    CPI = safe_numeric(CPI)
  )

# Calculate Real Wage Growth (WPI adjusted for CPI)
combined_data <- combined_data %>%
  mutate(Real_Wage_Growth = WPI / CPI * 100)

# Visualize the data
ggplot(combined_data, aes(x = Date)) +
  geom_line(aes(y = WPI, color = "WPI"), size = 1.2) +
  geom_line(aes(y = CPI, color = "CPI"), size = 1.2) +
  geom_line(aes(y = Real_Wage_Growth, color = "Real Wage Growth"), size = 1.2, linetype = "dashed") +
  labs(
    title = "Comparison of Wage Price Index (WPI) and Consumer Price Index (CPI) in Melbourne",
    x = "Year",
    y = "Index (2005 = 100)",
    color = "Index"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::number_format()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )