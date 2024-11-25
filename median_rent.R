# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Load the data
median_rent <- read.csv("medianrent.csv")
colnames(median_rent)[1] <- "Area"

# Remove dollar signs and convert to numeric
median_rent_clean <- median_rent %>%
  mutate(across(-Area, ~ as.numeric(gsub("\\$", "", .))))

# Reshape the data to long format and filter from 2010 onwards
median_rent_long <- median_rent_clean %>%
  pivot_longer(
    cols = -Area,
    names_to = "Quarter",
    values_to = "MedianRent"
  ) %>%
  mutate(
    Quarter = as.Date(paste0(Quarter, "-01"), format = "%b.%y-%d")
  ) %>%
  filter(Quarter >= as.Date("2010-01-01"))

# Get the last data point for each area to place the labels
labels <- median_rent_long %>%
  group_by(Area) %>%
  filter(Quarter == max(Quarter))

# Plot all suburbs in one plot with labels and highlight COVID-19 period
ggplot(median_rent_long, aes(x = Quarter, y = MedianRent, color = Area)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2022-12-01"), ymin = -Inf, ymax = Inf, alpha = 0.32, fill = "red") +
  annotate("text", x = as.Date("2021-06-01"), y = Inf, label = "COVID-19 Pandemic", vjust = 2,, hjust = 0.45, color = "red", size = 2.5, angle = 0) +  # Add text label
  geom_line(alpha = 0.93) +
  geom_text_repel(data = labels, aes(label = Area),
                  nudge_x = 40,  # Adjust this value to nudge labels further to the right
                  hjust = 0,
                  direction = "y",
                  segment.color = NA,
                  size = 3) +  # Adjust the size of the text if needed
  labs(
    title = "Quarterly Median Rent Trends by Area in Melbourne (2010 Onwards)",
    x = "Quarter",
    y = "Median Rent (AUD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove the legend
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  expand_limits(x = max(median_rent_long$Quarter) + 1200)  # Add margin to the right