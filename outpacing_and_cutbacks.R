# Load the packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)  # For better axis labeling
library(RColorBrewer)  # For better color palettes

# Read the "Data1" sheet from "640103.xlsx"
data <- read_excel("640103.xlsx", sheet = "Data1")

melb_data <- data[,14:25]
melb_data$Month <- data[1]
melb_data <- melb_data[10:312,]
melb_data <- melb_data[228:303,]

# Extract the date values and convert to Date type
data_dates <- melb_data[[13]] %>%
  pull(...1) %>%
  as.numeric() %>%
  as.Date(origin = "1899-12-30")

# Check the result
head(data_dates)
class(data_dates)

# Function to safely convert to numeric
safe_numeric <- function(x) {
  as.numeric(gsub(",", "", x))  # Remove any commas
}

# Apply to all columns except the last (date) column
melb_data <- melb_data %>%
  select(-13) %>%
  mutate(across(everything(), safe_numeric))

# Add back the correctly formatted date column
melb_data$Date <- data_dates

# Reorder to have Date first
melb_data<- melb_data %>%
  select(Date, everything())

# Rename columns for easier use
names(melb_data) <- c(
  "Date", "Food", "Alcohol", "Clothing", "Housing",
  "Furnishings", "Health", "Transport", "Communication",
  "Recreation", "Education", "Insurance", "CPI_All"
)

melb_data_long <- melb_data %>%
  select(-CPI_All) %>%
  pivot_longer(cols = -Date, names_to = "Category", values_to = "CPI")

#building blocks of melbourne's CPI

weights <- read.csv("weights.csv", skip = 1)
colnames(weights) <- c("Group","2019, Pre Covd","2023","2024","Percentage Points Change")
weights <- weights[0:11,]

weights[,1] <- c(
  "Food", "Alcohol", "Clothing", "Housing",
  "Furnishings", "Health", "Transport", "Communication",
  "Recreation", "Education", "Insurance"
)

#Which costs are outpaciing other in melbourne?
library(ggrepel)

melb_data_indexed <- melb_data %>% 
  mutate(across(-Date, ~ . / first(.) * 100, .names = "Index_{.col}"))

melb_data_indexed_long <- melb_data_indexed %>% 
  select(starts_with("Index_"), Date) %>% 
  pivot_longer(cols = starts_with("Index_"), names_to = "Category", values_to = "Index") %>%
  mutate(Category = sub("Index_", "", Category))

# Get the last data point for each category
last_points <- melb_data_indexed_long %>%
  group_by(Category) %>%
  filter(Date == max(Date))

# Calculate min and max y-values to set the plot limits
min_y <- 70  # Starting a bit below 100 to show some context
max_y <- 280

# Order categories by their last value for better label placement
category_order <- last_points %>%
  arrange(desc(Index)) %>%
  pull(Category)

melb_data_indexed_long$Category <- factor(melb_data_indexed_long$Category, levels = category_order)

# Load necessary libraries
library(ggplot2)
library(RColorBrewer)
library(scales)
library(ggrepel)

# Using Paired palette (n = 12) which provides 12 distinct colors
colors <- brewer.pal(n = 12, name = "Paired")
colors[11] <- "#FFD700"
# Assign the colors to the categories (ensure you have 11 categories)
category_colors <- setNames(colors[1:11], c("Alcohol", "Education", "Health", "Housing", "Food", "Furnishings", "Clothing", "Communication", "Insurance", "Transport", "Recreation"))

# Create the line plot
ggplot(melb_data_indexed_long, aes(x = Date, y = Index, color = Category)) + 
  geom_line(size = 1) + 
  geom_text_repel(data = last_points, aes(label = Category), size = 4, force = 6, max.overlaps = 20) +
  labs(title = "Which Costs Are Outpacing Others in Melbourne?", 
       subtitle = "All categories indexed to 100 at the start", 
       x = NULL, 
       y = "Growth Index (Start = 100)") +
  scale_y_continuous(
    labels = comma,
    limits = c(min_y, max_y),
    breaks = scales::breaks_extended(n = 8),
    minor_breaks = NULL
  ) +
  scale_x_date(expand = expansion(mult = c(0.02, 0.50))) +  # Add more space on the right
  scale_color_manual(values = category_colors) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin = margin(r = 80),  # Reduced right margin as we have more horizontal space
    panel.grid.minor = element_blank(),  # Remove minor grid lines for cleaner look
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )



######### breakdown of CPI ###########
# Filter the data for December 2023 and March 2024
recent_data <- melb_data %>%
  filter(Date %in% as.Date(c("2023-12-01", "2024-03-01")))

# Calculate the percentage change from December 2023 to March 2024
recent_data_change <- recent_data %>%
  pivot_longer(cols = -Date, names_to = "Category", values_to = "Value") %>%
  spread(Date, Value) %>%
  mutate(Change = (`2024-03-01` - `2023-12-01`) / `2023-12-01` * 100) %>%
  select(Category, Change)

# Order categories by the percentage change for better visual impact
recent_data_change <- recent_data_change %>%
  arrange(desc(Change))

# Define a better color palette using RColorBrewer
colors <- brewer.pal(n = 11, name = "Set3")

# Assign the colors to the categories
category_colors <- setNames(colors, c("Alcohol", "Education", "Health", "Housing", "Food", "Furnishings", "Clothing", "Communication", "Insurance", "Transport", "Recreation"))

# Create the bar chart
plot_change_recent <- ggplot(recent_data_change, aes(x = reorder(Category, Change), y = Change, fill = Category)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +  # Flip coordinates for a horizontal bar chart
  labs(title = "Breakdown of CPI Categories in Melbourne", 
       subtitle = "Percentage Change from December 2023 to March 2024", 
       x = "Category", 
       y = "Percentage Change (%)") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Format y-axis as percentages
  scale_fill_manual(values = category_colors) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove the legend as it's not needed
    panel.grid.minor = element_blank(),  # Remove minor grid lines for a cleaner look
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

# Display the plot
print(plot_change_recent)


#cutbacks
# Load and prepare the dataset
cutback <- read.csv("spending_cutback_percentage.csv")
colnames(cutback) <- c("Category", "All", "18-29", "30-49", "50-64", "65+", "Men", "Women", "Lower Income", "Higher Income")
cutback_long <- cutback %>%
  gather(key = "Demographic", value = "Percentage", -Category)

cutback_age <- cutback_long %>%
  filter(Demographic == "All")

# Update category names and percentages
cutback_age$Category <- c("Eating out","Micro Treats","Entertainment","Car Journeys","Holiday Plans","Food delivery","Charity","Major household purchase","Streaming subscriptions","Skipping Meals","Other Subscriptions","Sports Membership","Insurances","Home services","Pets","Children's Activities","Private school fees")
cutback_age$Percentage <- cutback_age$Percentage * 100

# Sort the data by Percentage in descending order
cutback_age <- cutback_age %>%
  arrange(desc(Percentage))

# Create the horizontal bar chart with labels inside the bars and values outside
ggplot(cutback_age, aes(x = reorder(Category, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Category), position = position_stack(vjust = 0), hjust = 0, color = "white", size = 3.2) +
  geom_text(aes(label = sprintf("%.0f%%", Percentage)), position = position_stack(vjust = 1), hjust = -0.11,color = "steelblue", size = 3.2) +
  coord_flip() +
  labs(title = "Impact of Cost of Living Crisis on Spending in Australia",
       subtitle = "Where people are cutting back costs",
       x = "Spending Category",
       y = "Percentage of Cutbacks (%)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), # Hide y-axis text
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))








