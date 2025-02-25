# Name: Libby Prince
# Date: February 20, 2025
# Purpose: To create a faceted line plot of COVID-19 cases for the six states with the most cases

# Load necessary libraries
library(tidyverse)

# URL for the COVID-19 data
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv"

# Read the data into an object called 'covid'
covid <- read_csv(url)

# Question 1: Make a faceted line plot of the 6 states with the most cases

# Step 1: Identify the six states with the most current cases
top_states <- covid %>%
  filter(date == max(date)) %>%  # Only use the most recent date
  group_by(state) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  top_n(6, total_cases) %>%
  pull(state)

# Step 2: Filter the raw data to those 6 states
covid_top_states <- covid %>%
  filter(state %in% top_states)

# Step 3: Set up a ggplot
ggplot(covid_top_states, aes(x = as.Date(date), y = cases, color = state)) +
  geom_line() +  # Add line plot
  labs(title = "COVID-19 Cases for Top 6 States", 
       x = "Date", 
       y = "Cases",
       color = "State") +
  facet_wrap(~ state, scales = "free_y") +  # Facet by state
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Step 4: Save the plot
ggsave("img/faceted_line_plot.png")
# Save the plot with adjusted width and height
ggsave("img/faceted_line_plot.png", width = 12, height = 8)

daily_total_cases <- covid %>%
  group_by(date) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))  # Summing the cases for each day

# Step 2: Set up a ggplot with geom_col() for a column plot
ggplot(daily_total_cases, aes(x = as.Date(date), y = total_cases)) +
  geom_col(fill = "steelblue") +  # Column plot (bars are vertical)
  labs(title = "Daily Total COVID-19 Cases in the USA", 
       x = "Date", 
       y = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Step 3: Save the plot
ggsave("img/daily_total_cases.png", width = 12, height = 8)

