library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# Loads the CSV file
data <- read_csv("data/umrti.csv")

# Filters data for ages between 10 and 24
filtered_data <- data %>% 
  filter(vek >= 10 & vek <= 24)

# Converts datum to Date type and extract the week
filtered_data <- filtered_data %>%
  mutate(date_of_death = as.Date(datum),
         week_of_death = floor_date(date_of_death, unit = "week"))

# Summarizes the weekly deaths
weekly_deaths <- filtered_data %>%
  group_by(week_of_death) %>%
  summarize(weekly_death_count = n())

# Plots the data using ggplot2
ggplot(weekly_deaths, aes(x = week_of_death, y = weekly_death_count)) +
  geom_col() +
  geom_text(aes(label = weekly_death_count), vjust = -0.5, size = 5) +
  labs(title = "CZ - Weekly Deaths for Ages 10 to 24",
       x = "Date of Death (Week)",
       y = "Number of Deaths") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 18)
  )


# Calculates the total number of deaths over the period
total_deaths <- sum(weekly_deaths$weekly_death_count)

# Prints the total number of deaths
print(paste("Total number of deaths over the period:", total_deaths))
