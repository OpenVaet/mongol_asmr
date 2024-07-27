library(dplyr)
library(tidyr)
library(stringr)
library(readr)  # Assuming you need this for read_csv

# Reads the datasets
mort_data <- read_csv("data/Morticd10_part5_rev.csv")

# Converts ICD10_Code to character type for merging
icd10_codes <- icd10_codes %>%
  mutate(ICD10_Code = as.character(ICD10_Code))

# Filters the first dataset
filtered_data <- mort_data %>%
  filter(Country == 4045, Cause != 1000)

# Calculates the 10 most common causes of deaths yearly in age groups 10 to 24
age_group_deaths <- filtered_data %>%
  mutate(deaths_10_to_24 = Deaths8 + Deaths9 + Deaths10) %>%
  group_by(Year, Cause) %>%
  summarize(total_deaths = sum(deaths_10_to_24, na.rm = TRUE)) %>%
  arrange(Year, desc(total_deaths))
print(age_group_deaths)

# Gets the top 10 causes of deaths for each year
top_10_causes <- age_group_deaths %>%
  group_by(Year) %>%
  top_n(10, wt = total_deaths) %>%
  ungroup()

# Views the result
print(top_10_causes, n=100)

# Create a data frame mapping Causes to ICD-10 Codes
icd10_mapping <- data.frame(
  Cause = c("AAA", "X70", "X80", "X81", "R99", "V475", "V476", "V031", "V051", "J180", 
            "V050", "X72", "V445", "I509", "V435", "G800", "C910", "J189", "V470", "Y34", 
            "V436", "Y20"),
  ICD10_Code = c("Abdominal aortic aneurysm",
                 "Intentional self-harm by hanging, strangulation and suffocation",
                 "Intentional self-harm by jumping from a high place",
                 "Intentional self-harm by jumping or lying in front of moving object",
                 "Ill-defined and unknown cause of mortality",
                 "Car driver injured in collision with fixed or stationary object in traffic accident",
                 "Car passenger injured in collision with fixed or stationary object in traffic accident",
                 "Occupant of three-wheeled motor vehicle injured in collision with pedal cycle",
                 "Pedestrian injured in collision with railway train or railway vehicle in traffic accident",
                 "Bronchopneumonia, unspecified organism", 
                 "Pedestrian injured in collision with railway train or railway vehicle in nontraffic accident",
                 "Intentional self-harm by handgun discharge",
                 "Car driver injured in collision with heavy transport vehicle or bus in traffic accident",
                 "Heart failure, unspecified",
                 "Car driver injured in collision with car, pick-up truck or van in traffic accident",
                 "Spastic quadriplegic cerebral palsy",
                 "Acute lymphoblastic leukemia",
                 "Pneumonia, unspecified organism",
                 "Car occupant injured in collision with fixed or stationary object",
                 "Unspecified event, undetermined intent", 
                 "Car passenger injured in collision with car, pick-up truck or van in traffic accident",
                 "Hanging, strangulation and suffocation, undetermined intent")
)

# Merges top_10_causes with icd10_mapping to get ICD-10 codes
causes_with_icd10 <- top_10_causes %>%
  left_join(icd10_mapping, by = "Cause")

# Prints the resulting data frame with ICD-10 codes
print(causes_with_icd10, n=100)

# Calculates the total deaths per year
total_deaths_per_year <- age_group_deaths %>%
  group_by(Year) %>%
  summarize(total_deaths_year = sum(total_deaths, na.rm = TRUE))

# Merges with top_10_causes to calculate percentages
causes_with_percentages <- top_10_causes %>%
  left_join(total_deaths_per_year, by = "Year") %>%
  mutate(percentage_of_total_deaths = (total_deaths / total_deaths_year) * 100)

# Merges with ICD-10 mapping for a complete view
final_result <- causes_with_percentages %>%
  left_join(icd10_mapping, by = "Cause")

# Prints the final result with percentages and ICD-10 codes
print(final_result, n=100)
