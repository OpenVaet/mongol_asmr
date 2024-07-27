library(dplyr)
library(data.table)

# Loads the data
rec <- fread("data/Vesely_106_202403141131.csv")

# Filters and summarize the data
t <- rec[Rok_narozeni < 2023 & !(!is.na(DatumUmrti) & DatumUmrti < "2023-01-01"), 
         .(recordlevel = .N), 
         .(age = pmin(2022 - Rok_narozeni, 100) %/% 5 * 5)][order(age)]

# Merges with census data
t <- merge(t, fread("mongoljunk/czcensus2023pop.csv")[, 
                                                      .(eurostat2023 = sum(pop)), 
                                                      .(age = age %/% 5 * 5)])

# Adds total row
t <- rbind(t, t[, lapply(.SD, sum)][, age := "Total"])

# Calculates percentage
t[, eurostatpct := recordlevel / eurostat2023 * 100]

# Rounds numerical columns
t <- mutate_if(t, is.double, round, 1)

# Prints the result
print(t, row.names = FALSE)

# Save the result to a CSV file
write.csv(t, "2023_record_to_census_compare.csv", row.names = FALSE)
