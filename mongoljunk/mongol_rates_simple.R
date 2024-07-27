# This code is derived from code authored by Henjin : https://sars2.net/czech.html
# And is provided simply to debunk it.

library(data.table)  # Loads data.table package for fast data manipulation
library(ggplot2)     # Loads ggplot2 package for data visualization
library(stringr)     # Loads stringr package for string manipulation

# Defines custom functions for later use
ma = \(x, b=1, f=b) { 
  x[] = rowMeans(embed(c(rep(NA, b), x, rep(NA, f)), f + b + 1), na.rm=T)
  x
}  # Function to calculate moving average

# Reads the weekly death data from a CSV file
xstart = as.Date("2021-1-1")  # Start date for the data
xend = as.Date("2022-12-31")  # End date for the data

# Reads the population data from a CSV file
std = read.csv("mongoljunk/czcensus2021pop.csv")[,2]

# Reads the daily bucket data from a CSV file
buck = fread("mongoljunk/czbucketsdaily.csv")
buck = buck[,.(alive = sum(alive), dead = sum(dead)), .(date, age = pmin(age, 100), dose = ifelse(dose == 0, "Unvaccinated", paste0("Dose ", ifelse(dose >= 4, "4+", dose))))]  # Aggregate data by date, age, and dose
buck = rbind(buck, cbind(expand.grid(date = as.IDate(seq(xstart, xend, 1)), age = 0:100, dose = unique(buck$dose)), alive = 0, dead = 0))  # Ensure all dates and age groups are covered
buck = unique(buck, by=1:3)[order(dose, age, date)]  # Remove duplicates and sort data

# Aggregates data for all vaccinated people
vax = cbind(buck[dose != "Unvaccinated", .(alive = sum(alive), dead = sum(dead)), .(age, date)], dose = "All vaccinated")

# Combines the original and aggregated vaccinated data
buck = rbind(buck, vax)
buck = buck[, .(date = date, alive = ma(alive, 10), dead = ma(dead, 10)), .(age, dose)]  # Apply moving average to alive and dead counts

# Calculates the population and standardized mortality rate
xy = with(buck, aggregate(list(pop = alive, y = dead / alive * 365e5 * std[age + 1] / sum(std)), list(x = date, z = dose), sum, na.rm=T))

# Filters only for "Unvaccinated" and "All vaccinated"
xy = xy[xy$z %in% c("Unvaccinated", "All vaccinated"),]

# Sets factor levels for the dose variable
xy$z = factor(xy$z, unique(xy$z))

# Sets mortality rate to NA for small population sizes
xy$y[xy$pop < 2e3] = NA

# Defines plot parameters
yend = 4000
ystep = 1000
yend2 = 100
ystep2 = 25
secmult = yend / yend2

# Defines colors for the plot
color = c(hcl(c(210, 120, 60, 0, 300) + 15, 90, 50), "black", "gray45", "gray70")
fill = c(hcl(c(210, 120, 60, 0, 300) + 15, 80, 70), "black", "gray45", "gray70")

print(xy)

# Creates labels for the plot
label = data.frame(
  x = xstart + .02 * (xend - xstart), 
  y = seq(yend, by = -yend / 15, length.out = nlevels(xy$z)) - yend / 16,  # Corrected the seq function call
  label = levels(xy$z)
)
print(label)

# Calculates the percentage of people with each dose
xy$pct = 99.99 * xy$pop / tapply(xy$pop, xy$x, sum, na.rm=T)[as.character(xy$x)]

# Creates the plot using ggplot2
ggplot(xy, aes(x, y)) +
  geom_area(aes(color = z, fill = z, y = pct * secmult), linewidth = .1, alpha = .22) +  # Add shaded areas
  geom_line(aes(color = z), linewidth = .4) +  # Add lines for mortality rate
  geom_hline(yintercept = c(0, yend), linewidth = .3, lineend = "square") +  # Add horizontal lines
  geom_vline(xintercept = c(xstart, xend), linewidth = .3, lineend = "square") +  # Add vertical lines
  geom_label(data = label, aes(x = x, y = y, label = label), fill = alpha("white", .7), label.r = unit(0, "lines"), label.padding = unit(.1, "lines"), label.size = 0, color = color[1:nrow(label)], size = 2.7, hjust = 0) +  # Add labels
  labs(title = "Something-mortality rate in Czech record-level data (±10-day moving averages)",
       x = NULL, y = "ASMR per 100k person-years") +
  scale_x_date(limits = c(xstart, xend), breaks = seq(xstart, xend, "2 month"), date_labels = "%b\n%y") +  # Set x-axis limits and labels
  scale_y_continuous(limits = c(0, yend), breaks = seq(0, yend, ystep), labels = \(x) ifelse(x == 0, x, paste0(x / 1e3, "k")), sec.axis = sec_axis(trans = ~./secmult, breaks = seq(0, yend2, ystep2), name = "Percentage of people with dose")) +  # Set y-axis limits and labels
  scale_color_manual(values = color) +  # Set colors
  scale_fill_manual(values = fill) +  # Set fill colors
  coord_cartesian(clip = "off", expand = F) +  # Set plot coordinates
  theme(axis.text = element_text(size = 6.5, color = "black"),
        axis.ticks = element_line(linewidth = .3),
        axis.ticks.length = unit(.18, "lines"),
        axis.title = element_text(size = 8),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(.3, .3, .3, .3, "lines"),
        plot.subtitle = element_text(size = 6.8),
        plot.title = element_text(size = 8.2))

# Saves the plot as a PNG file
ggsave("1.png", width = 5, height = 3.3, dpi = 400)