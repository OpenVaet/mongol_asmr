# This code is derived from code authored by Henjin : https://sars2.net/czech.html
# And is provided simply to debunk it.

library(data.table)  # Loads data.table package for fast data manipulation
library(ggplot2)     # Loads ggplot2 package for data visualization
library(stringr)     # Loads stringr package for string manipulation

xy = read.csv("asmr_dataframe.csv")
xy$x <- as.Date(xy$x)
xy$z <- as.character(xy$z)
xy$y <- as.numeric(xy$y)
xy$pct <- as.numeric(xy$pct)

xy <- xy[order(-xtfrm(xy$z), xy$x), ]
print(xy)

# Define the date range
xstart = as.Date("2021-01-01")
xend = as.Date("2022-12-31")

# Sets factor levels for the dose variable
xy$z = factor(xy$z, unique(xy$z))

# Define plot parameters
yend = 6000
ystep = 1500
yend2 = 100
ystep2 = 25
secmult = yend / yend2

# Define colors for the plot
color = c(hcl(c(210, 120, 60, 0, 300) + 15, 90, 50), "black", "gray45", "gray70")
fill = c(hcl(c(210, 120, 60, 0, 300) + 15, 80, 70), "black", "gray45", "gray70")

# Create labels for the plot
label = data.frame(
  x = xstart + .02 * (xend - xstart), 
  y = seq(yend, by = -yend / 15, length.out = length(unique(xy$z))) - yend / 16,  
  label = unique(xy$z)
)
print(label)

# Creates the plot using ggplot2
ggplot(xy, aes(x, y)) +
  geom_area(aes(color = z, fill = z, y = pct * secmult), linewidth = .1, alpha = .22) +  # Add shaded areas
  geom_line(aes(color = z), linewidth = .4) +  # Add lines for mortality rate
  geom_hline(yintercept = c(0, yend), linewidth = .3, lineend = "square") +  # Add horizontal lines
  geom_vline(xintercept = c(xstart, xend), linewidth = .3, lineend = "square") +  # Add vertical lines
  geom_label(data = label, aes(x = x, y = y, label = label), fill = alpha("white", .7), label.r = unit(0, "lines"), label.padding = unit(.1, "lines"), label.size = 0, color = color[1:nrow(label)], size = 2.7, hjust = 0) +  # Add labels
  labs(title = "ASMR in Czech record-level data (±10-days rolling)",
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