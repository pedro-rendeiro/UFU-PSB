library(tidyverse)

# A) OPEN FILE
# By looking at the file, it was noticed that all that matters starts at line 3

# Saving path to file
file_path <- "./dataset1.CSV"

# Read file escaping the first 2 rows
data <- read_csv(file_path, skip = 2)

# Open interactive window view
View(data)

# B) HR OVER TIME LINE GRAPH
ggplot(data, aes(x = Time, y = `HR (bpm)`)) + 
  geom_line()

# C) QUANTIZE HR IN 5 LEVELS
# Remove NA rows
data <- filter(data, !is.na(`HR (bpm)`))

# Get global min and max
min_data <- min(data$`HR (bpm)`)
max_data <- max(data$`HR (bpm)`)

# Define quantization step
num_levels <- 5
level_size <- (max_data - min_data) / num_levels

# Create intervals
q_intervals <- seq(min_data, max_data, by=level_size)

d_values <- numeric(num_levels)

# Prints min, max and mean for each level
# And calculates quantized levels (d_values)
for (i in 1:num_levels) {
  level_min <- q_intervals[i]
  level_max <- q_intervals[i + 1]
  d_values[i] <- (level_min + level_max) / 2
  cat("Level", i, ": Min =", level_min, "| Max =", level_max, "| Mean =", d_values[i], "\n")
}

# Map value to the correct level
data$`Q_HR (bpm)` <- cut(data$`HR (bpm)`, breaks = q_intervals,
                         labels = d_values, include.lowest = TRUE)

View(data)

# D) PLOT QUANTIZED HR AS A LINE (NUMERIC)
ggplot(data, aes(x = Time, y = as.numeric(`Q_HR (bpm)`))) + 
  geom_line()

# E) PLOT QUANTIZED HR SCATTERED (FACTOR)
ggplot(data, aes(x = Time, y = `Q_HR (bpm)`)) + 
  geom_point()

# F) HR METRICS
# Requirements: remove NA; range [125,150]; 3 points precision
data %>%  # compound assignment operator
  filter(!is.na(`HR (bpm)`)) %>%  # remove NA
  filter(`HR (bpm)` >= 125, `HR (bpm)` <= 150) %>%  # filter to range [125,150]
  # calculates metrics over all remained values
  summarise(media = round(mean(`HR (bpm)`), 3),
            var   = round(var(`HR (bpm)`),  3),
            v_max = round(max(`HR (bpm)`),  3),
            v_min = round(min(`HR (bpm)`),  3) ) %>%
  glimpse()  # plot results

# G) HR METRICS 2
# Requirements: remove NA; range [90,120]; points precision not fixed
data %>%  # compound assignment operator
  filter(!is.na(`HR (bpm)`)) %>%  # remove NA
  filter(`HR (bpm)` >= 90, `HR (bpm)` <= 120) %>%  # filter to range [90,120]
  # calculates metrics over all remained values
  summarise(media = round(mean(`HR (bpm)`), 3),
            var   = round(var(`HR (bpm)`),  3),
            v_max = round(max(`HR (bpm)`),  3),
            v_min = round(min(`HR (bpm)`),  3) ) %>%
  glimpse()  # plot results
