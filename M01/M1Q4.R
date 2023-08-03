library(tidyverse)
library(cowplot)
library(lubridate)

# A) SAME AS Q3

# Objetivo:
#  - Plotar 6 gráficos ao longo de 3 linhas e 2 colunas
#  - Coluna 1: HR, PACE e SPEED em função do tempo
#  - Coluna 2: HR, PACE e SPEED em função do espaço

# Saving path to file
file_path <- "./dataset3.CSV"

# Read file escaping the first 2 rows
data <- read_csv(file_path, skip = 2)

# Filter NA
data <- filter(data, !is.na(`HR (bpm)`) & !is.na(`Pace (min/km)`) & !is.na(`Speed (km/h)`))

# Open interactive window view
View(data)

p1 <- ggplot(data, aes(Time, `HR (bpm)`)) + 
  geom_point() +
  labs(x = "", y = "", title ="HR (bpm)") +
  theme(plot.title = element_text(hjust = 0.5))

p3 <- ggplot(data, aes(Time, minute(`Pace (min/km)`))) + 
  geom_point() +
  labs(x = "", y = "", title ="Pace (min/km)") +
  theme(plot.title = element_text(hjust = 0.5))

p5 <- ggplot(data, aes(Time, `Speed (km/h)`)) + 
  geom_point() +
  labs(x = "tempo (hh:mm:ss)", y = "", title ="Speed (km/h)") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data, aes(`Distances (m)`, `HR (bpm)`)) +
  geom_point() +
  labs(x = "", y = "", title ="HR (bpm)") +
  theme(plot.title = element_text(hjust = 0.5))

p4 <- ggplot(data, aes(`Distances (m)`, minute(`Pace (min/km)`))) +
  geom_point() +
  labs(x = "", y = "", title ="Pace (min/km)") +
  theme(plot.title = element_text(hjust = 0.5))

p6 <- ggplot(data, aes(`Distances (m)`, `Speed (km/h)`)) +
  geom_point() +
  labs(x = "distância (m)", y = "", title ="Speed (km/h)") +
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(p1,p2,p3,p4,p5,p6, labels = c('A', 'B'), label_size = 12,
          ncol = 2, nrow = 3)


# B) MEAN BETWEEN EVERY SAMPLE

data %>% 
  summarise(mHR    = round(mean(`HR (bpm)`), 3),
            mPace  = round(mean(minute(`Pace (min/km)`)),  3),
            mSpeed = round(mean(`Speed (km/h)`),  3)) %>%
  glimpse()


# C) MEAN FROM 200m TO 1000m

data %>% 
  filter(`Distances (m)` >= 200, `Distances (m)` <= 1000) %>%
  summarise(mHR    = round(mean(`HR (bpm)`), 3),
            mPace  = round(mean(minute(`Pace (min/km)`)),  3),
            mSpeed = round(mean(`Speed (km/h)`),  3)) %>%
  glimpse()


# D) MEAN FROM 80s TO 200s

data %>% 
  filter(Time >= 80, Time <= 200) %>%
  summarise(mHR    = round(mean(`HR (bpm)`), 3),
            mPace  = round(mean(minute(`Pace (min/km)`)),  3),
            mSpeed = round(mean(`Speed (km/h)`),  3)) %>%
  glimpse()


# E) MIN-MAX FROM 80s TO 200s

data %>% 
  filter(Time >= 80, Time <= 200) %>%
  summarise(minHR    = round(min(`HR (bpm)`), 3),
            maxHR    = round(max(`HR (bpm)`), 3),
            minPace  = round(min(minute(`Pace (min/km)`)),  3),
            maxPace  = round(max(minute(`Pace (min/km)`)),  3),
            minSpeed = round(min(`Speed (km/h)`),  3),
            maxSpeed = round(max(`Speed (km/h)`),  3)) %>%
  glimpse()


# F) MEAN FROM THE LAST 5s

data %>% 
  slice_max(Time, n=5) %>%
  glimpse() %>%  # Just to check the pipeline
  summarise(mHR    = round(mean(`HR (bpm)`), 3),
            mPace  = round(mean(minute(`Pace (min/km)`)),  3),
            mSpeed = round(mean(`Speed (km/h)`),  3)) %>%
  glimpse()
