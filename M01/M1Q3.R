library(tidyverse)
library(cowplot)
library(lubridate)

# Objetivo:
#  - Plotar 6 gráficos ao longo de 3 linhas e 2 colunas
#  - Coluna 1: HR, PACE e SPEED em função do tempo
#  - Coluna 2: HR, PACE e SPEED em função do espaço

# Saving path to file
file_path <- "./dataset2.CSV"

# Read file escaping the first 2 rows
data <- read_csv(file_path, skip = 2)

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

minute(data$`Pace (min/km)`)
