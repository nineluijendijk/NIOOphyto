library(readxl)
library(tidyverse)
library(here)
library(plotly)

pc_values <- read_excel(here("data/pc_values.xlsx"))
pc_values_tidy <- separate(pc_values, Sample, into = c("Lake", "Date"), sep = " ")

pcplot <- ggplot(data = pc_values_tidy, aes(x = Date, y = CaseR.1, group = Lake, color = Lake)) +
  geom_line() +
  geom_point() +
  labs(y = "PC1 value", title = "PC1 values over time per lake") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
ggplotly(pcplot)
