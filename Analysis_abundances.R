# Analysis of abundances

library(tidyverse)
library(here)

load(file = here("data/data_tidy.RData")) # load object data_tidy

# Abundance is ((Counts/Proportion)/Sampling volume)

data <- data_tidy %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L)

view(data)


