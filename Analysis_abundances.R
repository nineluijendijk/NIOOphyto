# Analysis of abundances

library(tidyverse)
library(here)
library(lubridate)
library(RColorBrewer)

load(file = here("data/data_tidy.RData")) # load object data_tidy

# Calculate abundance and relative abundance

data <- data_tidy %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L,
                             "Relative_abundance" = Counts / Total_individuals * 100)

data$Month <- month(data$Date, label = TRUE) # Add month names

summary <- data %>% group_by(Species, Month) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE),
                                                           sum_total = sum(Total_individuals, na.rm = TRUE)) # Calculate counts per month

summary <- summary %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # Calculate relative abundance per month

getPalette <- colorRampPalette(brewer.pal(9, "Set1")) 

summary %>%
  ggplot(aes(x = Month, y = Relative_abundance, fill = Species)) +
  geom_col(color = "Black", linewidth = 0.05) +
  labs(y = "Relative abundance (%)") +
  theme_minimal()+
  scale_fill_manual(values = getPalette(24)) # Generate plot of relative abundance per month, not sure about the colors
