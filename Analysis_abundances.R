# Analysis of abundances

# Do it seperately per year/lake

library(tidyverse)
library(here)
library(RColorBrewer)
library(gridExtra)

load(file = here("data/data_tidy.RData")) # load object data_tidy

lakes <- unique(data_tidy$Lake)

lakes[3]

plotlist <- list()

for (i in 1:length(lakes)){
  Ldata <- filter(data_tidy, Lake == lakes[i])

# Calculate abundance and relative abundance

data <- Ldata %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L,
                             "Relative_abundance" = Counts / Total_individuals * 100)



summary <- data %>% group_by(Species, Month) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE),
                                                           sum_total = sum(Total_individuals, na.rm = TRUE)) # Calculate counts per month

summary <- summary %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # Calculate relative abundance per month

getPalette <- colorRampPalette(brewer.pal(9, "Set1")) # prepare color palette

result <- summary %>% ggplot(aes(x = Month, y = Relative_abundance, fill = Species))+
  geom_col(color = "Black", linewidth = 0.05)+
  labs(y = "Relative abundance (%)", title = paste0("Lake ", lakes[[i]]))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_manual(values = getPalette(24)) # Generate plot of relative abundance per month, not sure about the colors

plotlist[[i]] <- result
}

plot_grid(plotlist = plotlist, ncol = 2)

