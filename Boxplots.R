# Boxplots for abudant species

library(tidyverse)
library(here)

load(file = here("data/data_tidy.RData")) # load object data_tidy

data <- data_tidy %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L)

summary <- data %>% group_by(Species) %>% summarize(stdev = sd(`Abundance_ind/L`, na.rm = TRUE),
                                                    sum_abundance = sum(`Abundance_ind/L`, na.rm = TRUE))

topspecies <- summary %>% arrange(desc(sum_abundance)) %>% head(n = 3) %>% .[[1]]

data %>% filter(Species %in% topspecies) %>% 
  ggplot(aes(x = Species, y = `Abundance_ind/L`, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(y = "Abundance individuals/L") +
  ylim(0, 4)

data %>% filter(Species %in% topspecies) %>% 
  ggplot(aes(x = Species, y = log(`Abundance_ind/L` + 1), fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(y = "Log abundance individuals/L")





summary2 <- data %>% group_by(Species) %>% summarize(stdev = sd(Counts, na.rm = TRUE),
                                                    sum_counts = sum(Counts, na.rm = TRUE))

topspecies2 <- summary2 %>% arrange(desc(sum_counts)) %>% head(n = 3) %>% .[[1]]

data %>% filter(Species %in% topspecies2) %>% 
  ggplot(aes(x = Species, y = Counts, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  ylim(0, 150)

data %>% filter(Species %in% topspecies2) %>% 
  ggplot(aes(x = Species, y = log(Counts + 1), fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "none")

data %>% filter(Species %in% topspecies2) %>% 
  ggplot(aes(x = Species, y = Counts, fill = Species)) +
  geom_violin() +
  theme(legend.position = "none")

data %>% filter(Species %in% topspecies2) %>% 
  ggplot(aes(x = Species, y = log(Counts + 1), fill = Species)) +
  geom_violin(draw_quantiles = c(.25, .5, .75, .95)) +
  theme(legend.position = "none")

data %>% filter(Species %in% topspecies2) %>% 
  ggplot(aes(x = Species, y = sqrt(Counts), fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "none")

data %>% filter(Species %in% topspecies2) %>% 
  ggplot(aes(x = Species, y = Counts^(1/3), fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "none")


# https://www.statology.org/transform-data-in-r/
# https://stats.stackexchange.com/questions/251066/boxplot-for-data-with-a-large-number-of-zero-values

  




