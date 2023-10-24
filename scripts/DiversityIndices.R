# Diversity indices

# used https://www.youtube.com/watch?v=wq1SXGQYgCs to help with the plots
# used https://dnr.wisconsin.gov/sites/default/files/topic/Research/lessons_MeasuringBiodiversityEducatorHandout.pdf to find the formula of the evenness index

library(tidyverse)
library(here)
library(vegan)
library(chemodiv)

load(file = here("data/data_tidy.RData")) # load object data_tidy

Ldata <- data_tidy %>% filter(LakeName == "Church Pond") 

Mdata <- Ldata %>% group_by(Month, Species, LakeName) %>% summarize(Count = sum(Counts, na.rm = TRUE))

summary <- Mdata[c("Month", "Species", "Count")] %>% group_by(Month) %>% summarize(richness = specnumber(Count),
                                                                        shannon = diversity(Count, index = "shannon"),
                                                                        simpson = diversity(Count, index = "simpson"),
                                                                        invsimpson = 1/simpson,
                                                                        total = sum(Count),
                                                                        evenness = shannon/log(richness))

summary %>% pivot_longer(cols = c(richness, shannon, simpson, invsimpson, evenness),
                         names_to = "metric",
                         values_to = "Count") %>%
  ggplot(aes(x = total, y = Count))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~metric, nrow = 5, scales = "free_y") # need to fix axis labels
