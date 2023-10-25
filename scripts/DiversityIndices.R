# Diversity indices

# used https://www.youtube.com/watch?v=wq1SXGQYgCs to help with the plots
# used https://dnr.wisconsin.gov/sites/default/files/topic/Research/lessons_MeasuringBiodiversityEducatorHandout.pdf to find the formula of the evenness index
# ADD RANKING OF HIGHEST DIVERSITY SCORE

library(tidyverse)
library(here)
library(vegan)

load(file = here("data/data_tidy.RData")) # load object data_tidy

lakes <- unique(data_tidy$LakeName)

years <- unique(data_tidy$Year)

for (i in 1:length(lakes)){
  Ldata <- data_tidy %>% filter(LakeName == lakes[i]) 
  
  for (j in 1:length(years)){
    Ydata <- filter(Ldata, Year == years[j])
    
    if (nrow(Ydata) > 0) {
      
      Mdata <- Ydata %>% group_by(Month, Species, LakeName) %>% summarize(Count = sum(Counts, na.rm = TRUE))
      
      summary <- Mdata[c("Month", "Species", "Count")] %>% group_by(Month) %>% summarize(richness = specnumber(Count),
                                                                                         shannon = diversity(Count, index = "shannon"),
                                                                                         simpson = diversity(Count, index = "simpson"),
                                                                                         invsimpson = 1/simpson,
                                                                                         total = sum(Count),
                                                                                         evenness = shannon/log(richness))
      
      result <- summary %>% pivot_longer(cols = c(richness, shannon, simpson, invsimpson, evenness),
                                         names_to = "metric",
                                         values_to = "Count") %>%
        ggplot(aes(x = total, y = Count))+
        geom_point()+
        geom_smooth()+
        facet_wrap(~metric, nrow = 5, scales = "free_y")+
        labs(y = "Value",
             x = "Number of counts",
             title = paste0(lakes[i], " in ", years[j]))
      
      #print(result)
      #print(knitr::kable(summary, 
                         format = "markdown",
                         caption = paste0("Diversity indices of ", lakes[i], " in ", years[j])))
      
    } else {
      warning(paste0("No data for ", lakes[[i]], "in the year ", years[[j]]))
    }
  }
}
