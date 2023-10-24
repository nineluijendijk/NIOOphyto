# Analysis of abundances per year

# Do it separately per year/lake

library(tidyverse)
library(here)
library(RColorBrewer)
library(plotly)

load(file = here("data/data_tidy.RData")) # load object data_tidy

lakes <- unique(data_tidy$LakeName)

years <- unique(data_tidy$Year)

plotlist <- list()

for (i in 1:length(lakes)){
  Ldata <- filter(data_tidy, LakeName == lakes[i])
  
  for (j in 1:length(years)){
    Ydata <- filter(Ldata, Year == years[j])
    
    if (nrow(Ydata) > 0) {
      
      data <- Ydata %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L,
                               "Relative_abundance" = Counts / Total_individuals * 100)
      
      summary <- data %>% group_by(Species, Month) %>% summarize(sum_counts = sum(Counts, na.rm = TRUE),
                                                                 sum_total = sum(Total_individuals, na.rm = TRUE)) # Calculate counts per month
      
      summary <- summary %>% mutate("Relative_abundance" = sum_counts / sum_total * 100) # Calculate relative abundance per month
      
      getPalette <- colorRampPalette(brewer.pal(9, "Set1")) # prepare color palette
      
      result <- summary %>% ggplot(aes(x = Month, y = Relative_abundance, fill = Species))+
        geom_col(color = "Black", linewidth = 0.05)+
        labs(y = "Relative abundance (%)", title = paste0(lakes[[i]], " in ", years[[j]]))+
        theme_minimal()+
        scale_fill_manual(values = getPalette(24)) # Generate plot of relative abundance per month, not sure about the colors
      
      plotlist[[(length(plotlist)+1)]] <- result
    } else {
      warning(paste0("No data for ", lakes[[i]], "in the year ", years[[j]]))
    }
  }
}


