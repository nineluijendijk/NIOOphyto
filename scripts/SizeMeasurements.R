#

library(tidyverse)
library(readxl)
library(here)

df1 <- read_excel(path = here("data_raw/Body_length_2010_2012.xlsx"), range = "CL!A1:J288") # import data
df2 <- read_excel(path = here("data_raw/Body_length_2010_2012.xlsx"), range = "TP!A1:I206") # import data
df1 <- df1[, -3]
df2 <- df2[, -3]

data1 <- pivot_longer(df1, cols = 3:9, names_to = "Species", values_to = "Size")
data2 <- pivot_longer(df2, cols = 3:8, names_to = "Species", values_to = "Size")

data <- rbind(data1, data2)
data$By <- "Kailin"

df3 <- read_excel(path = here("data_raw/Body_length_2005-2007.xlsx"), range = "Measurements!A1:F119") # import data
df3 <- df3[, -3]
df3 <- df3[, -5]
df3$By <- "Nine"

data_tidy <- rbind(data, df3) # combine both datasets

summary <- data_tidy %>% group_by(Species) %>% summarize(mean = mean(Size, na.rm = TRUE),
                                              stdev = sd(Size, na.rm = TRUE),
                                              min = min(Size, na.rm = TRUE),
                                              max = max(Size, na.rm = TRUE)) # calculate mean, stdev, min and max of each species

data_tidy$SizeR <- plyr::round_any(data_tidy$Size, accuracy = 100) # round body sizes to create groups

ggplot(data = data_tidy, aes(x = Size, fill = By))+
  geom_histogram(binwidth = 50, alpha = 1, position = "stack")+ # the histograms are stacked, not overlapped!
  labs(y = "Frequency", x = "Body Length in µm", title = "Measured body lengths shown in stacked histograms rather than overlapped")

counts <- plyr::count(data_tidy$SizeR) # count how many times a body size was measured
total <- sum(!is.na(data_tidy$SizeR))
percentages <- head(counts, n = -1) %>% mutate("Rfreq" = freq / (total / 100)) # find the % of how many times a body size was measured
colnames(percentages)[1] <- "SizeR"
percentagesF <- filter(percentages, Rfreq >= 1.0) # filter for body sizes measured over 1% of the time

# left_join(data_tidy, percentages, by = "SizeR")

ggplot(data = percentagesF, aes(x = SizeR / 1000, y = Rfreq))+
  geom_col()+
  scale_x_continuous(breaks = seq(0, (max(percentagesF$SizeR))/1000, 0.2))+
  labs(y = "Frequency %", x = "Body Length in mm", 
       title = "Relative frequenceies of body lenghts rounded to the nearest 100,\nonly size ranges that make up at least 1% of the total sample included")


# Generate this plot for every lake using the following function

plotRfreqLake <- function(lake = "CL") {
  dataF <- filter(data_tidy, Lake == lake)
  
  dataF$SizeR <- plyr::round_any(dataF$Size, accuracy = 100) # round body sizes to create groups
  
  counts <- plyr::count(dataF$SizeR) # count how many times a body size was measured
  total <- sum(!is.na(dataF$SizeR))
  percentages <- head(counts, n = -1) %>% mutate("Rfreq" = freq / (total / 100)) # find the % of how many times a body size was measured
  colnames(percentages)[1] <- "SizeR"
  percentagesF <- filter(percentages, Rfreq >= 1.0) # filter for body sizes measured over 1% of the time
  
  # left_join(dataF, percentages, by = "SizeR")
  
  ggplot(data = percentagesF, aes(x = SizeR / 1000, y = Rfreq))+
    geom_col()+
    scale_x_continuous(breaks = seq(0, (max(percentagesF$SizeR))/1000, 0.2))+
    labs(y = "Frequency %", x = "Body Length in mm", 
         title = paste0("Relative frequenceies of body lenghts rounded to the nearest 100,\nonly size ranges that make up at least 1% of the total sample included in ", lake))
}

lakes <- unique(data_tidy$Lake)

for (i in lakes) {
  print(plotRfreqLake(lake = i))
}





# Histogram per Species

plotRfreqSpecies <- function(species = "D. hyalina") {
  dataF <- filter(data_tidy, Species == species)
  
  dataF$SizeR <- plyr::round_any(dataF$Size, accuracy = 100) # round body sizes to create groups
  
  counts <- plyr::count(dataF$SizeR) # count how many times a body size was measured
  total <- sum(!is.na(dataF$SizeR))
  percentages <- counts %>% mutate("Rfreq" = freq / (total / 100)) # find the % of how many times a body size was measured
  colnames(percentages)[1] <- "SizeR"
  percentagesF <- filter(percentages, Rfreq >= 1.0) # filter for body sizes measured over 1% of the time
  
  # left_join(dataF, percentages, by = "SizeR")
  
  ggplot(data = percentagesF, aes(x = SizeR / 1000, y = Rfreq))+
    geom_col()+
    scale_x_continuous(breaks = seq(0, 3, 0.2), limits = c(0, 3.2))+
    scale_y_continuous(breaks = seq(0, 100, 5))+
    labs(y = "Frequency %", x = "Body Length in mm", 
         title = paste0("Distribution of ", species, " body size measurements"))
}

species <- unique(data_tidy$Species)

for (i in species) {
  print(plotRfreqSpecies(species = i))
}
