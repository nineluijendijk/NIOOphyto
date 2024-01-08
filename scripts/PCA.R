# PCA

# PCA species where each sample is a lake/date count number
# https://www.datacamp.com/tutorial/pca-analysis-r
# https://www.youtube.com/watch?v=0Jp4gsfOLMs

library(tidyverse)
library(here)
library(pheatmap)
library(plotly)

load(file = here("data/data_tidy.RData")) # load object data_tidy
data_tidy <- mutate(data_tidy, LakeType = case_when(Lake == "CL" | Lake == "CH" ~ "Not connected",
                                                    Lake == "TP" | Lake == "CN" | Lake == "MP" ~ "Connected",
                                                    Lake == "BP" ~ "Half connected"))
data_tidy <- filter(data_tidy, Year < 2009)
data_tidy <- filter(data_tidy, Species != "A. quadrangularis") %>% filter(Species != "Diaptomus")
data_extra <- data_tidy %>% mutate("Abundance_ind/L" = (Counts / Proportion_of_sample_counted) / Sampling_volume_net_L,
                         "Relative_abundance" = Counts / Total_individuals * 100)

data_wide <- pivot_wider(data = select(data_extra, -Counts, -"Relative_abundance"), names_from = "Species", values_from = "Abundance_ind/L")
data_wide$Combined <- paste(data_wide$Lake, data_wide$Date)
data_wide <- na.omit(data_wide)

data <- as.data.frame(data_wide[14:36])
rownames(data) <- data$Combined
data_matrix <- as.matrix(data[2:12])

test <- scale(data_matrix)

pca <- prcomp(data_matrix, scale = TRUE) # test here, set scale to false
pca_summary <- summary(pca)$importance

pca_summary_mutated <- pca_summary %>% t() %>% as_tibble() %>%
  dplyr::rename(Proportion_of_Variance = "Proportion of Variance") %>% 
  mutate(PC=colnames(pca_summary)) %>%
  mutate(perc_variatie = Proportion_of_Variance*100)

pca_summary_mutated <- pca_summary %>% t() %>% as_tibble() %>%
  dplyr::rename(Proportion_of_Variance = "Proportion of Variance") %>% 
  mutate(PC=colnames(pca_summary)) %>%
  mutate(perc_variation = Proportion_of_Variance*100)

# Plot the percentages in a bar graph
pca_summary_mutated %>% ggplot(aes(x = reorder(PC, -perc_variation), y = perc_variation)) +
  geom_col(aes(fill = PC))+
  labs(title = "Percentage of variation explained by each PC",
       y = "Percentage variation",
       x = "PC")+
  geom_text(aes(label = round(perc_variation, digits = 1)), vjust=-0.25)+
  theme_minimal()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))#+
  #coord_cartesian(ylim = c(0, 15))

pca_plotting <- cbind(data_wide, pca$x)

# Obtain the percentages of variation covered by PC1 and PC2
PC1_var <- round(pca_summary["Proportion of Variance", "PC1"]*100, digits = 1)
PC2_var <- round(pca_summary["Proportion of Variance", "PC2"]*100, digits = 1)

# Plot PC1 vs PC2
speciesnames <- colnames(pca_plotting[14:32])
b <- pivot_longer(data = pca_plotting, cols = all_of(speciesnames), names_to = "Species", values_to = "Value") # so that species can be added to the plot

h <- ggplot(data = b, aes(x=PC1, y=PC2, color = LakeType, shape = Lake))+ 
  geom_point(size = 3, alpha = 0.7)+
  ggtitle("Principal component analysis plot")+
  xlab(paste0("PC1 (", PC1_var, "%)"))+
  ylab(paste0("PC2 (", PC2_var, "%)"))+
  theme_minimal()

ggplotly(h)

data_cor <- as.matrix(cor(data_matrix, method="spearman"))
pheatmap(data_cor, scale = "row")
