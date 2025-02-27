############# Packages ################################################

{
  if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
  if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
  if (!require("moments")) install.packages("moments", dependencies=TRUE)
  if (!require("tidyr")) install.packages("tidyr", dependencies=TRUE)
  if (!require("scales")) install.packages("scales", dependencies=TRUE)
}

#### Read data ##########################################################

setwd("C:/Users/gusta/OneDrive/Área de Trabalho/Doutorado/Submissões/2025/ISF/02_Simulation_files_output") # Change your path 
all_fut_scenarios<-readRDS("Future_scenarios.RDATA")

setwd("C:/Users/gusta/OneDrive/Área de Trabalho/Doutorado/Submissões/2025/ISF/Data")
sdg_criteria<-read.csv("SDG_Criteria.csv", check.names = FALSE)


########################## Indicator ##############################

number_scenarios<-1000 # Number of scenarios previously simulated
horizon<-12 # Scenario horizon previously simulated

###### ODS + wind version ######

criteria_weights<-list()
mabac_results<-list()

for (i in 1:number_scenarios) {
  
    wind_scenario <- data.frame(sapply(all_fut_scenarios, function(df) df[(((i - 1)*horizon)+1):(i*horizon), 4])) 
    colnames(wind_scenario) <- sub("^X", "", colnames(wind_scenario))
    
    #colnames(wind_scenario) <- paste0("municip", 1:length(all_fut_scenarios))
    wind_scenario <- as.data.frame(t(wind_scenario))
    colnames(wind_scenario) <- paste0("wind_", 1:horizon)
    wind_scenario$Code <- as.numeric(rownames(wind_scenario))
    
    case <- merge(sdg_criteria, wind_scenario, by = "Code", all.x = TRUE)
    
    #case <- cbind(sdg_criteria, wind_scenario)
    case <- case %>%
      mutate(across(3:21, as.numeric))
    
    ########################## CRITIC ##############################
    
    # Step 1: Select the criteria columns (columns 3 to 21)
    criteria <- case[, 3:21]
    
    # Step 2: Identify benefit-based and cost-based criteria
    benefit_indices <- c(3, 10:21) - 2  
    cost_indices <- setdiff(1:ncol(criteria), benefit_indices)
    
    # Step 3: Normalize the criteria based on the type
    normalize <- function(x, type) {
      if (type == "benefit") {
        return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      } else { # cost-based
        return((x - max(x, na.rm = TRUE)) / (min(x, na.rm = TRUE) - max(x, na.rm = TRUE)))
      }
    }
    
    normalized_criteria <- as.data.frame(sapply(1:ncol(criteria), function(j) {
      if (j %in% benefit_indices) {
        normalize(criteria[[j]], "benefit")
      } else {
        normalize(criteria[[j]], "cost")
      }
    }))
    
    # Step 4: Compute standard deviation for each criterion
    std_dev <- apply(normalized_criteria, 2, sd, na.rm = TRUE)
    
    # Step 5: Compute Spearman correlation matrix
    cor_matrix <- cor(normalized_criteria, method = "spearman", use = "complete.obs")
    
    # Step 6: Compute information amount for each criterion
    info_amount <- std_dev * rowSums(1 - cor_matrix, na.rm = TRUE)
    
    # Step 7: Compute the weights
    weights <- info_amount / sum(info_amount)
    
    # Step 8: Create a dataframe with criteria names and their corresponding weights
    criteria_weights[[i]] <- data.frame(Criteria = colnames(criteria), Weight = weights)
    
    
    ########################## MABAC ##############################
    
    # Step 1: Use the normalized decision matrix from the CRITIC method
    # (Assuming `normalized_criteria` and `criteria_weights` are available from the previous step)
    
    # Step 2: Compute the weighted matrix
    weighted_matrix <- as.data.frame(sapply(1:ncol(normalized_criteria), function(k) {
      (normalized_criteria[[k]] + 1) * criteria_weights[[i]]$Weight[k]
    }))
    
    # Step 3: Compute the border approximation area for each criterion
    gj <- apply(weighted_matrix, 2, function(x) prod(x)^(1/nrow(weighted_matrix)))
    
    # Step 4: Compute the distance from each alternative to the border approximation area
    q_matrix <- as.data.frame(sapply(1:ncol(weighted_matrix), function(f) {
      weighted_matrix[[f]] - gj[f]
    }))
    colnames(q_matrix) <- colnames(weighted_matrix)
    
    # Step 5: Compute the final MABAC scores for each alternative
    final_scores <- rowSums(q_matrix)
    
    # Step 6: Create a final dataframe with municipalities and their MABAC scores
    mabac_results[[i]] <- data.frame(Municipality = case$Municipality, Code = case$Code, Score = final_scores)

}

## Saving indicator files ##

{
  saveRDS(criteria_weights, file="Criteria_weights.RData")
  saveRDS(mabac_results, file="Ranking.RData")
}


########### Analysys #######################################################

### SDGs - Distribution

rename_dict <- c(
  "Porcentage of population living with at most 1/2 minimum wage (%)" = "Low_Income_Pop (%)",
  "Employed population (%)" = "Employment (%)",
  "Natural forests (%)" = "Natural_Forest (%)",
  "Households with access to electric energy (%)" = "Elec_Access (%)",
  "Planted or destined to harvest area (%)" = "Harvest_Area (%)",
  "Maximum capcacity of reservoirs (m^3)" = "Reservoir_Cap (m³)",
  "Nature conservation unit demarcated area(%)" = "Protected_Area (%)"
)

sdg_criteria_2 <- sdg_criteria
colnames(sdg_criteria_2)[(ncol(sdg_criteria_2)-6):ncol(sdg_criteria_2)] <- rename_dict

criteria_cols <- tail(names(sdg_criteria_2), 7)

ordered_criteria <- c(
  "Low_Income_Pop (%)",
  "Harvest_Area (%)",
  "Reservoir_Cap (m³)",
  "Elec_Access (%)",
  "Employment (%)",
  "Natural_Forest (%)",
  "Protected_Area (%)"
)

png("Combined_histograms.png", width = 1200, height = 600)

par(mfrow = c(2, 4))

for (crit in ordered_criteria) {
  hist(sdg_criteria_2[[crit]], 
       main = crit, 
       xlab = "", 
       col = "lightblue", 
       breaks = 20,
       cex.main = 2,    
       cex.axis = 1.5,   
       cex.lab = 1.7)
}

dev.off()


### Weights analysis


setwd("C:/Users/gusta/OneDrive/Área de Trabalho/Doutorado/Submissões/2025/ISF/03_Indicator_files_output") # Change your path 
criteria_weights<-readRDS("Criteria_weights.RData")
mabac_results<-readRDS("Ranking.RData")

wind_weight<-vector()

for (p in 1:1000) {
  wind_weight[p]<-sum(criteria_weights[[p]][c(8:19),2])
}

summary(wind_weight)

### Weights distribution 

rename_dict <- c(
  "Porcentage of population living with at most 1/2 minimum wage (%)" = "Low_Income_Pop",
  "Employed population (%)" = "Employment",
  "Natural forests (%)" = "Natural_Forest",
  "Households with access to electric energy (%)" = "Elec_Access",
  "Planted or destined to harvest area (%)" = "Harvest_Area",
  "Maximum capcacity of reservoirs (m^3)" = "Reservoir_Cap",
  "Nature conservation unit demarcated area(%)" = "Protected_Area",
  "wind_1" = "Wind_Jan",
  "wind_2" = "Wind_Feb",
  "wind_3" = "Wind_Mar",
  "wind_4" = "Wind_Apr",
  "wind_5" = "Wind_May",
  "wind_6" = "Wind_Jun",
  "wind_7" = "Wind_Jul",
  "wind_8" = "Wind_Aug",
  "wind_9" = "Wind_Sep",
  "wind_10" = "Wind_Oct",
  "wind_11" = "Wind_Nov",
  "wind_12" = "Wind_Dec"
)

criteria_weights_2 <- lapply(criteria_weights, function(df) {
  df$Criteria <- recode(df$Criteria, !!!rename_dict)
  return(df)
})

df_combined <- bind_rows(criteria_weights_2, .id = "Scenario")

ordered_criteria <- c(
  "Low_Income_Pop",
  "Harvest_Area",
  "Reservoir_Cap",
  "Elec_Access",
  "Employment",
  "Natural_Forest",
  "Protected_Area",
  "Wind_Jan", "Wind_Feb", "Wind_Mar", "Wind_Apr", "Wind_May", "Wind_Jun",
  "Wind_Jul", "Wind_Aug", "Wind_Sep", "Wind_Oct", "Wind_Nov", "Wind_Dec"
)

ordered_criteria <- c(
  "Reservoir_Cap",
  "Protected_Area",
  "Harvest_Area",
  "Low_Income_Pop",
  "Employment",
  "Elec_Access",
  "Natural_Forest",
  "Wind_Jan", "Wind_Feb", "Wind_Mar", "Wind_Apr", "Wind_May", "Wind_Jun",
  "Wind_Jul", "Wind_Aug", "Wind_Sep", "Wind_Oct", "Wind_Nov", "Wind_Dec"
)

df_combined$Criteria <- factor(df_combined$Criteria, levels = ordered_criteria)

graphic<-ggplot(df_combined, aes(x = Criteria, y = Weight)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Criteria", y = "Weight", title = "Distribution of Criteria Weights") +
  theme(
    plot.title = element_text(size = 12, hjust = 0),
    axis.text.x = element_text(angle = 55, hjust = 1),
    axis.text = element_text(size = 10, colour = "black"),
    axis.title = element_text(size = 12),
    panel.grid = element_blank(),         
    axis.line = element_line(color = "gray70", linewidth = 0.6),
    axis.ticks = element_line(color = "gray70", linewidth = 0.6)
  )

filename <- paste0("DistributionWeights.png")
ggsave(filename, plot = graphic, width = 7.5, height = 5, dpi = 300)

### Sort each dataframe within the list in ascending order of Score

sorted_mabac_results <- lapply(mabac_results, function(df) {
  df[order(df$Score, decreasing = TRUE), ]  
})

# Create a dataframe to store the municipalities' positions in each scenario
ranking_matrix <- do.call(cbind, lapply(sorted_mabac_results, function(df) df$Municipality))
ranking_df <- as.data.frame(ranking_matrix)
municipalities <- unique(ranking_df[,1])  
positions <- data.frame(Municipality = municipalities)

# Calculate the mean, standard deviation, minimum and maximum position for each municipality across the 1000 rankings
positions <- positions %>%
  rowwise() %>%
  mutate(
    Mean = mean(which(ranking_df == Municipality, arr.ind = TRUE)[,1]),
    StdDev = sd(which(ranking_df == Municipality, arr.ind = TRUE)[,1]), 
    Median = median(which(ranking_df == Municipality, arr.ind = TRUE)[,1]),
    MinPosition = min(which(ranking_df == Municipality, arr.ind = TRUE)[,1]),  
    MaxPosition = max(which(ranking_df == Municipality, arr.ind = TRUE)[,1])   
  ) %>%
  ungroup()

positions <- positions %>%
  mutate(Range = MaxPosition-MinPosition) %>%
  mutate(CV = StdDev / Mean)

# Count how many times each municipality appears in the Top 10
top10_freq <- table(unlist(lapply(sorted_mabac_results, function(df) df$Municipality[1:10])))
top10_freq_df <- as.data.frame(top10_freq) %>%
  rename(Municipality = Var1, Frequency = Freq) %>%
  arrange(desc(Frequency))

top10_freq_df$Frequency<-top10_freq_df$Frequency/number_scenarios

top10_freq_df<-top10_freq_df[c(1:20),]

# Visualize the most frequent municipalities in the Top 10
graphic<-ggplot(top10_freq_df, aes(x = reorder(Municipality, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frequency of Municipalities in the Top 10",
       x = "Municipality", y = "Relative Frequency") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1),labels = label_number(accuracy = 0.1)) +
  theme_minimal() +
  theme(
      plot.title = element_text(size = 14, hjust = 0),
      #panel.grid = element_blank(),         
      #axis.line = element_line(color = "gray60", size = 0.8), 
      axis.text = element_text(size = 11, colour = "black"),   
      axis.title = element_text(size = 12),  
      #axis.ticks = element_line(color = "gray60", size = 0.8),
      #legend.position = "bottom", 
      #legend.title = element_blank(), 
      #legend.text = element_text(size = 14),
      #legend.key.width = unit(1, "cm"),
      panel.grid = element_blank(),         
      axis.line = element_line(color = "gray70", size = 0.6),
      axis.ticks = element_line(color = "gray70", size = 0.6)
    ) 

filename <- paste0("Freq_Top10.png")
ggsave(filename, plot = graphic, width = 9, height = 5, dpi = 300)

# Create a boxplot to visualize the distribution of municipalities' positions
ranking_long <- bind_rows(lapply(seq_along(sorted_mabac_results), function(i) {
  sorted_mabac_results[[i]] %>%
    mutate(Position = row_number(),  
           Scenario = i)  
}))

# Filter only the top 10 municipalities that appear most frequently in the Top 10
top_municipalities <- ranking_long %>%
  group_by(Municipality) %>%
  summarise(
    MedianRank = median(Position),
    StdDevRank = sd(Position)
  ) %>%
  arrange(MedianRank, StdDevRank) %>% 
  head(10) %>%
  pull(Municipality)

filtered_ranking <- ranking_long %>% 
  filter(Municipality %in% top_municipalities)

ordered_municipalities <- filtered_ranking %>%
  group_by(Municipality) %>%
  summarise(Median_Position = median(Position),
            SD_Position = sd(Position)) %>%  
  arrange(Median_Position, SD_Position) %>%
  pull(Municipality)

filtered_ranking <- filtered_ranking %>%
  mutate(Municipality = factor(Municipality, levels = ordered_municipalities))

# Create the boxplot
graphic<-ggplot(filtered_ranking, aes(x = Municipality, y = Position)) +
  geom_boxplot(width = 0.2, outlier.size = 0.5, alpha = 0.8) +
  coord_flip() +  # Rotate the plot
  theme_minimal() +  
  labs(title = "Distribution of Municipality Rankings",
       x = "Municipality", 
       y = "Ranking Position") +
  scale_y_continuous(breaks = seq(1, max(filtered_ranking$Position), by = 7), 
                     labels = function(x) paste0(x, "º"), limits = c(1,70))+
  scale_x_discrete(limits = rev(levels(factor(filtered_ranking$Municipality))))+
  theme(
    #axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0),
    #panel.grid = element_blank(),         
    #axis.line = element_line(color = "gray60", size = 0.8), 
    axis.text = element_text(size = 11, colour = "black"),   
    axis.title = element_text(size = 12),  
    #axis.ticks = element_line(color = "gray60", size = 0.8),
    #legend.position = "bottom", 
    #legend.title = element_blank(), 
    #legend.text = element_text(size = 14),
    #legend.key.width = unit(1, "cm")
    panel.grid = element_blank(),         
    axis.line = element_line(color = "gray70", size = 0.6),
    axis.ticks = element_line(color = "gray70", size = 0.6)
    ) 

filename <- paste0("DistributionRankings.png")
ggsave(filename, plot = graphic, width = 10, height = 5, dpi = 300)

###### Wind version #####

criteria_weights_2<-list()
mabac_results_2<-list()

for (i in 1:number_scenarios) {
  
  wind_scenario <- data.frame(sapply(all_fut_scenarios, function(df) df[(((i - 1)*horizon)+1):(i*horizon), 4])) 
  colnames(wind_scenario) <- sub("^X", "", colnames(wind_scenario))
  
  #colnames(wind_scenario) <- paste0("municip", 1:length(all_fut_scenarios))
  wind_scenario <- as.data.frame(t(wind_scenario))
  colnames(wind_scenario) <- paste0("wind_", 1:horizon)
  wind_scenario$Code <- as.numeric(rownames(wind_scenario))
  
  case <- merge(sdg_criteria, wind_scenario, by = "Code", all.x = TRUE)
  
  #case <- cbind(sdg_criteria, wind_scenario)
  case <- case %>%
    mutate(across(3:21, as.numeric))
  
  ########################## CRITIC ##############################
  
  # Step 1: Select the criteria columns
  criteria <- case[, 10:21]
  
  # Step 2: Identify benefit-based and cost-based criteria
  benefit_indices <- c(1:12)  
  
  # Step 3: Normalize the criteria based on the type

  normalized_criteria <- as.data.frame(sapply(1:ncol(criteria), function(j) {
    if (j %in% benefit_indices) {
      normalize(criteria[[j]], "benefit")
    } else {
      normalize(criteria[[j]], "cost")
    }
  }))
  
  # Step 4: Compute standard deviation for each criterion
  std_dev <- apply(normalized_criteria, 2, sd, na.rm = TRUE)
  
  # Step 5: Compute Spearman correlation matrix
  cor_matrix <- cor(normalized_criteria, method = "spearman", use = "complete.obs")
  
  # Step 6: Compute information amount for each criterion
  info_amount <- std_dev * rowSums(1 - cor_matrix, na.rm = TRUE)
  
  # Step 7: Compute the weights
  weights <- info_amount / sum(info_amount)
  
  # Step 8: Create a dataframe with criteria names and their corresponding weights
  criteria_weights_2[[i]] <- data.frame(Criteria = colnames(criteria), Weight = weights)
  
  
  ########################## MABAC ##############################
  
  # Step 1: Use the normalized decision matrix from the CRITIC method
  # (Assuming `normalized_criteria` and `criteria_weights` are available from the previous step)
  
  # Step 2: Compute the weighted matrix
  weighted_matrix <- as.data.frame(sapply(1:ncol(normalized_criteria), function(k) {
    (normalized_criteria[[k]] + 1) * criteria_weights_2[[i]]$Weight[k]
  }))
  
  # Step 3: Compute the border approximation area for each criterion
  gj <- apply(weighted_matrix, 2, function(x) prod(x)^(1/nrow(weighted_matrix)))
  
  # Step 4: Compute the distance from each alternative to the border approximation area
  q_matrix <- as.data.frame(sapply(1:ncol(weighted_matrix), function(f) {
    weighted_matrix[[f]] - gj[f]
  }))
  colnames(q_matrix) <- colnames(weighted_matrix)
  
  # Step 5: Compute the final MABAC scores for each alternative
  final_scores <- rowSums(q_matrix)
  
  # Step 6: Create a final dataframe with municipalities and their MABAC scores
  mabac_results_2[[i]] <- data.frame(Municipality = case$Municipality, Code = case$Code, Score = final_scores)
  
}

## Saving indicator files ##

{
  saveRDS(criteria_weights_2, file="Criteria_weights_2.RData")
  saveRDS(mabac_results_2, file="Ranking_2.RData")
}

########### Analysys #######################################################

setwd("C:/Users/gusta/OneDrive/Área de Trabalho/Doutorado/Submissões/2025/ISF/03_Indicator_files_output") # Change your path 
criteria_weights_2<-readRDS("Criteria_weights_2.RData")
mabac_results_2<-readRDS("Ranking_2.RData")

#Sort each dataframe within the list in ascending order of Score

sorted_mabac_results <- lapply(mabac_results_2, function(df) {
  df[order(df$Score, decreasing = TRUE), ]  
})

# Create a dataframe to store the municipalities' positions in each scenario
ranking_matrix <- do.call(cbind, lapply(sorted_mabac_results, function(df) df$Municipality))
ranking_df <- as.data.frame(ranking_matrix)
municipalities <- unique(ranking_df[,1])  
positions <- data.frame(Municipality = municipalities)

# Calculate the mean, standard deviation, minimum and maximum position for each municipality across the 1000 rankings
positions <- positions %>%
  rowwise() %>%
  mutate(
    Mean = mean(which(ranking_df == Municipality, arr.ind = TRUE)[,1]),
    StdDev = sd(which(ranking_df == Municipality, arr.ind = TRUE)[,1]), 
    Median = median(which(ranking_df == Municipality, arr.ind = TRUE)[,1]),
    MinPosition = min(which(ranking_df == Municipality, arr.ind = TRUE)[,1]),  
    MaxPosition = max(which(ranking_df == Municipality, arr.ind = TRUE)[,1])   
  ) %>%
  ungroup()

positions <- positions %>%
  mutate(CV = StdDev / Mean)

# Count how many times each municipality appears in the Top 10
top10_freq <- table(unlist(lapply(sorted_mabac_results, function(df) df$Municipality[1:10])))
top10_freq_df_2 <- as.data.frame(top10_freq) %>%
  rename(Municipality = Var1, Frequency = Freq) %>%
  arrange(desc(Frequency))

top10_freq_df_2<-top10_freq_df_2[c(1:30),]

# Visualize the most frequent municipalities in the Top 10
ggplot(top10_freq_df_2, aes(x = reorder(Municipality, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Frequency of Municipalities in the Top 10",
       x = "Municipality", y = "Frequency") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100))

# Create a boxplot to visualize the distribution of municipalities' positions
ranking_long <- bind_rows(lapply(seq_along(sorted_mabac_results), function(i) {
  sorted_mabac_results[[i]] %>%
    mutate(Position = row_number(),  
           Scenario = i)  
}))

# Filter only the top 10 municipalities that appear most frequently in the Top 10
top_municipalities <- ranking_long %>%
  group_by(Municipality) %>%
  summarise(MeanRank = mean(Position)) %>%
  arrange(MeanRank) %>%
  head(10) %>%
  pull(Municipality)

filtered_ranking <- ranking_long %>% 
  filter(Municipality %in% top_municipalities)

ordered_municipalities <- filtered_ranking %>%
  group_by(Municipality) %>%
  summarise(Median_Position = median(Position),
            SD_Position = sd(Position)) %>%  
  arrange(desc(Median_Position), SD_Position) %>%  # Sorts by highest median and lowest standard deviation
  pull(Municipality)

filtered_ranking <- filtered_ranking %>%
  mutate(Municipality = factor(Municipality, levels = ordered_municipalities))

# Create the boxplot
ggplot(filtered_ranking, aes(x = Municipality, y = Position)) +
  geom_boxplot(width = 0.2, outlier.size = 0.5, alpha = 0.8) +
  coord_flip() +  # Rotate the plot
  theme_minimal() +  
  theme(axis.text.y = element_text(size = 10)) + 
  labs(title = "Distribution of Municipality Rankings",
       x = "Municipality", 
       y = "Ranking Position") +
  scale_y_continuous(breaks = seq(1, max(filtered_ranking$Position), by = 8), 
                     labels = function(x) paste0(x, "º"), limits = c(1,75))

