library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

# Importing Data Sets

Continent_Classification <- read.csv("data sets/continents-according-to-our-world-in-data.csv")
GDP_per_capita <- read.csv("data sets/gdp-per-capita-worldbank.csv")
LDC <- read.csv("data sets/LDC.csv")
World_Population <- read.csv("data sets/world population.csv")
NEET <- read.csv("data sets/youth-not-in-education-employment-training.csv")


# Rename Columns

colnames(GDP_per_capita) <- c("Country", "Code", "Year", "GDP_Per_Capita")
colnames(Continent_Classification) <- c("Country", "Code", "Year", "Continent")
colnames(World_Population) <- c("Country", "Code", "Year", "Population")


# Convert columns to correct types

GDP_per_capita <- GDP_per_capita %>%
  mutate(
    Country = as.character(Country),
    Code = as.character(Code),
    Year = as.integer(Year),
    GDP_Per_Capita = map_dbl(GDP_Per_Capita, as.numeric)
  )

Continent_Classification <- Continent_Classification %>%
  mutate(
    Country = as.character(Country),
    Code = as.character(Code),
    Year = as.integer(Year),
    Continent = as.character(Continent)
  )

World_Population <- World_Population %>%
  mutate(
    Country = as.character(Country),
    Code = as.character(Code),
    Year = as.integer(Year),
    Population = as.numeric(Population)
  )

# Combine Data Sets

GDP_Continent_Population_Combined <- GDP_per_capita %>%
  left_join(Continent_Classification %>% select(Code, Continent), by = "Code") %>%
  left_join(World_Population %>% select(Code, Year, Population), by = c("Code", "Year"))


GDP_Continent_Population_Combined <- GDP_Continent_Population_Combined %>%
  drop_na()


# LDC Classification

GDP_Continent_Population_Combined <- GDP_Continent_Population_Combined %>%
  mutate(LDC_Status = Country %in% LDC$Country & Year %in% LDC$Year)

# Add growth rate

GDP_Continent_Population_Combined <- GDP_Continent_Population_Combined %>%
  arrange(Country, Year) %>%  
  group_by(Country) %>%
  mutate(GDP_growth_rate = (GDP_Per_Capita - lag(GDP_Per_Capita)) / lag(GDP_Per_Capita) * 100) %>%
  ungroup()


# Determine Fair growth Target

# Europe
europemean <- GDP_Continent_Population_Combined %>%
  filter(Continent == "Europe") %>%
  filter(Year >= 2009 & Year <= 2021) %>%
  summarise(mean_gdp_pc = mean(GDP_Per_Capita, na.rm = TRUE))

# Asia
asiamean <- GDP_Continent_Population_Combined %>%
  filter(Continent == "Asia") %>%
  filter(Year >= 2009 & Year <= 2021) %>%
  summarise(mean_gdp_pc = mean(GDP_Per_Capita, na.rm = TRUE))

# Africa
africamean <- GDP_Continent_Population_Combined %>%
  filter(Continent == "Africa") %>%
  filter(Year >= 2009 & Year <= 2021) %>%
  summarise(mean_gdp_pc = mean(GDP_Per_Capita, na.rm = TRUE))

# North America
northamericamean <- GDP_Continent_Population_Combined %>%
  filter(Continent == "North America") %>%
  filter(Year >= 2009 & Year <= 2021) %>%
  summarise(mean_gdp_pc = mean(GDP_Per_Capita, na.rm = TRUE))

# South America
southamericamean <- GDP_Continent_Population_Combined %>%
  filter(Continent == "South America") %>%
  filter(Year >= 2009 & Year <= 2021) %>%
  summarise(mean_gdp_pc = mean(GDP_Per_Capita, na.rm = TRUE))

# Oceania
oceaniamean <- GDP_Continent_Population_Combined %>%
  filter(Continent == "Oceania") %>%
  filter(Year >= 2009 & Year <= 2021) %>%
  summarise(mean_gdp_pc = mean(GDP_Per_Capita, na.rm = TRUE))



# Combine your six means into one data frame
means_df <- data.frame(
  Continent = c("Europe", "Asia", "Africa", "North America", "South America", "Oceania"),
  mean_gdp_pc = c(
    europemean$mean_gdp_pc,
    asiamean$mean_gdp_pc,
    africamean$mean_gdp_pc,
    northamericamean$mean_gdp_pc,
    southamericamean$mean_gdp_pc,
    oceaniamean$mean_gdp_pc
  )
)

# Plot the line of best fit and average of each continent
TargetGrowth <- GDP_Continent_Population_Combined %>%
  filter(GDP_growth_rate > -10, GDP_growth_rate < 10, GDP_Per_Capita < 60000) %>%
  ggplot(aes(x = GDP_Per_Capita, y = GDP_growth_rate)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_vline(data = means_df,
             aes(xintercept = mean_gdp_pc, color = Continent),
             linetype = "dashed", size = 0.7) +
  labs(title = "GDP per Capita vs GDP Growth Rate with Continent Means",
       x = "GDP per Capita", y = "GDP Growth Rate (%)",
       color = "Continent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# output


# 1) Fit the linear model on your filtered data (same filters as plot)
plot_df <- GDP_Continent_Population_Combined %>%
  filter(GDP_growth_rate > -10,
         GDP_growth_rate < 10,
         GDP_Per_Capita < 60000)

m <- lm(GDP_growth_rate ~ GDP_Per_Capita, data = plot_df)

# 2) Combine your six mean values
means_df <- data.frame(
  Continent    = c("Europe", "Asia", "Africa", "North America", "South America", "Oceania"),
  mean_gdp_pc  = c(
    europemean$mean_gdp_pc,
    asiamean$mean_gdp_pc,
    africamean$mean_gdp_pc,
    northamericamean$mean_gdp_pc,
    southamericamean$mean_gdp_pc,
    oceaniamean$mean_gdp_pc
  )
)

# 3) Predict growth at each mean GDP per capita (these are your 6 values)
means_df$predicted_growth_at_mean <- predict(m, newdata = data.frame(GDP_Per_Capita = means_df$mean_gdp_pc))

# 4) Output neatly (rounded)
result <- means_df %>%
  mutate(
    mean_gdp_pc = round(mean_gdp_pc, 2),
    predicted_growth_at_mean = round(predicted_growth_at_mean, 3)
  )

result



# filter the years from 2009-2021
GDP_Continent_Population_Combined <- GDP_Continent_Population_Combined %>%
  filter(Year >= 2009 & Year <= 2021)


# Compute Continent Growth

continent_growth <- GDP_Continent_Population_Combined %>%
  group_by(Continent, Year) %>%
  summarise(
    weighted_gdp_pc = sum(GDP_Per_Capita * Population, na.rm = TRUE) / sum(Population, na.rm = TRUE), # weighted gdp_pc
    average_gdp_pc  = mean(GDP_Per_Capita, na.rm = TRUE),  # average gdp_pc
    .groups = "drop"
  ) %>%
  arrange(Continent, Year) %>%
  group_by(Continent) %>%
  mutate(weighted_growth = (weighted_gdp_pc / lag(weighted_gdp_pc) - 1) * 100) %>% # growth rate weighted by population
  mutate(average_growth = (average_gdp_pc / lag(average_gdp_pc) - 1) * 100) %>% # Average growth rate
  ungroup()


# NA during the years will affect accuracy of gdp growth rate
na_counts <- GDP_Continent_Population_Combined %>%
  group_by(Continent, Year) %>%
  summarise(
    na_GDP_growth_rate = sum(is.na(GDP_growth_rate)),
    .groups = "drop"
  ) %>%
  arrange(Continent, Year)
# Europe in 1995 is unreliable because 8 country have NA growth rate


# Plotting the graph for Continents



# Africa (threshold: 3%)
Africa_growth <- ggplot(subset(continent_growth, Continent == "Africa"), aes(x = Year)) +
  geom_line(aes(y = weighted_growth, color = "Weighted")) +
  geom_point(aes(y = weighted_growth, color = "Weighted"), size = 0.75) +
  geom_line(aes(y = average_growth, color = "Average")) +
  geom_point(aes(y = average_growth, color = "Average"), size = 0.75) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(title = "Africa", y = "Growth Rate (%)", x = NULL) +
  theme_minimal()

# Asia (threshold: 2%)
Asia_growth <- ggplot(subset(continent_growth, Continent == "Asia"), aes(x = Year)) +
  geom_line(aes(y = weighted_growth, color = "Weighted")) +
  geom_point(aes(y = weighted_growth, color = "Weighted"), size = 0.75) +
  geom_line(aes(y = average_growth, color = "Average")) +
  geom_point(aes(y = average_growth, color = "Average"), size = 0.75) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(title = "Asia", y = "Growth Rate (%)", x = NULL) +
  theme_minimal()

# Europe (threshold: 1.6%)
Europe_growth <- ggplot(subset(continent_growth, Continent == "Europe"), aes(x = Year)) +
  geom_line(aes(y = weighted_growth, color = "Weighted")) +
  geom_point(aes(y = weighted_growth, color = "Weighted"), size = 0.75) +
  geom_line(aes(y = average_growth, color = "Average")) +
  geom_point(aes(y = average_growth, color = "Average"), size = 0.75) +
  geom_hline(yintercept = 1.6, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(title = "Europe", y = "Growth Rate (%)", x = NULL) +
  theme_minimal()

# North America (threshold: 1.8%)
NorthAmerica_growth <- ggplot(subset(continent_growth, Continent == "North America"), aes(x = Year)) +
  geom_line(aes(y = weighted_growth, color = "Weighted")) +
  geom_point(aes(y = weighted_growth, color = "Weighted"), size = 0.75) +
  geom_line(aes(y = average_growth, color = "Average")) +
  geom_point(aes(y = average_growth, color = "Average"), size = 0.75) +
  geom_hline(yintercept = 1.8, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(title = "North America", y = "Growth Rate (%)", x = NULL) +
  theme_minimal()

# South America (threshold: 2%)
SouthAmerica_growth <- ggplot(subset(continent_growth, Continent == "South America"), aes(x = Year)) +
  geom_line(aes(y = weighted_growth, color = "Weighted")) +
  geom_point(aes(y = weighted_growth, color = "Weighted"), size = 0.75) +
  geom_line(aes(y = average_growth, color = "Average")) +
  geom_point(aes(y = average_growth, color = "Average"), size = 0.75) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(title = "South America", y = "Growth Rate (%)", x = NULL) +
  theme_minimal()

# Oceania (threshold: 2%)
Oceania_growth <- ggplot(subset(continent_growth, Continent == "Oceania"), aes(x = Year)) +
  geom_line(aes(y = weighted_growth, color = "Weighted")) +
  geom_point(aes(y = weighted_growth, color = "Weighted"), size = 0.75) +
  geom_line(aes(y = average_growth, color = "Average")) +
  geom_point(aes(y = average_growth, color = "Average"), size = 0.75) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(title = "Oceania", y = "Growth Rate (%)", x = NULL) +
  theme_minimal()


# Combine all six with one shared legend
Growth_Combined_Plot <- (Africa_growth | Asia_growth | Europe_growth) /
  (NorthAmerica_growth | SouthAmerica_growth | Oceania_growth) +
  plot_annotation(title = "GDP per Capita Growth by Continent",
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")




# Share of LDC meeting the 7% target every year

ldc_target_share <- GDP_Continent_Population_Combined %>%
  filter(LDC_Status) %>%                                   # keep only LDCs
  group_by(Continent, Year) %>%
  summarise(
    n_ldc         = n_distinct(Country),                   # LDC count observed
    n_meet        = n_distinct(Country[GDP_growth_rate >= 7]),
    share_meeting = if_else(n_ldc > 0, 100 * n_meet / n_ldc, NA_real_), # percent
    .groups = "drop"
  )

# Plot the share of LDC meeting target

LDCShare <- ggplot(ldc_target_share, aes(x = Year, y = share_meeting, color = Continent)) +
  geom_line(size = 0.5) +
  geom_point(size=0.7) +
  labs(
    title = "Share of LDCs Meeting the UN ≥7% GDP Growth Target by Continents",
    x = "Year",
    y = "% of LDCs meeting target",
    color = "Continent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)   # center title
  )


# The growth rate of LDC


# LDC GDP per capita growth (weighted vs average) by continent-year

ldc_continent_growth <- GDP_Continent_Population_Combined %>%
  filter(LDC_Status) %>%
  group_by(Continent, Year) %>%
  summarise(
    weighted_gdp_pc_ldc = sum(GDP_Per_Capita * Population, na.rm = TRUE) / 
      sum(Population, na.rm = TRUE),
    average_gdp_pc_ldc  = mean(GDP_Per_Capita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Continent, Year) %>%
  group_by(Continent) %>%
  mutate(
    weighted_growth_ldc = (weighted_gdp_pc_ldc / lag(weighted_gdp_pc_ldc) - 1) * 100,
    average_growth_ldc  = (average_gdp_pc_ldc  / lag(average_gdp_pc_ldc)  - 1) * 100
  ) %>%
  ungroup()

# Africa

Africa_LDC <- ggplot(subset(ldc_continent_growth, Continent == "Africa"), aes(x = Year)) +
  geom_line(aes(y = weighted_growth_ldc, color = "Weighted"), linewidth = 1) +
  geom_point(aes(y = weighted_growth_ldc, color = "Weighted"), size = 1.5) +
  geom_line(aes(y = average_growth_ldc,  color = "Average"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = average_growth_ldc,  color = "Average"), size = 1.5) +
  scale_color_manual(values = c("Weighted" = "#1f77b4", "Average" = "#ff7f0e")) +
  labs(title = "Africa LDC GDP per Capita Growth", y = "Growth (%)", x = NULL, color = "Measure") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Asia

# Graph after 1995 because Maldive is included as LDC in 1995 which caused distortion
ldc_continent_growth_Asia <- ldc_continent_growth %>%
  filter(Continent == "Asia", Year %in% 1996:2021)

Asia_LDC <- ggplot(ldc_continent_growth_Asia, aes(x = Year)) +
  geom_line(aes(y = weighted_growth_ldc, color = "Weighted"), linewidth = 1) +
  geom_point(aes(y = weighted_growth_ldc, color = "Weighted"), size = 1.5) +
  geom_line(aes(y = average_growth_ldc,  color = "Average"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = average_growth_ldc,  color = "Average"), size = 1.5) +
  scale_color_manual(values = c("Weighted" = "#1f77b4", "Average" = "#ff7f0e")) +
  labs(title = "Asia LDC GDP per Capita Growth", y = "Growth (%)", x = NULL, color = "Measure") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# The sharp decline in 2020 was due to Maldives, having a -34% decline

# Oceania

Oceania_LDC <- ggplot(subset(ldc_continent_growth, Continent == "Oceania"), aes(x = Year)) +
  geom_line(aes(y = weighted_growth_ldc, color = "Weighted"), linewidth = 1) +
  geom_point(aes(y = weighted_growth_ldc, color = "Weighted"), size = 1.5) +
  geom_line(aes(y = average_growth_ldc,  color = "Average"), linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = average_growth_ldc,  color = "Average"), size = 1.5) +
  scale_color_manual(values = c("Weighted" = "#1f77b4", "Average" = "#ff7f0e")) +
  labs(title = "Oceania LDC GDP per Capita Growth", y = "Growth (%)", x = NULL, color = "Measure") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


# Box Plot to Show Distribution

# Common y-limits across the three continents (ignoring NAs)
ylim_range <- ldc_continent_growth %>%
  filter(Continent %in% c("Africa", "Asia", "Oceania")) %>%
  pull(weighted_growth_ldc) %>%
  range(na.rm = TRUE)

# Africa
Africa_box <- ggplot(subset(ldc_continent_growth, Continent == "Africa" & !is.na(weighted_growth_ldc)),
                     aes(x = "Africa", y = weighted_growth_ldc)) +
  geom_boxplot(fill = "#1f77b4") +
  coord_cartesian(ylim = ylim_range) +
  labs(title = "Africa", x = NULL, y = "Growth Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Asia
Asia_box <- ggplot(subset(ldc_continent_growth, Continent == "Asia" & !is.na(weighted_growth_ldc)),
                   aes(x = "Asia", y = weighted_growth_ldc)) +
  geom_boxplot(fill = "#1f77b4") +
  coord_cartesian(ylim = ylim_range) +
  labs(title = "Asia", x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Oceania
Oceania_box <- ggplot(subset(ldc_continent_growth, Continent == "Oceania" & !is.na(weighted_growth_ldc)),
                      aes(x = "Oceania", y = weighted_growth_ldc)) +
  geom_boxplot(fill = "#1f77b4") +
  coord_cartesian(ylim = ylim_range) +
  labs(title = "Oceania", x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Combine into one row with a shared title
LDC_Distribution_Combined <- (Africa_box | Asia_box | Oceania_box) +
  plot_annotation(
    title = "Distribution of Weighted LDC GDP per Capita Growth",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )


# Compare Caribbean with rest

GDP_Continent_Population_Combined_NorthAmerica <- GDP_Continent_Population_Combined %>%
  filter(Continent == "North America")

# Vector of Caribbean codes

caribbean <- c("ATG","BHS","BRB","CUB","DMA","DOM","GRD","HTI","JAM","KNA","LCA","VCT","TTO")

# Two region weighted growth rate
CaribbeanNorthAmerica <-GDP_Continent_Population_Combined_NorthAmerica %>%
  mutate(Group = if_else(Code %in% caribbean, "Caribbean", "Rest of North America")) %>%
  group_by(Year, Group) %>%
  summarise(
    weighted_gdp_pc = sum(GDP_Per_Capita * Population, na.rm = TRUE) / 
      sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Group, Year) %>%
  group_by(Group) %>%
  mutate(
    weighted_growth = (weighted_gdp_pc / lag(weighted_gdp_pc) - 1) * 100,
  ) %>%
  ungroup()

# Plot the graph of the two region

CaribbeanNorthAmericaGraph <- ggplot(CaribbeanNorthAmerica, aes(x = Year, y = weighted_growth, color = Group)) +
  geom_line(size = 0.75) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = c("Caribbean" = "#D62728",  # red
                                "Rest of North America" = "#7F7F7F")) +  # grey
  labs(
    title = "Caribbean vs Rest of North America",
    x = "Year",
    y = "GDP per capita growth (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )




# Africa Diversify vs Dependent


# Diversified economies 
diversified_codes <- c(
  "BWA","CPV","DJI","EGY","SWZ","GHA","KEN","MUS","MAR","NAM",
  "RWA","SEN","SYC","ZAF","TZA","TUN"
)

# Commodity-dependent economies 
commodity_codes <- c(
  "DZA","AGO","BEN","BFA","BDI","CMR","CAF","TCD","COM","COG","CIV","COD",
  "GNQ","ETH","GAB","GMB","GIN","GNB","LSO","LBR","LBY","MDG","MWI","MLI",
  "MRT","MOZ","NER","NGA","STP","SLE","SOM","SDN","TGO","UGA","ZMB","ZWE"
)


# Classify each country into Diversified vs Commodity-dependent
DiversifiedCommodity <- GDP_Continent_Population_Combined %>%
  mutate(Group = case_when(
    Code %in% diversified_codes ~ "Diversified",
    Code %in% commodity_codes   ~ "Commodity-dependent",
    TRUE                        ~ NA_character_   # drop unclassified
  )) %>%
  filter(!is.na(Group)) %>%
  group_by(Year, Group) %>%
  summarise(
    weighted_gdp_pc = sum(GDP_Per_Capita * Population, na.rm = TRUE) / 
      sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Group, Year) %>%
  group_by(Group) %>%
  mutate(
    weighted_growth = (weighted_gdp_pc / lag(weighted_gdp_pc) - 1) * 100
  ) %>%
  ungroup()


# Plot the graph
DiversifiedCommodityGraph <- ggplot(DiversifiedCommodity, 
                                    aes(x = Year, y = weighted_growth, color = Group)) +
  geom_line(size = 0.75) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = c("Diversified" = "#1f77b4", "Commodity-dependent" = "#D62728")) +
  labs(
    title = "Diversified vs Commodity-dependent",
    x = "Year",
    y = "GDP per capita growth (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Print the graph
DiversifiedCommodityGraph




