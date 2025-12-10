library(tidyverse)
library(ggplot2)
library(dplyr)
setwd("C:/Users/User/Downloads")

NEET <- read.csv("youth-not-in-education-employment-training.csv")
Continents <- read.csv("continents-according-to-our-world-in-data.csv")

NEET <- NEET %>%
  filter(Year %in% c(2015, 2020)) %>%
  inner_join(Continents, Year, by = "Entity") %>%
  select(-Year.y, -Code.y) %>%
  rename(NEET = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.)

continent_neet <- NEET %>%
  group_by(Continent, Year.x) %>%
  summarise(mean_NEET = mean(NEET, na.rm = TRUE))

continent_proportion <- continent_neet %>%
  mutate(
    NEET = mean_NEET,
    In_EET = 100 - mean_NEET
  ) %>%
  select(Continent, Year.x, NEET, In_EET)

continent_long <- continent_proportion %>%
  pivot_longer(cols = c("NEET", "In_EET"),
               names_to = "Category",
               values_to = "Proportion")

ggplot(continent_long,
       aes(x = factor(Year.x),  
           y = Proportion,
           fill = Category)) +   
  geom_col(position = "stack") +  
  facet_wrap(~ Continent, nrow = 1) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Change in Proportion of Youth NEET in Each Continent (2015–2020)",
    x = "Continents",
    y = "Proportion (%)"
  ) +
  theme_minimal()