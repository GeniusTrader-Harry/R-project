library(tidyverse)
library(ggplot2)
library(dplyr)
setwd("C:/Users/User/Downloads")

youth_NEET <- read.csv("youth-not-in-education-employment-training.csv")
continents <- read.csv("continents-according-to-our-world-in-data.csv")

#tidy columns
youth_NEET_continent_1 <- inner_join(youth_NEET, continents, by = "Entity")
youth_NEET_continent <- youth_NEET_continent_1 %>% select(-Code.y, -Year.y)

#rename
youth_NEET_continent <- youth_NEET_continent %>% 
  rename(NEET = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population. , Year = Year.x)

#calculate mean
mean_NEET_continents <- youth_NEET_continent %>% 
  group_by(Continent, Year) %>% 
  summarise(mean_NEET = mean(NEET, na.rm = TRUE))

#plotting
mean_NEET_continents %>%
  filter(Year >= 2010, Year <= 2020) %>%
  ggplot(aes(x = Year, y = mean_NEET, colour = Continent)) + 
  geom_point(size = 1.8) +
  geom_line(size = 0.8) + 
  scale_x_continuous(breaks = 2010:2020) + 
  labs(title = "Mean Youth NEET Percentage Across Continents from 2010 to 2020", y = "Mean NEET")