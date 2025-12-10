library(tidyverse)
library(dplyr)
library(ggplot2)

# Load data

GDP <- read.csv("data sets/gdp-per-capita-worldbank.csv")
NEET <- read.csv("data sets/youth-not-in-education-employment-training.csv")
Continents <- read.csv("data sets/continents-according-to-our-world-in-data.csv")

# Filter the data of NEET in Africa in 2010-2021
youth_NEET <- NEET %>%
  filter(Year >= 2010, Year <= 2020) %>%
  inner_join(Continents, Year, by = "Entity") %>%
  select(-Year.y, -Code.y) %>%
  filter(Continent == "Africa")

# Filter the data of GDP in Africa in 2010-2021

GDP <- GDP %>%
  filter(Year >= 2009, Year <= 2020) %>%
  inner_join(Continents, Year, by = "Entity") %>%
  select(-Year.y, -Code.y) %>%
  filter(Continent == "Africa")

# Calculate GDP growth rate

GDP_Growth <- GDP %>%
  group_by(Year.x) %>%
  summarise(Total_GDP = sum(GDP.per.capita..PPP..constant.2017.international...)) %>%
  mutate(GDP_Growth = ((Total_GDP - lag(Total_GDP)) / lag(Total_GDP)) * 100)

# Calculate mean NEET 

mean_youth_NEET <- youth_NEET %>%
  group_by(Year.x) %>%
  summarise(mean_NEET = mean(Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.))


# Combine the two data set for plot
GDP_Growth_NEET <- inner_join(GDP_Growth, mean_youth_NEET, by = "Year.x")


# Plot NEET and GDP growth
ggplot(GDP_Growth_NEET, aes(x = Year.x)) + 
  geom_point(aes(y = mean_NEET, colour = "Mean NEET"), size = 1.8)+
  geom_line(aes(y = mean_NEET, colour = "Mean NEET"), size = 0.8)+
  geom_point(aes(y = GDP_Growth, colour = "GDP Growth"), size = 1.8) + 
  geom_line(aes(y = GDP_Growth, colour = "GDP Growth"), size = 0.8) +
  scale_x_continuous(breaks = 2010:2020) + 
  labs(title = "GDP Growth per Capita VS Mean Youth NEET in Africa (2010-2020)", x = "Year", y = "GDP Growth per Capita and Mean Youth NEET")

