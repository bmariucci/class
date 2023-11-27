library(tidyverse)
library(ggplot2)
library(dbplyr)

Overdose <-
  '/Users/briellemariucci/Desktop/R/Unintentional_Drug_Overdose_Death_Rate_by_Race_Ethnicity.csv'
OD <- read.csv(Overdose)

#overdose count for each ethnicity over the last 3 years
#done in clas
col.graph <- OD %>% 
  filter(race_ethnicity != 'All races') %>% # != means not equal
  ggplot(aes(x=overdose_death_count, 
             y = factor(race_ethnicity),
             fill = factor(year)
  ))+
  geom_col(position = 'dodge')+
  theme_bw()+
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300)
                     # seq(0,300,by=50) 
  )+
  labs(title = "unintentional overdoses by ethnicity",
       x = "count",
       y = "ethnicity")+
  guides(fill=guide_legend(title="death count"))

col.graph #It seems the trend is that has slightly 
#gone down overall for each ethnicity



# Try geom_area
# done on Sunday 11/27 -- took 4 hours :)
thedata <- 
data.frame(year = OD$year, 
          count = OD$overdose_death_count, 
          ethnicity = OD$race_ethnicity)

# Filter out specific parts of ethnicity
filtered.data <- thedata %>%
  filter(ethnicity != 'All races')

# Stacked area chart
area.graph <-
ggplot(filtered.data, 
       aes(x = year, 
           y = count, 
           fill = ethnicity)) +
  geom_area() +
  labs(x = "year", 
       y = "count", 
       fill = "Ethnicity",
       title = 'Unintentional Overdoses in San Francisco between 2020-2022',
       caption = 'data sourced from DataSF- updated October 3, 2023') +
  theme_linedraw()+
  scale_x_continuous(breaks = seq(min(filtered_data$year), 
                                  max(filtered_data$year), by = 1))+#make year whole integers
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700))#make count by 100

# I dont love stacked graphs and perhaps would want to see the 
#counts by ethnicity overlapped so the y-axis is smaller 
#and each value corresponds to the exact count but I am proud!!! 

area.graph
col.graph

#Changing colors
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(colorspace)


area.graph.color <-
  ggplot(filtered.data, 
         aes(x = year, 
             y = count, 
             fill = ethnicity)) +
  geom_area() +
  geom_area(color = "white", linewidth = 0.4) +  # Add a white border around the filled areas
  scale_fill_discrete_sequential(palette = 'viridis')+ #change fill
  labs(x = "year", 
       y = "count", 
       fill = "Ethnicity",
       title = 'Unintentional Overdoses in San Francisco 2020-2022',
       caption = 'data sourced from DataSF- updated October 3, 2023') +
  theme_linedraw()+
  scale_x_continuous(breaks = seq(min(filtered_data$year), 
                                  max(filtered_data$year), by = 1))+
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700))
  )


area.graph.color #Yahoo!!! 


