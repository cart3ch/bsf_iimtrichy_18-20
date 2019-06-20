#### Use case for dplyr - Reworking the first assignment

library(dplyr)
library(readxl)
housing<- read_excel("BSF G2T10/Assignment1/Housing.xlsx", sheet = 1)

## question1
housing %>% # %>% is called the pipe symbol 
  select(price, lotsize, bedrooms) %>%
  View()

## question2
housing %>%
  filter(bedrooms==4) %>%
  summarise(meanp = mean(price))

## question3
housing %>%
  mutate(pricels = price/lotsize) %>%
  select(pricels) %>%
  summary(pricels)  

## question4
housing %>%
  group_by(stories) %>%
  summarise(meanp = mean(price), medianp=median(price), 
            maxp=max(price))

## question6
housing%>%
  arrange(-lotsize)%>%
  head()

housing%>%
  arrange(lotsize)%>%
  tail(10)
