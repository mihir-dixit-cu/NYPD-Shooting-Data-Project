---
title: "Shooting Incidents in the Boroughs of New York City"
author: "Mihir Dixit"
date: "2023-03-06"
output: html_document
---

## Introduction

The Boroughs of New York City --Brooklyn, Queens, The Bronx, Manhattan, and Staten Island-- Are often considered their own respective environments within the greater city. With this in mind, my analysis of NYPD Shooting Incident data from the period 2006 to 2021 aims to understand how each borough's number of shooting incidents have changed over time.

## Ingestion and Tidying

I then bring in the tidyverse and lubridate libraries, followed by ingestion of the csv file from the url provided
```{r}
library(tidyverse)
library(lubridate)

url <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"

historic_shootings <- read_csv(url)
```

I then trim down to the data I will need for analysis. This includes the incident key, the date, the borough, and the murder flag. I then group the data by date and borough, and finally tally the number of 
```{r}
by_boro <- historic_shootings %>%
  mutate(INCIDENT_KEY = as.character(INCIDENT_KEY),
  OCCUR_DATE = mdy(OCCUR_DATE),
  BORO = factor(BORO),
  STATISTICAL_MURDER_FLAG = factor(STATISTICAL_MURDER_FLAG),)%>%
  select(c(INCIDENT_KEY,BORO,OCCUR_DATE,STATISTICAL_MURDER_FLAG))
```
Next I create three more tables which are the boroughs grouped by daily, monthly, and yearly incidents
```{r}
by_boro_daily <- by_boro%>%
  group_by(BORO, OCCUR_DATE)%>% 
  tally(name = "INCIDENT_COUNT")

by_boro_monthly <- by_boro_daily%>%
  group_by(BORO, month = floor_date(OCCUR_DATE, 'month'))%>%
  tally(name = "INCIDENT_COUNT")

by_boro_yearly <- by_boro_daily %>%
  group_by(BORO, year = floor_date(OCCUR_DATE, 'year'))%>%
  tally(name = "INCIDENT_COUNT")

```
 I then take a look at the total number of shooting incidents within each of the Boro's. This bar chart shows us an overall picture of where the most incidents have occured throughout the city
```{r}
daily_total_shootings<-historic_shootings%>%
  count(OCCUR_DATE, name = "Total Daily Shootings", sort = "TRUE")

ggplot(by_boro_daily, aes(x = BORO, y = INCIDENT_COUNT, fill = BORO)) + geom_bar(stat="identity") +
  scale_fill_discrete(c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))
```


While this bar chart proivdes us with a total number of incidents through 2021, I would like to know how each of the boroughs approached these numbers over time. 


##Daily Shootings as a function of Time
I then proceed to create a linear models of Incident count as a function of time for daily data. I create one for the full set of city data and one for each borough.

```{r}
mod_total <- lm(INCIDENT_COUNT ~ OCCUR_DATE, data = by_boro_daily)
summary(mod_total)

incidents_lm<-by_boro_daily %>%
  ungroup()%>%
  mutate(pred = predict(mod_total))

mod_bk <- lm(INCIDENT_COUNT ~ OCCUR_DATE, data = by_boro_daily%>%filter(BORO == "BROOKLYN"))
summary(mod_bk)

incidents_lm_bk<-by_boro_daily %>%
  filter(BORO == "BROOKLYN") %>%
  mutate(pred = predict(mod_bk))

mod_queens <- lm(INCIDENT_COUNT ~ OCCUR_DATE, data = by_boro_daily%>%filter(BORO == "QUEENS"))
summary(mod_queens)

incidents_lm_queens<-by_boro_daily %>%
  filter(BORO == "QUEENS") %>%
  mutate(pred = predict(mod_queens))

mod_bronx <- lm(INCIDENT_COUNT ~ OCCUR_DATE, data = by_boro_daily%>%filter(BORO == "BRONX"))
summary(mod_bronx)

incidents_lm_bronx<-by_boro_daily %>%
  filter(BORO == "BRONX") %>%
  mutate(pred = predict(mod_bronx))

mod_mnht <- lm(INCIDENT_COUNT ~ OCCUR_DATE, data = by_boro_daily%>%filter(BORO == "MANHATTAN"))
summary(mod_mnht)

incidents_lm_mnht<-by_boro_daily %>%
  filter(BORO == "MANHATTAN") %>%
  mutate(pred = predict(mod_mnht))

mod_si <- lm(INCIDENT_COUNT ~ OCCUR_DATE, data = by_boro_daily%>%filter(BORO == "STATEN ISLAND"))
summary(mod_si)

mod_bonus <-lm(INCIDENT_COUNT ~ OCCUR_DATE, data = by_boro_daily)
summary(mod_bonus)

incidents_lm_si<-by_boro_daily %>%
  filter(BORO == "STATEN ISLAND") %>%
  mutate(pred = predict(mod_si))


incidents_lm_total <- incidents_lm_bk %>%
  rbind(incidents_lm_bronx)%>%
  rbind(incidents_lm_queens)%>%
  rbind(incidents_lm_mnht)%>%
  rbind(incidents_lm_si)

incidents_lm <- incidents_lm %>%
  left_join(incidents_lm_total, by = c("BORO", "OCCUR_DATE"))%>%
  mutate(bonus = predict(mod_bonus))
```
