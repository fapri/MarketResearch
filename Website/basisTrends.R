

library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

cornBasis <- readRDS("~/OneDrive - University of Missouri/FAPRI Projects/fapriProjects/precipitationAndBasis/cornBasis.rds")

soybeanBasis <- readRDS("~/OneDrive - University of Missouri/FAPRI Projects/fapriProjects/precipitationAndBasis/soybeanBasis.rds")

marketingYears = read_csv("https://raw.githubusercontent.com/fapri/MarketResearch/project/Decoupling/Basis/basisCharting/Data/marketingYears.csv")

marketingYears$Start = mdy(marketingYears$Start)
marketingYears$Stop = mdy(marketingYears$Stop)
marketingYears$interval = interval(marketingYears$Start, marketingYears$Stop)

cornBasis$date = as.Date(cornBasis$date)
cornBasis$basis = as.numeric(cornBasis$basis)

dailyAvg = cornBasis %>% 
  mutate(marketingYear = case_when(cornBasis$date %within% marketingYears$interval[1] ~ 2008,
                                   cornBasis$date %within% marketingYears$interval[2] ~ 2009,
                                   cornBasis$date %within% marketingYears$interval[3] ~ 2010,
                                   cornBasis$date %within% marketingYears$interval[4] ~ 2011,
                                   cornBasis$date %within% marketingYears$interval[5] ~ 2012,
                                   cornBasis$date %within% marketingYears$interval[6] ~ 2013,
                                   cornBasis$date %within% marketingYears$interval[7] ~ 2014,
                                   cornBasis$date %within% marketingYears$interval[8] ~ 2015,
                                   cornBasis$date %within% marketingYears$interval[9] ~ 2016,
                                   cornBasis$date %within% marketingYears$interval[10] ~ 2017,
                                   cornBasis$date %within% marketingYears$interval[11] ~ 2018,
                                   cornBasis$date %within% marketingYears$interval[12] ~ 2019,
                                   cornBasis$date %within% marketingYears$interval[13] ~ 2020))


dailyAvg = dailyAvg %>%
  mutate(date = floor_date(date)) %>%
  group_by(date, marketingYear) %>%
  summarize(basisAvg = mean(basis, na.rm = TRUE))


dailyAvg$marketingYear = as.factor(dailyAvg$marketingYear)


x = melt(dailyAvg, id="marketingYear")


dailyAvg$DayOfYear <- as.numeric(format(dailyAvg$date, "%j"))

ggplot(data = dailyAvg, aes(x = DayOfYear, y = basisAvg, group = marketingYear, color = marketingYear)) + 
  geom_line(group = dailyAvg$marketingYear) +
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))





dailyAvg$newDay = as.Date(dailyAvg$date) - 244
dailyAvg$DayOfYear2 <- as.numeric(format(dailyAvg$newDay, "%j"))

ggplot(data = dailyAvg, aes(x = DayOfYear2, y = basisAvg, group = marketingYear, color = marketingYear)) + 
  geom_line(group = dailyAvg$marketingYear) +
  scale_x_continuous(breaks = scales::pretty_breaks(n =  14),
    labels = function(x) format(as.Date(as.character(x), "%j") + 244, "%d-%b"))





format(as.Date(as.character(dailyAvg$DayOfYear2), "%j") + 244, "%d-%b")





dailyAvg$CommonDate <- as.Date(paste0("2000-",format(dailyAvg$date, "%j")), "%Y-%j") - 244



ggplot(data = dailyAvg, mapping = aes(x = CommonDate, y = basisAvg, colour = marketingYear)) +
  geom_line() +
  scale_x_date(labels = function(x) format(x, "%d-%b")) +
  theme_bw()









subset = dailyAvg[which(dailyAvg$marketingYear == 2012), ]
subset2 = dailyAvg[which(dailyAvg$marketingYear == 2013), ]

ggplot(mapping = aes(x = date, y = basisAvg, group = marketingYear, color = marketingYear)) + 
  geom_line(data = subset)

