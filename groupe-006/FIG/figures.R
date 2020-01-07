# Representation et correlation des variables
#
#

# import libraries
library(knitr)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(stringr)
library(gridExtra)
library(scales)
library(lubridate)
library(ggrepel)
library(leaflet)
library(rgdal)
library(tibble)
library(purrr)
library(splitstackshape)
library(PerformanceAnalytics) 
library(tidyr)
library(corrplot)
library(lubridate)

#Choose "dataset_df1.csv" : you can generate it with "treatment_data.R" on 'PRE' folder
df1 = read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE, na.strings=c("NA", ""))
#Choose "dataset_df2.csv" : you can generate it with "treatment_data.R" on 'PRE' folder
df2 = read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE, na.strings=c("NA", ""))
#Choose "dataset_dfnum.csv" : you can generate it with "treatment_data.R" on 'PRE' folder
dfnum = read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE, na.strings=c("NA", ""))

bystateinj <- df2 %>% group_by(state) %>% summarize(n = n(), ninj = sum(n_injured)) %>% tidyr::gather(key = "type", value = "num", ninj)
ggplot(bystateinj, aes(x = state, y = num, fill = type)) + geom_boxplot() + theme_bw()
bystatekill <- df2 %>% group_by(state) %>% summarize(n = n(), nkill = sum(n_killed))%>% tidyr::gather(key = "type", value = "num", nkill)
ggplot(bystatekill, aes(x = state, y = num, fill = type)) + geom_boxplot() + theme_bw()
bystate <- df2 %>% group_by(state) %>% summarize(n = n(), nkill = sum(n_killed), ninj = sum(n_injured)) %>% tidyr::gather(key = "type", value = "num", nkill, ninj)
ggplot(bystate, aes(x = state, y = num, fill = type)) + geom_boxplot() + theme_bw()

summary(df2$participant_age)
cor(dfnum, method = c("pearson"))
mcor <- cor(dfnum)
mcor
col<- colorRampPalette(c("blue", "white", "red"))(20) 
heatmap(x = mcor, col = col, symm = TRUE)

respear1nkni<-cor.test(dfnum$n_killed,dfnum$n_injured, method="pearson")
respear1nkni
respear1nkage<-cor.test(dfnum$n_killed,dfnum$participant_age, method="pearson")
respear1nkage

#Question 3

df3 <- subset(df1, select = c(n_killed, n_injured, state, participant_age, date))
df3$date <- ymd(df3$date)
str(df3$date)

df3=na.omit(df3)
df3$month <- month(df3$date, label=TRUE)
df3$year <- year(df3$date)
df3$month <- factor(df3$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), labels = c(1,2,3,4,5,6,7,8,9,10,11,12))

plotly::ggplotly(df3 %>% select(month) %>% count(month) %>%
                   ggplot(aes(x=month, y=n)) + geom_bar(stat='identity', fill='red') +
                   scale_y_continuous(labels=comma) +
                   labs(x='Months', y='Number of incidents', title='Incidents by Month'))
plotly::ggplotly(df3 %>% select(year, month) %>% group_by(year) %>% count(month) %>%
                   ggplot(aes(x=month, y=n)) + geom_bar(stat='identity', fill='red') +
                   scale_y_continuous(labels=comma) +  facet_grid(.~year) +
                   labs(x='Months', y='Number of incidents', title='Incidents by Month by year'))

df3num <- subset(df3, select = c(n_killed, n_injured, participant_age, month, year))
class(df3$month)
df3num$month=as.numeric(df3num$month)
df3num$year=as.numeric(df3num$year)
df3num<-na.omit(df3num)
cor(df3num, method = c("pearson","kendall","spearman"))
mcor1 <- cor(df3num)
mcor1
col<- colorRampPalette(c("blue", "white", "red"))
corrplot(mcor1, type="upper", order="hclust", tl.col="black", tl.srt=45)
chart.Correlation(df3num, histogram=TRUE, pch=19)

respear3nkni<-cor.test(df3num$n_killed,df3num$n_injured, method="pearson")
respear3nkni
respear3nkage<-cor.test(df3num$n_killed,df3num$participant_age, method="pearson")
respear3nkage
respear3niage<-cor.test(df3num$n_injured,df3num$participant_age, method="pearson")
respear3niage
respear3nkyr<-cor.test(df3num$n_killed,df3num$year, method="pearson")
respear3nkyr
respear3nimth<-cor.test(df3num$n_injured,df3num$month, method="pearson")
respear3nimth
