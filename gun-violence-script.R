##########################################################
#                                                        #
# PROJET : Gun violence data                             #
# Etude avec le language R                               #
#                                                        #
# Auteurs : Rémi FELIN / Yasmin MOSBAH / Alexis VIGHI    #
#                                                        #
##########################################################

# Import des librairies
install.packages('lubridate')
install.packages('xts')
install.packages('zoo')
install.packages('splitstackshape')
install.packages('scales')
install.packages('gridExtra')
install.packages('stringr')
install.packages('tibble')
install.packages('ggplot2')
install.packages('readr')
install.packages('knitr')
install.packages('stats')
install.packages('graphics')
install.packages('utils')
install.packages('methods')
install.packages('corrplot')
install.packages('dplyr')

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
df = read.csv('D:/Travail/MIAGE/TP_R/Projet-R-Gun-violence-data/data/gun-violence-data.csv', header=TRUE,stringsAsFactors = FALSE, na.strings=c("NA", ""))
df1 <- subset(df, select = -c(incident_url_fields_missing,location_description,notes,incident_url,incident_characteristics,longitude,latitude,sources, source_url))
# - nombre  de blessés;
# - nombre de morts;
# - nombre de malfaiteurs;
# - age des malfaiteurs;
# - e ??tat dans lequel le fait divers a eu lieu. 


#Valeurs manquantes
apply(df1, 2, function(col)sum(is.na(col))/length(col))
#We'll delete all the columns with more than 40% of missing values
df1 <- subset(df1, select = -c(gun_stolen,gun_type,n_guns_involved,participant_name,participant_relationship))
apply(df1, 2, function(col)sum(is.na(col))/length(col))
#Basically, we should delete participant_age, because this column have 38.5% of missing data,
#But we have to keep it, maybe we will use some data science methods to input missing data.

#We delete all the missing values, we could replace it with Miss FAMD method, but as we aren't
#doing Machine Learning, it's not necessary.

df1=na.omit(df1)

#
#We'll change some data for the readibility of them
age=cSplit(df1,c("participant_age"),sep="||",direction="wide",drop=FALSE)
age$age=gsub(".*::","",age$participant_age)
age$age=as.numeric(age$age)
head(age$age)
df1$participant_age=age$age

type=cSplit(df1,"participant_status",sep="||" ,direction="wide",drop=FALSE)
type$participant_status=gsub(".*::","",type$participant_status)
head(type$participant_status)
df1$participant_status=type$participant_status

type=cSplit(df1,"participant_age_group",sep="||",direction="wide",drop=FALSE)
type$participant_age_group=gsub(".*::","",type$participant_age_group)
head(type$participant_age_group)
df1$participant_age_group=type$participant_age_group

type=cSplit(df1,c("participant_gender"),sep="||",direction="wide",drop=FALSE)
type$participant_gender=gsub(".*::","",type$participant_gender)
df1$participant_gender=type$participant_gender

type=cSplit(df1,c("participant_type"),sep="||",direction="wide",drop=FALSE)
type$participant_type=gsub(".*::","",type$participant_type)
head(type$participant_type)
df1$participant_type=type$participant_type


#Now, after some preprocessing and the reorganization of the columns, we will kept only the
#columns needed.
#State, n_killed, n_injured, participant_ages
df2 <- subset(df1, select = c(n_killed, n_injured, state, participant_age))
df2

#We delete the last NA
df2=na.omit(df2)
summary(df2$participant_age)
#We can see a crazy value, we will delete it
df2 <- df2[-which(df2$participant_age==209),]
dfnum <- subset(df2, select = c(n_killed, n_injured, participant_age))






#Question 1

bystate <- df2 %>% group_by(state) %>% summarize(n = n(), nkill = sum(n_killed), ninj = sum(n_injured)) %>% tidyr::gather(key = "type", value = "num", nkill, ninj)
ggplot(bystate, aes(x = state, y = num, fill = type)) + geom_boxplot() + theme_bw()


summary(df2$participant_age)


cor(dfnum, method = c("pearson", "kendall", "spearman"))
mcor <- cor(dfnum)
mcor

col<- colorRampPalette(c("blue", "white", "red"))(20) 
heatmap(x = mcor, col = col, symm = TRUE)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

chart.Correlation(dfnum, histogram=TRUE, pch=19)

#As we can see, there is no correlation  in our numerical subset
bystateinj <- df2 %>% group_by(state) %>% summarize(n = n(), ninj = sum(n_injured)) %>% tidyr::gather(key = "type", value = "num", ninj)
ggplot(bystateinj, aes(x = state, y = num, fill = type)) + geom_boxplot() + theme_bw()

bystatekill <- df2 %>% group_by(state) %>% summarize(n = n(), nkill = sum(n_killed))%>% tidyr::gather(key = "type", value = "num", nkill)
ggplot(bystatekill, aes(x = state, y = num, fill = type)) + geom_boxplot() + theme_bw()

bystate <- df2 %>% group_by(state) %>% summarize(n = n(), nkill = sum(n_killed), ninj = sum(n_injured)) %>% tidyr::gather(key = "type", value = "num", nkill, ninj)
ggplot(bystate, aes(x = state, y = num, fill = type)) + geom_boxplot() + theme_bw()




# Question 2

#Prenons les datas df2 et ajoutons les dates
data1 <- subset(df1, select = c(n_killed, n_injured, state, participant_age, date))

#Pour la période suivante : 01-01-2013 -> 31-12-2017
data1WithPeriod <- subset(data1, data1$date >= as.Date('2013-01-01') & data1$date <= as.Date('2017-12-31'))

#Etudions plus en détail ce jeu de données
summary(data1WithPeriod)
#a voir si on peut améliorer cette partie 

#Considérons l'échantillon suivant : df1



#Question 3
df3 <- subset(df1, select = c(n_killed, n_injured, state, participant_age, date))
df3$date <- ymd(df3$date)
str(df3$date)

#We delete the last NA
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

df3num$participant_age
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



#We can conclude that the only month which had a correlation over the year is July, maybe with the 4th.