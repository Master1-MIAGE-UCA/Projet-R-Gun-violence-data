install.packages('lubridate')
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
library(purr)
library(splitstackshape)

library(lubridate)
df = read.csv('/Users/ALEXIS/Downloads/gun-violence-data_01-2013_03-2018.csv', header=TRUE,stringsAsFactors = FALSE, na.strings=c("NA", ""))
df1 <- subset(df, select = -c(incident_url_fields_missing,location_description,notes,incident_url,incident_characteristics,longitude,latitude,sources, source_url))
# — nombre  de blessés;
# — nombre de morts;
# — nombre de malfaiteurs;
# — age des malfaiteurs;
# — e ́tat dans lequel le fait divers a eu lieu. 


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
#State, n_killed, n_injured, participant_age
df2 <- subset(df1, select = c(n_killed, n_injured, state, participant_age))
df2






