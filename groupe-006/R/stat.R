# STATISTIQUES ET TESTS D'HYPOTHESES
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

#Prenons les datas df1 avec les colonnes utiles
data <- subset(df1, select = c(n_killed, n_injured, state, participant_age, date))

#et prenons les datas recueillis en 2018 (01-01-2018 -> 31-12-2018)
data_2018 <- subset(data, data$date >= as.Date('2018-01-01') & data$date <= as.Date('2018-03-31'))

#Pour la p�riode suivante : 01-01-2013 -> 31-12-2017
data_With_Periode <- subset(data, data$date >= as.Date('2013-01-01') & data$date <= as.Date('2017-12-31'))



#Etudions plus en d�tail ce jeu de donn�es
summary(data_With_Periode)
#a voir si on peut am�liorer cette partie 

#Consid�rons l'�chantillon suivant : data_2018
summary(data_2018)


# Si nous comparons les r�sultats, nous remarquons les choses suivantes :

# H0 NB MORT : Le nombre de morts obtenus en 2018 a permis de modifier la moyenne de mort 
# HO NB BLESSE : Le nombre de blesses obtenus en 2018 a permis de modifier la moyenne de blesses 
# HO AGE PARTICIPANT : L'age des participants releves en 2018 a permis de modifier la moyenne d'age 

#
# Pour les variables relatives aux nombres de mort, blesses et l'age des protagonistes : nous effectuerons
# un test bilat�rale afin de valider ou non H0
#

# Etablissons nos fonctions au pr�alable, elles faciliterons le traitement et ce sera plus lisible
test <- function(mu, n, xbar, sigma) {
  res = abs(xbar-mu)/(sigma/sqrt(n))
  return(res)
}

p_value <- function(alpha, ddl) {
  pval = qt(alpha, df=ddl, lower.tail = FALSE)
  return(pval)
}

sigma_value <- function(data) {
  res = var(data)
  return(res)
}

###################
#                 #
# DEBUT DES TESTS #
#                 #
###################

#data fixes

#taille de l'echantillon
n <- length(data_2018)
# On trouve notre valeur de comparaison (P-value) avec alpha = 5%
tPVAL <- p_value(0.05, n-1)

#############################################################

#
## Pour H0 NB DE MORTS
#

#d'apr�s les resultats obtenus, on a :
mu_NbMort <- mean(data_2018$n_killed)
Xbar_NbMort <- mean(data_With_Periode$n_killed)
sigma_NbMort <- sigma_value(data_2018$n_killed)

# On applique notre test avec les donn�es obtenues
t_NbMort <- test(mu_NbMort, n, Xbar_NbMort, sigma_NbMort)

# On compare et on regarde le r�sultat 
if(t_NbMort >= -tPVAL && t_NbMort <= tPVAL) {
  cat("pour alpha = 5%, on a t_nbMort (", t_NbMort ,") appartenant � l'intervalle [", 
      -tPVAL , ";", tPVAL, "]\n-> Ainsi, on ne peut pas refuser H0\n")
} else {
  cat("pour alpha = 5%, on a t_nbMort (", t_NbMort ,") n'appartenant pas � l'intervalle [", 
      -tPVAL , ";", tPVAL, "]\n-> Ainsi,on rejette H0\n")
}

#############################################################

#
## Pour H0 NB DE BLESSES
#

#d'apr�s les resultats obtenus, on a :
mu_NbBlesses <- mean(data_2018$n_injured)
Xbar_NbBlesses <- mean(data_With_Periode$n_injured)
sigma_NbBlesses <- sigma_value(data_2018$n_injured)

# On applique notre test avec les donn�es obtenues
t_NbBlesses <- test(mu_NbBlesses, n, Xbar_NbBlesses, sigma_NbBlesses)

# On compare et on regarde le r�sultat 
if(t_NbBlesses >= -tPVAL && t_NbBlesses <= tPVAL) {
  cat("pour alpha = 5%, on a t_nbMort (", t_NbBlesses ,") appartenant � l'intervalle [", 
      -tPVAL , ";", tPVAL, "]\n-> Ainsi, on ne peut pas refuser H0\n")
} else {
  cat("pour alpha = 5%, on a t_nbMort (", t_NbBlesses ,") n'appartenant pas � l'intervalle [", 
      -tPVAL , ";", tPVAL, "]\n-> Ainsi,on rejette H0\n")
}

#############################################################

#
## Pour l'age des participants
#

#d'apr�s les resultats obtenus, on a :
mu_Age <- mean(data_2018$participant_age)
Xbar_Age <- 29.63 # mean(data_With_Periode$participant_age) la cmd ne marche pas ... � voir
#tmp : on la saisie � la main
sigma_Age <- sigma_value(data_2018$participant_age)

# On applique notre test avec les donn�es obtenues
t_Age <- test(mu_Age, n, Xbar_Age, sigma_Age)

# On compare et on regarde le r�sultat 
if(t_Age >= -tPVAL && t_Age <= tPVAL) {
  cat("pour alpha = 5%, on a t_nbMort (", t_Age ,") appartenant � l'intervalle [", 
      -tPVAL , ";", tPVAL, "]\n-> Ainsi, on ne peut pas refuser H0\n")
} else {
  cat("pour alpha = 5%, on a t_nbMort (", t_Age ,") n'appartenant pas � l'intervalle [", 
      -tPVAL , ";", tPVAL, "]\n-> Ainsi,on rejette H0\n")
}

#############################################################