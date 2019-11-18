##########################################################
#                                                        #
# PROJET : Gun violence data                             #
# Etude avec le language R                               #
#                                                        #
# Auteurs : Rémi FELIN / Yasmin MOSBAH / Alexis VIGHI    #
#                                                        #
##########################################################

#set path
path = "D:/Travail/MIAGE/TP_R/data/gun-violence-data.csv" #TODO : entrer le chemin relatif vers votre jeu de données
data = read.csv(path, header=T, sep = ",")

#on affiche les données
head(data)