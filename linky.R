#!/usr/bin/env Rscript
# Author : Dekloo (dekloo.net)
# Contrib : superDocteur
# 

#1) récupérer le zip fourni par ENEDIS (compteurt linky)
#2) renommer le fichier mes-puissances-atteintes-30min-XXXXXXXXXXXXXXXXXX.csv en mes-puissances-atteintes-30min.csv

#******************************************************************
#*** PROGRAMME EN LANGAGE R avec RSTUDIO **************************
#******************************************************************

# Charger les librairies nécessaires
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Lire le fichier CSV brut en ignorant les 4 premières lignes
# Renommer les colonnes pour donner des noms explicites

data_brut <- read.csv2("mes-puissances-atteintes-30min.csv",
                       skip = 2,
                       col.names = c("heure", "conso", "nature"),
                       encoding = "latin1")

# Supprimer les lignes contenant uniquement des données vides (comme ";;")
data_brut <- data_brut %>% filter(heure != "" | !is.na(conso))

# Identifier les lignes qui contiennent les dates (lignes où 'conso' est vide)
data_brut <- data_brut %>% mutate(date = ifelse(is.na(conso), heure, NA))

# Remplir les dates manquantes avec la dernière date observée
data_brut <- data_brut %>% fill(date, .direction="down")

# Supprimer les lignes où 'conso' est vide (car ces lignes contiennent les dates uniquement)
data_brut <- data_brut %>% filter(!is.na(conso) & conso != "")

# Supprimer la colonne 'nature' (car elle contient "Réelle" qui est inutile)
datas <- data_brut %>% select(-nature)

# Conversion de la colonne 'date' qui est au format JJ/MM/AAAA
# Convertir la colonne 'heure' en heure numérique
# Extraire le mois à partir de la date
# Extraire l'annee à partir de la date
# création d'un index anneemois
# toutes les taches ci dessus sont réalisables au sein d'un même dplyr::mutate()
# en dplyr :
datas <- datas %>% mutate(date  = as.Date(date, format = '%d/%m/%Y'),
                        heure = hour(hms(heure)),
                        mois  = month(date),
                        annee = year(date),
                        anneemois = sprintf("%04d%02d", annee, mois)
)

# Afficher les premières lignes du fichier pour vérifier la présence des colonnes
#head(datas) #si besoin de visualiser les données

# Calculer la moyenne de la consommation par mois et par heure
moyenne_conso <- datas %>%
  group_by(anneemois, heure) %>%
  summarise(moy_conso = mean(conso, na.rm = TRUE))

# Note : Là on touche à ggplot() donc les gouts et les couleurs... et tout le
#        monde sait que ça ne se discute pas :)

# Créer une heatmap avec axes intervertis (mois sur x et heure sur y)
ggplot(moyenne_conso, aes(x = anneemois, y = heure, fill = moy_conso)) +
  geom_tile() +
  scale_y_continuous(breaks = 0:23) +
  scale_x_discrete() +
  scale_fill_gradient(low = "blue", high = "red", name = "Consommation (kWh)") +
  labs(title = "Moyenne de la Consommation par Mois et par Heure",
       x = "Mois",
       y = "Heure de la Journée") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
