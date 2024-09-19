#!/usr/bin/env Rscript
# Note : j'ai rajouté un shebang, ça mange pas de pain même si ça ne sert à rien
#        sous Windows ou si on le lance via Rstudio
#        Je trouve tout de même que ça aide à voir d'un rapide coup d'oeil ce 
#        qui se trouve dans le fichier surtout si l'extension n'est pas parlante.

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
# CODE ORIGINAL : data_brut <- read.csv("mes-puissances-atteintes-30min.csv", sep = ";", skip = 4, header = FALSE)
# Renommer les colonnes pour donner des noms explicites
# CODE ORIGINAL : colnames(data_brut) <- c("heure", "conso", "nature")
# Note : read.csv() est décliné en read.csv2() qui accepte directement le format
#        csv le plus courant en france, celui séparé par des ";"
# Note : les fonctions read.*() acceptent un paramètre col.names= qui 
#        attend un vecteur de noms de colonnes
#        par ailleurs chez moi, la colonne `nature`contient du texte qui n'est
#        pas au bon encodage. Il faudrait donc préciser encoding = "latin1"
#        Il est donc possible d'écrire : 

data_brut <- read.csv2("mes-puissances-atteintes-30min.csv",
                       skip = 2,
                       col.names = c("heure", "conso", "nature"),
                       encoding = "latin1")

# *** A partie de là, en théorie, il faut choisir : soit un travaille entièrement en R de base ou en dplyr.
# Supprimer les lignes contenant uniquement des données vides (comme ";;")
# CODE ORIGINAL : data_brut <- data_brut[!(data_brut$heure == "" & data_brut$conso == ""), ]
# en dplyr :
# data_brut <- data_brut %>% filter(! (heure == "" & conso == "")) # perso, je trouve ça plus clair
# voire en connaissant ses règles de substritution d'opérations booléennes :
# data_brut <- data_brut %>% filter(heure != "" | conso != "")) # encore plus clair
# c'est à dire garder les lignes où heure n'est pas vide ou conso n'est pas vide
# ATTENTION : $conso n'est pas un vecteur de chaines de caractères mais d'entiers
#             le code n'a donc pas trop de sens. 
#             Pour être cohérent, il faut écrire :
data_brut <- data_brut %>% filter(heure != "" | !is.na(conso))

# Identifier les lignes qui contiennent les dates (lignes où 'conso' est vide)
# CODE ORIGINAL : data_brut$date <- ifelse(is.na(data_brut$conso) | data_brut$conso == "", data_brut$heure, NA)
# en dplyr
data_brut <- data_brut %>% mutate(date = ifelse(is.na(conso), heure, NA))

# Remplir les dates manquantes avec la dernière date observée
# CODE ORIGINAL : data_brut <- fill(data_brut, date, .direction = "down")
# en utilisant le pipe %>% c'est plus homogène avec le reste du code dplyr
data_brut <- data_brut %>% fill(date, .direction="down")

# Supprimer les lignes où 'conso' est vide (car ces lignes contiennent les dates uniquement)
# CODE ORIGINAL : data_brut <- data_brut %>% filter(!is.na(conso) & conso != "")
# Note : idem que ci-dessus, !="" n'a pas de sens
data_brut <- data_brut %>% filter(!is.na(conso) & conso != "")

# Supprimer la colonne 'nature' (car elle contient "Réelle" qui est inutile)
# CODE ORIGINAL : data_brut <- data_brut %>% select(date, heure, conso)
# Note : select() accepte le signe "-" pour retirer une colonne :
data_brut <- data_brut %>% select(-nature)

# Note : une autre solution, vu qu'on n'utilise jamais cette colonne est de ne
#        pas la charger en renseignant le paramètre colClasses= de read.csv2()
#        il faut mettre "NULL" en regard de la colonne en question
#
#data_brut <- read.csv2("mes-puissances-atteintes-30min.csv",
#                       skip = 4,
#                       colClasses = c("character","integer","NULL"),
#                       col.names = c("heure","conso","nature"),
#                       header = FALSE,
#                       encoding = "latin1")

# Sauvegarder le fichier nettoyé et restructuré
write.csv2(data_brut, "fichier_restructure.csv", row.names = FALSE, quote = FALSE)
# Note : Perso, je ne resauvegarde jamais de données dans un format 
#        "restructuré" sauf si c'est la finalité du script ou que la suite du
#        traitement est externe à R. C'est un trop grand risque de traiter des
#        données plus à jour...

# Afficher les premières lignes pour vérifier
# head(data_brut) #si besoin de visualiser les données

# Lire le fichier CSV, en ajoutant des noms de colonnes
data <- read.csv("fichier_restructure.csv", sep = ";")  
# Note : ATTENTION la fonction data() existe, il peut y avoir des effets de bord
#        si on a besoin d'appeler la fonction data(), il faudra alors passer par
#        utils::data()

# Conversion de la colonne 'date' qui est au format JJ/MM/AAAA
# data$date <- as.Date(data$date, format="%d/%m/%Y")  # Format JJ/MM/AAAA

# Convertir la colonne 'heure' en heure numérique
# CODE ORIGINAL : data$heure <- as.numeric(substr(data$heure, 1, 2))  # Extraire les heures (de 0 à 23)
# Note : la résultion du fichier est la 1/2 heure, là on perd la granularité...
#        vu que lubridate est chargé pourquoi ne pas faire :
# data$heure <- hms(data$heure)
#        qui permet de garder la granularité (et une structure d'heure)
#        et si on ne veut que l'heure :
# data$heure <- hour(hms(data$heure))

# Extraire le mois à partir de la date
# CODE ORIGINAL : data$mois <- format(month(data$date), nsmall = 2) # month(data$date)
# Note : quel est l'intérêt du nsmall ? c'est un paramètre qui définit le nombre
#        de zéros après la virgule... Je me contenterais d'un :
# data$mois <- month(data$date)

# Extraire l'annee à partir de la date
# data$annee <- year(data$date)

# création d'un index anneemois
# CODE ORIGINAL : data$anneemois <- paste0(year(data$date),sprintf("%02d", month(data$date)),sep='')
# Note : C'est alambiqué d'utiliser paste0() ET sprintf(), autant n'utiliser que
#        sprintf()...
#        En plus dans les lignes au dessus on a déjà fait les conversions, 
#        autant les réutiliser (et c'est plus lisible)
# data$anneemois <- sprintf("%04d%02d", data$annee,data$mois)

# toutes les taches ci dessus sont réalisables au sein d'un même dplyr::mutate()
# en dplyr :
data <- data %>% mutate(date  = as.Date(date, format = '%d/%m/%Y'),
                        heure = hour(hms(heure)),
                        mois  = month(date),
                        annee = year(date),
                        anneemois = sprintf("%04d%02d", annee, mois)
)

# Afficher les premières lignes du fichier pour vérifier la présence des colonnes
#head(data) #si besoin de visualiser les données

# Calculer la moyenne de la consommation par mois et par heure
moyenne_conso <- data %>%
  group_by(anneemois, heure) %>%
  summarise(moy_conso = mean(conso, na.rm = TRUE))

# Créer une heatmap avec axes intervertis (mois sur x et heure sur y)
ggplot(moyenne_conso, aes(x = anneemois, y = heure, fill = moy_conso)) +
  geom_tile(color = "white") +
  scale_y_continuous(breaks = seq(min(moyenne_conso$heure), max(moyenne_conso$heure), by = 1)) +
  
  scale_fill_gradient(low = "blue", high = "red", name = "Consommation (kWh)") +
  labs(title = "Moyenne de la Consommation par Mois et par Heure",
       x = "Mois",
       y = "Heure de la Journée") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
