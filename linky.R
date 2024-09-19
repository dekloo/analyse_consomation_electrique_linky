#! Rcmd
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
data_brut <- read.csv("mes-puissances-atteintes-30min.csv", sep = ";", skip = 4, header = FALSE)

# Renommer les colonnes pour donner des noms explicites
colnames(data_brut) <- c("heure", "conso", "nature")

# Supprimer les lignes contenant uniquement des données vides (comme ";;")
data_brut <- data_brut[!(data_brut$heure == "" & data_brut$conso == ""), ]

# Identifier les lignes qui contiennent les dates (lignes où 'conso' est vide)
data_brut$date <- ifelse(is.na(data_brut$conso) | data_brut$conso == "", data_brut$heure, NA)

# Remplir les dates manquantes avec la dernière date observée
data_brut <- fill(data_brut, date, .direction = "down")

# Supprimer les lignes où 'conso' est vide (car ces lignes contiennent les dates uniquement)
data_brut <- data_brut %>% filter(!is.na(conso) & conso != "")

# Supprimer la colonne 'nature' (car elle contient "Réelle" qui est inutile)
data_brut <- data_brut %>% select(date, heure, conso)

# Sauvegarder le fichier nettoyé et restructuré
write.csv2(data_brut, "fichier_restructure.csv", row.names = FALSE, quote = FALSE)

# Afficher les premières lignes pour vérifier
# head(data_brut) #si besoin de visualiser les données

# Lire le fichier CSV, en ajoutant des noms de colonnes
data <- read.csv("fichier_restructure.csv", sep = ";")  

# Conversion de la colonne 'date' qui est au format JJ/MM/AAAA
data$date <- as.Date(data$date, format="%d/%m/%Y")  # Format JJ/MM/AAAA

# Convertir la colonne 'heure' en heure numérique
data$heure <- as.numeric(substr(data$heure, 1, 2))  # Extraire les heures (de 0 à 23)

# Extraire le mois à partir de la date
data$mois <- format(month(data$date), nsmall = 2) # month(data$date)

# Extraire l'annee à partir de la date
data$annee <- year(data$date)

# création d'un index anneemois
data$anneemois <- paste0(year(data$date),sprintf("%02d", month(data$date)),sep='')

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



