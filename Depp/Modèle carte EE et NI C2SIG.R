####### Script de cartographie pour modèles EE et NI - R serveur #######

# Si souci avec les accents réouvrir avec un encodage UTF-8

#### Chargement des packages, des polices et des réglages ####

library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)
library(BAMMtools)
library(xlsx)
library(showtext)
library(Rcpp)
font_paths("/opt/cartographie/Documentation/Publications - consignes et modèles de cartes/RERS/Cartes R/MARIANNE V2")
font_add(family = "Marianne", regular = "Marianne-Regular.otf")
showtext_auto()
options(bitmapType='cairo')


#### Import des couches ####

# Données à l'académie avec habillage région
CPCP <- st_read("/opt/cartographie/Couches_SIG/Zonages Admin/Académies/academies_FRMetroDrom_zoomParis.shp", quiet=T)
CHAB <- st_read("/opt/cartographie/Couches_SIG/Zonages Admin/Régions/regions_FrMDrom.shp", quiet=T)

# Données au département avec habillage académie
CPCP <- st_read("/opt/cartographie/Couches_SIG/Zonages Admin/Départements/dep_FRMetroDrom_zoomPCouronne.shp", quiet=T)
CHAB <- st_read("/opt/cartographie/Couches_SIG/Zonages Admin/Académies/NoZoom/academies_FRMetroDrom.shp", quiet=T)

# Données au bassin de vie avec habillage académie
CPCP <- st_read("/opt/cartographie/Couches_SIG/Zonages Etude/BV/BV_FRMetroDrom.shp", quiet=T)
CHAB <- st_read("/opt/cartographie/Couches_SIG/Zonages Admin/Académies/NoZoom/academies_FRMetroDrom.shp", quiet=T)


#### Eléments complémentaires ####

# Systématique
LIPA <- st_read("/opt/cartographie/Documentation/Publications - consignes et modèles de cartes/RERS/Cartes R/ligne_Paris_R.shp", quiet=T)
LIBL <- st_read("/opt/cartographie/Documentation/Publications - consignes et modèles de cartes/RERS/Cartes R/ligne_R.shp", quiet=T)

# Optionnel dans le cas où les données de Mayotte soient manquantes
DONMQ <- st_read("/opt/cartographie/Documentation/Publications - consignes et modèles de cartes/RERS/Cartes R/polygon_R6.shp", quiet=T)
MAYN <- st_read("/opt/cartographie/Documentation/Publications - consignes et modèles de cartes/RERS/Cartes R/mayotte.shp", quiet=T)


#### Import et concaténation des données ####

# Import des données
DON <- read.csv2("/home/depp/###/fichiercible.csv") # Si fichier csv, renseigner la destination
DON <- read.xlsx2("/home/depp/###/fichiercible.xlsx", 1) # Si fichier xlsx, renseigner la destination et le numéro de l'onglet

# Concaténation des données avec la couche principale
CPCPDON <- left_join(CPCP, DON, by = c("CODE_ACA" = "Code.académie")) # Si données académiques, vérifier que dans les données 
                                                                      # la variable des codes académiques est bien Code.académie
                                                                      # Penser à vérifier que les codes à un chiffre commencent
                                                                      # bien par 0, exemple : "01"

CPCPDON <- left_join(CPCP, DON, by = c("INSEE_DEP" = "Code.département")) # Si données départementales, vérifier que dans les données 
                                                                          # la variable des codes départementaux est bien Code.département
                                                                          # Penser à vérifier que les codes à un chiffre commencent
                                                                          # bien par 0, exemple : "01"

CPCPDON$VAR <- as.numeric(CPCPDON$NomVar) # Remplacer NomVar par le nom de la variable de données territoriales
CPCPDON$EFF <- as.numeric(CPCPDON$NomVar) # Remplacer NomVar par le nom de la variable des effectifs (optionnel)


#### Tests statistiques et choix des classes ####

# Distribution des valeurs à vérifier
hist(CPCPDON$VAR)
hist(CPCPDON$VAR,  breaks = 15) # Si le premier histogramme n'est pas assez détaillé, utiliser ce modèle et augmenter la valeur au besoin

# Choix de la méthode de découpage des classes
classes = quantile(CPCPDON$VAR,  probs = seq(0, 1, by = 1/5), na.rm = T ) # Si quantiles, nombre de classes = n, remplacer 5 par n
classes = getJenksBreaks(CPCPDON$VAR, 6, subset = NULL) # Si ruptures naturelles, nombre de classes = n, remplacer 6 par n+1
classes = c(A, B, C, D, E, G) # Si seuils manuels, remplacer A, ..., G par valeurs entières à part cas particuliers (cf. consignes cartographiques)

# Mise en forme des classes pour utiliser des bornes entières (optionnel mais fortement conseillé à part cas particuliers)
classes=c(floor(first(classes)), round(classes[-c(1, 6)]), ceiling(last(classes))) # nombre de classes = n, remplacer 6 par n + 1


#### Contenu de la carte ####

# Textes
TitFig="Titre de la figure" # Renseigner le titre de la figure
MoyNat="France métropolitaine\n+ DROM : X,X %" # Renseigner la moyenne nationale

# Couleurs des territoires
Col=c("#FCE5D6", "#F7CAAC", "#EF9F6E", "#ED8248", "#DE6C30") # Orange avec cinq classes
Col=c("#D4E6F5", "#B1D9F2", "#7CB0E0", "#3476B0", "#0F4E7D") # Bleu avec cinq classes
Col=c("#FCE5D6", "#F7CAAC", "#EF9F6E", "#ED8248") # Orange avec quatre classes
Col=c("#D4E6F5", "#B1D9F2", "#7CB0E0", "#3476B0") # Bleu avec quatre classes
Col=c("#5C93C1", "#87BADC", "#C6E7FB", "#F1CEE3", "#E3A6CB") # Rose/Bleu avec trois classes négatives et deux positives
Col=c("#87BADC", "#C6E7FB", "#F1CEE3", "#E3A6CB", "#D77BB0") # Rose/Bleu avec deux classes négatives et trois positives

# Couleurs supplémentaires dans le fichier excel situé dans le dossier M:\prj-depp-cartographie\Documentation\Consignes aux auteurs de cartes\Couleurs
palette_explorer() # A exécuter si vous voulez choisir d'autres couleurs personnalisées

# Bornes des effectifs le cas échéant
LegEFF=c(X, Y, Z) # Placer les valeurs dans l'ordre décroissant


#### Modèle carte ####

Carte <- tm_shape(CPCPDON) +
        tm_fill(col = "VAR",
                palette = Col, style="fixed", breaks=classes, 
                legend.format = c(text.separator = " "), # Pour définir manuellement les bornes 
                legend.is.portrait=F, title = "",        # remplacer legend.format par :
                colorNA = NULL, showNA = F) +            # labels=(A-B, B-C, C-D...)
        tm_borders("white", lwd = 0.25) +
        tm_layout(main.title = TitFig,
                  main.title.position = c("left", "top"),
                  main.title.size = 0.8,
                  fontfamily = "Marianne",
                  frame = F,
                  legend.position = c(0.5, 0),
                  legend.hist.height = -1,
                  legend.text.size = 0.58,
                  legend.title.size = 0.58,
                  inner.margins = c(0.075, 0, 0.10, 0)) +
  tm_shape(CHAB) +
        tm_borders("black", lwd = 0.8) +
        tm_credits(MoyNat, size = 0.49, fontfamily = "Marianne",
                   position = c("LEFT", "BOTTOM")) +
        tm_credits("en %", size = 0.58, fontfamily = "Marianne", 
                   position = c(0.635, 0.05)) +
    tm_shape(LIPA) +
      tm_lines(lwd = 0.25, lty = 2) +
    tm_shape(LIBL) +
      tm_lines(col = "white") +
      tm_text(text = "NOM", size = 0.4, along.lines = T) +
    tm_shape(DONMQ) +                                                         # Partie à supprimer si les données de
      tm_borders("black", lwd = 0.2) +                                        # Mayotte sont disponibles
      tm_credits("Données manquantes", size = 0.58, fontfamily = "Marianne",  # Supprimer également le + à la fin 
               position = c(0.58, 0.08)) +                                    # de la ligne précédente
    tm_shape(MAYN) +                                                          #
     tm_borders("black", lwd = 0.2) +                                         #
    tm_shape(CPCPDON) +                                                                        # A supprimer si seules des données
     tm_symbols(shape = 19, size = "EFF", col = "#5CB032", scale = 2.2, sizes.legend =LegEFF , # territoriales sont représentées
               legend.size.is.portrait = T, title.size = "Effectifs d'élèves") +               # Supprimer également le + à la fin 
     tm_legend(position = c(0.65, 0.82)) +                                                     # de la ligne précédente
  
  
#### Export de la carte comme pdf vectorisé ####

tmap_save(Carte, "/home/depp/XXX/NomFichier.pdf", width = 131.4, units = "mm") # Renseigner l'adresse de destination 
                                                                             # du fichier et remplacer NomFichier
                                                                             # par le nom du Fichier