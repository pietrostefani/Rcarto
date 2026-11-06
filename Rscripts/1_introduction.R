# ============================================================
# RCarto — Introduction
# Dr. Elisabetta Pietrostefani
# ============================================================
# Ce script couvre :
#   - Installation et chargement des packages
#   - Types de données de base en R
#   - Data frames : accès, filtrage, exploration
#   - Le pipe |>
#   - La grammaire ggplot2
#   - La fonction utilitaire clean_cut_labels()
# ============================================================

# ---- Installer les packages (une seule fois) ---------------
# Décommentez et lancez ce bloc la première fois uniquement.
# install.packages(c(
#   "sf",                  # données vectorielles
#   "spData",              # jeux de données géographiques d'exemple
#   "terra",               # données raster
#   "stars",               # raster multi-dimensionnel
#   "tidyterra",           # terra + ggplot2
#   "ggplot2",             # visualisation
#   "patchwork",           # assembler plusieurs graphiques
#   "dplyr",               # manipulation de données
#   "classInt",            # classification choroplèthe
#   "viridis",             # palettes perceptuellement uniformes
#   "RColorBrewer",        # palettes ColorBrewer
#   "scales",              # formatage axes et légendes
#   "ggspatial",           # flèche nord et barre d'échelle
#   "rnaturalearth",       # données géographiques mondiales
#   "rnaturalearthdata",   # données Natural Earth
#   "maps",                # données cartographiques de base
#   "spdep",               # poids spatiaux et voisinage
#   "gstat",               # interpolation IDW
#   "exactextractr",       # statistiques zonales raster → polygones
#   "leaflet",             # cartes interactives
#   "htmlwidgets",         # enregistrer une carte interactive en HTML
#   "jsonlite",            # lire des données d'API (JSON)
#   "readr",               # lire des fichiers CSV
#   "mapsf"                # cartographie thématique
# ))

# ---- Charger les packages ----------------------------------
# library() charge un package pour la session en cours.
# À relancer à chaque nouvelle session RStudio.
library(sf)
library(ggplot2)
library(dplyr)

# ============================================================
# 1. TYPES DE DONNÉES DE BASE EN R
# ============================================================
# En R, tout est un objet. class() révèle le type d'un objet.

class(TRUE)                                          # logique (booléen)
class("Je suis une ville")                           # caractère (texte)
class(2021)                                          # numérique
class(as.factor(c("rural", "urbain", "périurbain"))) # facteur (catégories)

# NA représente une valeur manquante — différent de 0 ou ""
c(NA, 1, 2, NA)

# ============================================================
# 2. DATA FRAMES
# ============================================================
# Un data frame est une table à deux dimensions (lignes × colonnes).
# Chaque colonne peut avoir un type différent.
# Les objets sf sont des data frames enrichis d'une colonne géométrie.

# Accès par position [ligne, colonne]
# dat[1, ]       # première ligne entière
# dat[, 1]       # première colonne entière
# dat[6, 3]      # valeur à la 6e ligne, 3e colonne

# Accès par nom de colonne ($ est le raccourci le plus courant)
# dat$population

# Filtrage conditionnel : garde les lignes qui vérifient la condition
# dat[dat$population > 10000, ]

# Explorer la structure d'un objet
# str(mon_dataframe)     # types et premiers éléments de chaque colonne
# summary(mon_dataframe) # statistiques descriptives rapides

# ============================================================
# 3. LE PIPE |>
# ============================================================
# Le pipe passe le résultat d'une expression comme premier argument
# de la suivante. Il permet de chaîner les opérations sans créer
# d'objets intermédiaires et se lit de gauche à droite.
#
# Raccourci clavier : Ctrl + Shift + M (Windows) / Cmd + Shift + M (Mac)
#
# Note : vous verrez aussi %>% dans d'anciens scripts R.
# |> est le pipe natif de R (depuis R 4.1) — c'est celui que nous utilisons.

# Sans pipe — difficile à lire (de l'intérieur vers l'extérieur) :
# head(filter(select(mon_df, nom, prix), prix > 100), 5)

# Avec pipe — on lit étape par étape, de gauche à droite :
# mon_df |>
#   select(nom, prix) |>
#   filter(prix > 100) |>
#   head(5)

# ============================================================
# 4. LA GRAMMAIRE GGPLOT2
# ============================================================
# ggplot2 construit les graphiques couche par couche avec +
# C'est la même syntaxe pour les histogrammes, nuages de points
# ET les cartes — une seule grammaire pour tout visualiser.
#
# Structure type :
#   ggplot(data = ...) +          # 1. données de base
#     geom_*(aes(...)) +          # 2. géométrie + mapping esthétique
#     scale_*() +                 # 3. échelles (couleur, taille, axes)
#     labs(...) +                 # 4. titres et légendes
#     theme_*()                   # 5. apparence générale

# Les éléments clés :
#   ggplot()         initialise le graphique
#   geom_sf()        dessine des objets spatiaux
#   geom_histogram() dessine un histogramme
#   geom_point()     dessine un nuage de points
#   aes()            lie une variable à une propriété visuelle (fill, color, size…)
#   scale_fill_*()   contrôle la palette de couleurs du remplissage
#   labs()           ajoute titre, sous-titre, caption, noms des axes
#   theme_void()     fond blanc sans axes (idéal pour les cartes)
#   theme_minimal()  fond épuré avec axes (idéal pour les graphiques)

# Exemples commentés :

# Histogramme
# ggplot(mon_df, aes(x = prix)) +
#   geom_histogram(binwidth = 100, fill = "#2c7bb6", color = "white") +
#   labs(title = "Distribution des prix", x = "Prix (€)", y = "Fréquence") +
#   theme_minimal()

# Nuage de points
# ggplot(mon_df, aes(x = superficie, y = prix)) +
#   geom_point(alpha = 0.6) +   # alpha : transparence (0 = invisible, 1 = opaque)
#   theme_minimal()

# Carte choroplèthe
# ggplot(mon_sf) +
#   geom_sf(aes(fill = ma_variable)) +
#   scale_fill_viridis_c(name = "Légende") +
#   labs(title = "Titre de la carte", caption = "Source : ...") +
#   theme_void()

# ============================================================
# 5. FONCTION UTILITAIRE : clean_cut_labels()
# ============================================================
# Quand on classifie une variable avec cut(), R génère des labels
# de type "(4.5,7.1]" — notation mathématique des intervalles.
#   ( ou ) = borne exclue (ouverte)
#   [ ou ] = borne incluse (fermée)
#
# Cette fonction remplace ces labels par "4.5 – 7.1", plus lisible
# sur une légende de carte. Copiez-la dans tous vos scripts.

clean_cut_labels <- function(x) {
  levels(x) <- gsub("\\(|\\[|\\]|\\)", "", levels(x)) |>
    gsub(",", " – ", x = _)
  x
}

# Utilisation type (avec classIntervals + cut) :
# breaks  <- classIntervals(mon_vecteur, n = 5, style = "jenks")
# ma_variable_classee <- cut(mon_vecteur,
#                             breaks         = breaks$brks,
#                             include.lowest = TRUE,
#                             dig.lab        = 4,
#                             ordered_result = TRUE) |>
#   clean_cut_labels()
