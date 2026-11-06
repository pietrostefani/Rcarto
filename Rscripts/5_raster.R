# ============================================================
# RCarto — Données Raster
# Dr. Elisabetta Pietrostefani
# ============================================================
# Ce script couvre :
#   - Vecteur vs Raster : quand utiliser quoi ?
#   - Le concept de raster avec le dataset volcano
#   - Package stars : images Landsat multibandes
#   - Package terra : modèles numériques de terrain (MNT)
#   - SCR et rasters : déclarer vs reprojeter
# ============================================================

library(sf)
library(ggplot2)
library(dplyr)
library(stars)    # données raster spatio-temporelles multibandes
library(terra)    # opérations raster rapides (MNT, reclassification)

# Rappel des différences vecteur / raster :
#   Vecteur : points, lignes, polygones — entités discrètes avec frontières nettes
#             formats : .shp, .gpkg, .geojson | package : sf
#   Raster  : grille régulière de pixels — phénomènes continus (altitude, T°…)
#             formats : .tif, .img, .nc   | packages : terra, stars

# ============================================================
# 1. LE CONCEPT DE RASTER : DATASET VOLCANO
# ============================================================
# volcano est une matrice 87 × 61 intégrée à R de base.
# Chaque cellule = altitude en mètres du volcan Maungawhau (Auckland, NZ).
# C'est exactement la structure d'un raster géographique —
# sauf qu'ici il n'a pas de SCR associé.

cat("Classe :", class(volcano), "\n")
cat("Dimensions :", nrow(volcano), "lignes ×", ncol(volcano), "colonnes\n")
print(volcano[1:3, 1:5])   # aperçu des premières valeurs

# Trois propriétés définissent un raster géographique :
#   Emprise (extent)  : la zone couverte (xmin, xmax, ymin, ymax)
#   Résolution        : la taille d'un pixel (ex. 30m × 30m)
#   SCR               : le système de coordonnées de référence

# Pour visualiser avec ggplot2, on convertit la matrice en data frame :
# expand.grid() crée toutes les combinaisons de x et y (une ligne par pixel)
# as.vector(t(volcano)) "aplatit" la matrice en vecteur (t = transposition)
# coord_equal() est OBLIGATOIRE pour les rasters : garde le ratio 1:1

volcano_df <- expand.grid(x = 1:ncol(volcano), y = 1:nrow(volcano))
volcano_df$altitude <- as.vector(t(volcano))

ggplot(volcano_df, aes(x = x, y = y, fill = altitude)) +
  geom_raster() +
  scale_fill_gradientn(
    colors = hcl.colors(50, "Terrain"),   # palette continue en niveaux de terrain
    name   = "Altitude (m)"
  ) +
  coord_equal() +
  labs(title    = "MNT — Volcan Maungawhau, Auckland (NZ)",
       subtitle = paste0("Grille de ", nrow(volcano), " × ", ncol(volcano),
                         " cellules | Altitude : ",
                         min(volcano), "–", max(volcano), " m"),
       caption  = "Source : dataset volcano (base R) | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 9))

# ============================================================
# 2. PACKAGE stars : IMAGES SATELLITAIRES MULTIBANDES
# ============================================================
# stars est conçu pour les données spatio-temporelles multidimensionnelles :
# images satellitaires multi-bandes, séries temporelles de rasters, données climatiques.
#
# L'image fournie est une image Landsat 7 ETM+ d'Olinda (Brésil) avec 6 bandes :
#   B1 = Bleu     B2 = Vert     B3 = Rouge
#   B4 = Proche Infrarouge (PIR) — végétation réfléchit fortement ici
#   B5 = SWIR1    B6 = SWIR2

L7 <- read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))

L7             # structure complète de l'objet (dimensions, résolution, SCR)
dim(L7)        # lignes × colonnes × bandes
st_crs(L7)$srid
st_bbox(L7)    # emprise géographique (xmin, ymin, xmax, ymax)

# Visualiser toutes les bandes avec facet_wrap()
# labeller permet de nommer chaque facette (B1 → "B1 — Bleu" etc.)
ggplot() +
  geom_stars(data = L7) +
  facet_wrap(
    ~band,
    labeller = labeller(band = c("1" = "B1 — Bleu",   "2" = "B2 — Vert",
                                  "3" = "B3 — Rouge",  "4" = "B4 — PIR",
                                  "5" = "B5 — SWIR1",  "6" = "B6 — SWIR2"))
  ) +
  scale_fill_viridis_c(option = "inferno", name = "Réflectance", na.value = "white") +
  coord_equal() +
  labs(title   = "Landsat 7 ETM+ — Olinda, Brésil",
       caption = "Source : package stars | Cours R-Carto") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 8))

# Extraire une seule bande : L7[,,,numero_bande]
# La bande 4 (PIR) : végétation = valeurs élevées (végétation dense réfléchit le PIR)
L7_b4 <- L7[,,,4]

ggplot() +
  geom_stars(data = L7_b4) +
  scale_fill_viridis_c(option = "viridis", name = "PIR", na.value = "white") +
  coord_equal() +
  labs(title    = "Bande 4 — Proche Infrarouge (PIR)",
       subtitle = "Valeurs élevées = végétation dense",
       caption  = "Source : Landsat 7 | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5))

# ============================================================
# 3. PACKAGE terra : MODÈLES NUMÉRIQUES DE TERRAIN
# ============================================================
# terra est optimisé pour les opérations rapides sur rasters simples :
# MNT, reclassification, calcul de pente, algèbre raster.
# Il remplace l'ancien package raster (déprécié depuis 2023).
#
# Pour les exemples suivants, installez spDataLarge :
# install.packages("spDataLarge", repos = "https://geocompr.r-universe.dev")

library(spDataLarge)
library(tidyterra)   # permet d'utiliser geom_spatraster() dans ggplot2

# Charger un MNT SRTM — Parc National de Zion, Utah (USA)
raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")
srtm <- rast(raster_filepath)

srtm                   # résumé complet (SCR, résolution, emprise, valeurs)
crs(srtm)              # SCR (noter la syntaxe terra vs sf)
res(srtm)              # résolution en degrés
ext(srtm)              # emprise (xmin, xmax, ymin, ymax)
nlyr(srtm)             # nombre de couches (1 pour un MNT simple)
global(srtm, "range")  # min / max des valeurs d'altitude

# geom_spatraster() de tidyterra intègre les objets terra dans ggplot2
ggplot() +
  geom_spatraster(data = srtm) +
  scale_fill_gradientn(
    colors   = hcl.colors(50, "Terrain"),
    name     = "Altitude (m)",
    na.value = "white"
  ) +
  labs(title   = "MNT SRTM — Parc National de Zion (Utah)",
       caption = "Source : spDataLarge | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Raster multi-couches (ex. image Landsat multibande avec terra)
multi_rast_file <- system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast      <- rast(multi_rast_file)

multi_rast          # 4 couches
nlyr(multi_rast)    # nombre de couches
names(multi_rast)   # noms des couches

# ============================================================
# 4. SCR ET RASTERS
# ============================================================
# Même distinction qu'avec sf : déclarer ≠ reprojeter.
# Mais attention : reprojeter un raster est plus coûteux qu'un vecteur
# car il faut réinterpoler les valeurs dans une nouvelle grille.
# → Travaillez dans le SCR natif le plus longtemps possible.

# Lire le SCR
crs(srtm)

# Déclarer un SCR manquant (ne modifie PAS les valeurs)
crs(srtm) <- "EPSG:4326"

# Reprojeter (réinterpolation des valeurs — plus lent)
srtm_lambert <- project(srtm, "EPSG:2154")

# Différence avec sf :
#   sf  : st_set_crs() pour déclarer | st_transform() pour reprojeter
#   terra : crs() <-  pour déclarer  | project()       pour reprojeter

# ============================================================
# EXERCICE 1 — Explorer le raster volcano
# ============================================================
# En utilisant volcano_df créé plus haut :
#
# 1. Calculez l'altitude moyenne, minimale et maximale.
#    Indice : summary(volcano_df$altitude)
#
# 2. Créez une carte qui ne montre que les zones > 160 m.
#    Utilisez if_else() pour remplacer les valeurs ≤ 160 par NA.
#    Indice : if_else(condition, valeur_si_vrai, NA_real_)
#
# 3. Changez la palette — essayez "YlOrRd" ou "RdBu"
#    dans scale_fill_gradientn()

summary(volcano_df$altitude)   # étape 1

volcano_hautes <- volcano_df |>
  mutate(altitude_filtre = if_else(altitude > ___, altitude, NA_real_))  # étape 2

ggplot(volcano_hautes, aes(x = x, y = y, fill = altitude_filtre)) +
  geom_raster() +
  scale_fill_gradientn(colors = hcl.colors(20, "___"), na.value = "grey90") +  # étape 3
  coord_equal() +
  theme_void()

# ============================================================
# EXERCICE 2 — NDVI : indice de végétation (avancé)
# ============================================================
# Le NDVI mesure la densité de végétation à partir de Landsat :
#
#   NDVI = (PIR - Rouge) / (PIR + Rouge) = (B4 - B3) / (B4 + B3)
#
# Valeurs : -1 (eau, sol nu) → +1 (végétation dense)
#
# 1. Extrayez B3 (rouge) et B4 (PIR) de l'image L7
# 2. Calculez le NDVI
# 3. Cartographiez avec une palette divergente "RdYlGn"
#    et des limites fixées entre -1 et 1

L7 <- read_stars(system.file("tif/L7_ETMs.tif", package = "stars"))

b3_rouge <- L7[,,,___]   # bande rouge
b4_pir   <- L7[,,,___]   # bande proche infrarouge

ndvi <- (___ - ___) / (___ + ___)   # formule du NDVI

ggplot() +
  geom_stars(data = ndvi) +
  scale_fill_distiller(
    palette   = "RdYlGn",
    direction = 1,
    name      = "NDVI",
    limits    = c(-1, 1),   # fixer les limites pour centrer la palette
    na.value  = "grey80"
  ) +
  coord_equal() +
  labs(title    = "NDVI — Indice de végétation",
       subtitle = "Vert = végétation | Rouge = sol nu / eau",
       caption  = "Landsat 7 ETM+ — Olinda, Brésil") +
  theme_void()
