# =============================================================================
# RCarto — Raster : relief du Liban
# Dr. Elisabetta Pietrostefani
#
# Un raster est une grille régulière de cellules (pixels), chacune portant
# une seule valeur, sans tableau d'attributs. Une grande partie du travail
# consiste à faire le pont entre cette grille de nombres et les données
# administratives ou d'enquête qui nous intéressent.
#
#   Partie 1 — Le Liban (guidé)  : chaque opération pas à pas
#   Partie 2 — La Corse (à vous) : reproduisez la démarche
#                                  (corrigé : 6b_raster_Corse.R)
#   Partie 3 — Comparer          : les deux territoires côte à côte
#
# Opérations couvertes :
#   rast() charger | project() reprojeter | crop() + mask() recadrer/masquer
#   aggregate() changer la résolution | terrain() pente | classify() reclassifier
#   extract() valeurs en des points | exact_extract() statistiques par zone
#
# Avant de commencer : ouvrez le projet (fichier .Rproj). Tous les chemins
# commencent par "data/...". Pour vérifier où vous êtes : getwd()
#
# Exécutez le script section par section (Ctrl/Cmd + Entrée), pas d'un seul
# coup : la partie 2 est à compléter. Le menu "Outline" de RStudio (en haut à
# droite de l'éditeur) permet de naviguer entre les sections.
# =============================================================================


# ---- 0. Packages -------------------------------------------------------------

# install.packages(c("terra", "tidyterra", "sf", "ggplot2", "dplyr",
#                    "exactextractr"))   # si besoin, une seule fois

library(terra)          # manipulation des rasters
library(tidyterra)      # geom_spatraster() et palettes pour ggplot2
library(sf)             # données vectorielles
library(ggplot2)        # visualisation
library(dplyr)          # manipulation de données
library(exactextractr)  # statistiques zonales

terraOptions(progress = 0)   # masquer les barres de progression de terra

# Remarque : terra remplace l'ancien package raster (déprécié depuis 2023).
# raster() -> rast(), extent() -> ext()


# =============================================================================
# PARTIE 1 — LE LIBAN (GUIDÉ)
# =============================================================================

# ---- 1.1 Charger et explorer le MNT ------------------------------------------

# Un MNT (Modèle Numérique de Terrain) est un raster dont chaque cellule
# contient une altitude.
dem_lb <- rast("data/Liban/LBN_elevation_w_bathymetry.tif")
dem_lb

# Lisez cet affichage : pas de lignes de tableau, mais
#   - les dimensions  : nombre de lignes et de colonnes de cellules
#   - la résolution   : taille d'une cellule (0,0025 degré, environ 250 m)
#   - l'emprise       : le rectangle couvert
#   - le SCR          : ici WGS 84, en degrés

plot(dem_lb)

# Les valeurs négatives correspondent à la bathymétrie (profondeur sous le
# niveau de la mer) : c'est pourquoi la Méditerranée apparaît.


# ---- 1.2 Le contour du pays et son SCR ---------------------------------------

liban <- read_sf("data/Liban/LBN_adm1.shp")   # les gouvernorats (mohafazat)

st_crs(liban)$input
crs(dem_lb, describe = TRUE)$name

# Les deux SCR sont différents : raster en degrés (WGS 84), vecteur en mètres
# (Lambert syrien, EPSG:22770). Il faut les aligner.


# ---- 1.3 Reprojeter le raster ------------------------------------------------

# On reprojette le raster dans le SCR du vecteur, qui est en MÈTRES
# (utile pour calculer la pente plus loin).
# vect() convertit un objet sf en vecteur terra (SpatVector).
dem_lb <- project(dem_lb, crs(vect(liban)))
crs(dem_lb, describe = TRUE)$name
res(dem_lb)   # résolution, désormais en mètres

# ATTENTION : reprojeter un vecteur déplace simplement des coordonnées.
# Reprojeter un raster oblige à construire une nouvelle grille et à ESTIMER
# la valeur de chaque cellule : les données changent légèrement.
# Reprojetez une seule fois, le plus tôt possible.


# ---- 1.4 Recadrer et masquer -------------------------------------------------

# crop() : recadre sur l'emprise rectangulaire du vecteur (raster plus petit)
# mask() : met à NA toutes les cellules en dehors du contour exact
# On recadre D'ABORD (rapide), on masque ENSUITE (plus coûteux).
dem_lb_crop <- crop(dem_lb, vect(liban))
dem_lb      <- mask(dem_lb_crop, vect(liban))

# Comparer les deux résultats
par(mfrow = c(1, 2))
plot(dem_lb_crop, main = "Recadré (crop)")
plot(dem_lb,      main = "Recadré + masqué (mask)")
par(mfrow = c(1, 1))


# ---- 1.5 Cartographier le relief ---------------------------------------------

# geom_spatraster() (tidyterra) fait entrer un raster dans ggplot2.
# scale_fill_hypso_c() : palette hypsométrique (vert -> brun -> blanc),
# la convention des atlas physiques.
ggplot() +
  geom_spatraster(data = dem_lb) +
  geom_sf(data = liban, fill = NA, colour = "grey20", linewidth = 0.4) +
  scale_fill_hypso_c(palette = "dem_poster", na.value = NA,
                     name = "Altitude (m)") +
  labs(title    = "Relief du Liban",
       subtitle = "Modèle numérique de terrain, gouvernorats en surimpression",
       caption  = "Source : SRTM ; GADM") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# ---- 1.6 Calculer la pente ---------------------------------------------------

# La pente compare une différence d'altitude (m) à une distance horizontale :
# un SCR en MÈTRES est indispensable, sinon la pente est fausse.
pente_lb <- terrain(dem_lb, v = "slope", unit = "degrees")

global(pente_lb, c("mean", "max"), na.rm = TRUE)

ggplot() +
  geom_spatraster(data = pente_lb) +
  geom_sf(data = liban, fill = NA, colour = "white", linewidth = 0.4) +
  scale_fill_viridis_c(option = "magma", na.value = NA, name = "Pente (°)") +
  labs(title = "Pente du terrain — Liban", caption = "Source : SRTM ; GADM") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Les deux chaînes (Mont-Liban à l'ouest, Anti-Liban à l'est) ressortent,
# séparées par la plaine de la Bekaa.


# ---- 1.7 Reclassifier en zones d'altitude ------------------------------------

# Matrice à 3 colonnes : de, à, nouvelle valeur.
# On réutilisera EXACTEMENT la même matrice pour la Corse, afin de comparer.
rcl <- matrix(c(
  -Inf,  200, 1,   # Littoral
   200,  800, 2,   # Basse montagne
   800, 1500, 3,   # Montagne
  1500,  Inf, 4    # Haute montagne
), ncol = 3, byrow = TRUE)

zones_lb <- classify(dem_lb, rcl)

# Noms et couleurs des zones (légendes et tableaux)
noms_zones <- c("1" = "Littoral (< 200 m)",
                "2" = "Basse montagne (200–800 m)",
                "3" = "Montagne (800–1 500 m)",
                "4" = "Haute montagne (> 1 500 m)")
couleurs_zones <- c("1" = "#74add1", "2" = "#fee090",
                    "3" = "#fc8d59", "4" = "#d73027")

# Part de chaque zone (en % des cellules)
freq(zones_lb) |>
  mutate(zone = noms_zones[as.character(value)],
         part = round(count / sum(count) * 100, 1)) |>
  select(zone, part)

ggplot() +
  geom_spatraster(data = as.factor(zones_lb)) +
  scale_fill_manual(values = couleurs_zones, labels = noms_zones,
                    na.value = NA, na.translate = FALSE,
                    name = "Zone d'altitude") +
  labs(title = "Zones d'altitude — Liban") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# ---- 1.8 Du raster au vecteur (1) : valeurs en des points --------------------

# 2 000 ménages enquêtés (localisations fictives) : on veut que chaque
# ménage connaisse son altitude.
menages <- read_sf("data/Liban/random_survey_LBN.shp")
st_crs(menages)$input                         # WGS 84 : pas le même SCR !

menages <- st_transform(menages, crs(dem_lb)) # on aligne les SCR

# extract() lit la valeur du raster à chaque point
alt <- terra::extract(dem_lb, vect(menages))
menages$altitude <- alt[[2]]                  # 1re colonne = ID, 2e = valeur

summary(menages$altitude)

# Une colonne remplie de NA ? C'est presque toujours un problème de SCR.
# Ici, quelques NA sont normaux : points juste en dehors du masque.

ggplot() +
  geom_sf(data = liban, fill = "grey95", colour = "grey60") +
  geom_sf(data = filter(menages, !is.na(altitude)),
          aes(colour = altitude), size = 0.8) +
  scale_colour_hypso_c(palette = "dem_poster", name = "Altitude (m)") +
  labs(title = "Altitude des ménages enquêtés") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# ---- 1.9 Du raster au vecteur (2) : statistiques zonales ---------------------

# On résume toutes les cellules de chaque polygone : une valeur par zone
# administrative. exact_extract() pondère les cellules qui ne sont que
# partiellement dans un polygone.
liban$alt_moy   <- exact_extract(dem_lb,   liban, "mean", progress = FALSE)
liban$pente_moy <- exact_extract(pente_lb, liban, "mean", progress = FALSE)

liban |>
  st_drop_geometry() |>
  select(NAME_1, alt_moy, pente_moy) |>
  arrange(desc(alt_moy))

# Le résultat est un objet sf ordinaire : c'est désormais une carte
# choroplèthe, comme dans les séances précédentes.
ggplot(liban) +
  geom_sf(aes(fill = alt_moy), colour = "white", linewidth = 0.4) +
  scale_fill_distiller(palette = "YlOrBr", direction = 1,
                       name = "Altitude\nmoyenne (m)") +
  labs(title    = "Altitude moyenne par gouvernorat — Liban",
       subtitle = "Statistiques zonales à partir du MNT",
       caption  = "Source : SRTM ; GADM") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


# =============================================================================
# PARTIE 2 — LA CORSE (À VOUS)
# =============================================================================
#
# La Corse a un relief comparable : une côte méditerranéenne et un massif qui
# culmine au Monte Cinto (2 706 m). Reproduisez la démarche de la partie 1.
#
# Fichiers :
#   data/France/Corse/Corse_tout_lambert.tif      MNT de la Corse, Lambert 93
#   data/France/gadm41_FRA_shp/gadm41_FRA_5.shp   Communes de France (GADM)
#
# Gardez les noms d'objets proposés (dem_co, pente_co, zones_co,
# communes_co) : la partie 3 les utilise.


# ---- Exercice 1 — Charger et explorer le MNT ---------------------------------

# Chargez le MNT de la Corse dans un objet dem_co.
# Quel est son SCR ? Sa résolution ? Combien de cellules (ncell()) ?
# Comparez avec le Liban.

dem_co <-


# ---- Exercice 2 — Harmoniser la résolution -----------------------------------

# Le MNT corse a des cellules d'environ 27 m, contre environ 250 m pour le
# Liban. Une pente calculée sur des cellules fines n'est pas comparable à une
# pente calculée sur des cellules larges.
# Utilisez aggregate() pour ramener dem_co à environ 250 m.
# Indice : fact = combien de cellules de 27 m dans 250 m ? fun = "mean"

dem_co <-


# Remarque : si le MNT était découpé en tuiles, on les assemblerait avec
# mosaic(t1, t2, t3, t4, fun = "mean").


# ---- Exercice 3 — Recadrer et masquer ----------------------------------------

# 1. Chargez les communes de France et gardez celles de la Corse
#    (NAME_1 == "Corse"). Combien y en a-t-il ?
# 2. Mettez-les dans le SCR du MNT (st_transform).
#    Question : pourquoi reprojeter le vecteur plutôt que le raster ici ?
# 3. Recadrez puis masquez dem_co.

communes_co <-

dem_co <-


# ---- Exercice 4 — Cartographier le relief ------------------------------------

# Carte hypsométrique du MNT corse, avec un titre et une source.




# ---- Exercice 5 — Calculer la pente ------------------------------------------

# Calculez la pente en degrés (objet pente_co), sa moyenne et son maximum,
# puis cartographiez-la.
# Question : pourquoi peut-on la calculer directement sur ce MNT ?

pente_co <-


# ---- Exercice 6 — Reclassifier en zones d'altitude ---------------------------

# Reclassifiez dem_co avec LA MÊME matrice rcl que pour le Liban
# (objet zones_co), calculez la part de chaque zone et cartographiez.

zones_co <-


# ---- Exercice 7 — Statistiques zonales par commune ---------------------------

# Calculez l'altitude moyenne de chaque commune avec exact_extract()
# (colonne communes_co$alt_moy), puis cartographiez-la.
# Quelles sont les 5 communes les plus hautes ?

communes_co$alt_moy <-



# =============================================================================
# PARTIE 3 — COMPARER LE LIBAN ET LA CORSE
# =============================================================================
#
# À exécuter une fois la partie 2 terminée : les deux MNT sont alors à une
# résolution comparable (environ 250 m), en mètres, et reclassifiés avec les
# mêmes seuils.


# ---- 3.1 Tableau de synthèse -------------------------------------------------

synthese <- data.frame(
  territoire = c("Liban", "Corse"),
  alt_moy    = c(global(dem_lb,   "mean", na.rm = TRUE)[[1]],
                 global(dem_co,   "mean", na.rm = TRUE)[[1]]),
  alt_max    = c(global(dem_lb,   "max",  na.rm = TRUE)[[1]],
                 global(dem_co,   "max",  na.rm = TRUE)[[1]]),
  pente_moy  = c(global(pente_lb, "mean", na.rm = TRUE)[[1]],
                 global(pente_co, "mean", na.rm = TRUE)[[1]]),
  # surface = nombre de cellules non vides × surface d'une cellule (km²)
  surface_km2 = c(global(!is.na(dem_lb), "sum")[[1]] * prod(res(dem_lb)) / 1e6,
                  global(!is.na(dem_co), "sum")[[1]] * prod(res(dem_co)) / 1e6)
)

synthese


# ---- 3.2 Comparer les zones d'altitude ---------------------------------------

parts <- bind_rows(
  freq(zones_lb) |> mutate(territoire = "Liban"),
  freq(zones_co) |> mutate(territoire = "Corse")
) |>
  group_by(territoire) |>
  mutate(part = count / sum(count) * 100,
         zone = factor(noms_zones[as.character(value)], levels = noms_zones)) |>
  ungroup()

ggplot(parts, aes(x = territoire, y = part, fill = zone)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = setNames(couleurs_zones, noms_zones),
                    name = "Zone d'altitude") +
  labs(x = NULL, y = "Part du territoire (%)",
       title = "Répartition du territoire par zone d'altitude") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom", legend.direction = "vertical")


# ---- 3.3 À discuter ----------------------------------------------------------
#
# - Lequel des deux territoires est le plus MONTAGNEUX ? Le plus ESCARPÉ ?
#   Est-ce la même chose ?
# - La surface calculée à partir des cellules correspond-elle exactement aux
#   chiffres officiels (Liban : 10 452 km² ; Corse : 8 680 km²) ? Pourquoi ?
# - L'altitude maximale de la Corse dans le tableau est inférieure à celle du
#   Monte Cinto (2 706 m). Pourquoi ? (Indice : que fait aggregate(fun =
#   "mean") aux sommets ?)
# - Que se passerait-il si l'on comparait la pente du Liban (cellules de
#   250 m) à celle de la Corse SANS agréger le MNT corse (cellules de 27 m) ?


# ---- Liens utiles ------------------------------------------------------------
#
# terra      : https://rspatial.github.io/terra/
# tidyterra  : https://dieghernan.github.io/tidyterra/
# Livre      : https://rspatial.org/
# Geocomputation with R : https://r.geocompx.org/spatial-operations.html
