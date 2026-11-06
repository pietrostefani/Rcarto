# ============================================================
# RCarto — Géotraitement : Londres
# Dr. Elisabetta Pietrostefani
# ============================================================
# Ce script couvre les opérations fondamentales de géotraitement
# appliquées aux ventes immobilières à Londres :
#   - Charger et spatialiser des données
#   - Définir le bon SCR
#   - Visualiser : carte de base, choroplèthe, flèche nord
#   - Jointure spatiale (st_join)
#   - Agréger par polygone (group_by + summarise)
#   - Zones tampons (st_buffer) et intersection (st_intersection)
#   - Calculs de distance (st_distance)
#   - Voisinage par distance (dnearneigh)
# ============================================================

library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggspatial)  # annotation_scale() et annotation_north_arrow()
library(spdep)      # poids spatiaux et voisinage
library(scales)

# ============================================================
# EXERCICE 1 — Charger les données
# ============================================================
# Chargez le shapefile des districts londoniens et le CSV des ventes.
#
# Indice :
#   read_sf(dsn = "chemin/fichier.shp")  → pour un shapefile
#   read.csv("chemin/fichier.csv")       → pour un tableau CSV

london <- ___(dsn = "data/London/Polygons/districts.shp")
ventes <- read.csv("data/London/Tables/housesales.csv")

# Vérification : combien de districts ? Combien de ventes ?
nrow(london)
nrow(ventes)
head(ventes)   # repérez les colonnes de coordonnées (colonnes 17 et 18)

# ============================================================
# EXERCICE 2 — Créer des points spatiaux depuis un CSV
# ============================================================
# Le CSV ventes est un data frame ordinaire avec des colonnes de coordonnées.
# Convertissez-le en objet sf avec st_as_sf().
#
# Indice : st_as_sf(data, coords = c(col_lon, col_lat))
#          Les coordonnées sont dans les colonnes 17 et 18.

ventes <- st_as_sf(ventes, coords = c(___, ___))

# ============================================================
# EXERCICE 3 — Définir le bon SCR
# ============================================================
# Les coordonnées de Londres sont en British National Grid (EPSG:27700).
# Déclarez ce SCR sur l'objet ventes.
#
# Rappel :
#   st_set_crs() déclare un SCR sans modifier les coordonnées
#   st_transform() reprojecte les coordonnées vers un nouveau SCR
# Ici on DÉCLARE, on ne reprojecte pas.

ventes <- st_set_crs(ventes, ___)

# ============================================================
# VISUALISATION DES WARDS (code fourni — lisez attentivement)
# ============================================================

wards <- read_sf("data/London/Polygons/wards.shp")
wards$level4p  <- wards$LEVEL4_5 / wards$POP16_74   # proportion avec niveau 4+
wards$level4pp <- wards$level4p * 100

# Carte minimale : fill et color constants (pas dans aes())
ggplot(wards) +
  geom_sf(fill = "grey90", color = "white", linewidth = 0.2) +
  theme_void(base_size = 12) +
  labs(title   = "Wards de Londres",
       caption = "Source : données Londres | Cours R-Carto") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Choroplèthe : fill dans aes() pour mapper une variable
# label_percent() formate la légende en pourcentages
ggplot(wards) +
  geom_sf(aes(fill = level4p), color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(name   = "Niveau 4+",
                        option = "mako",
                        labels = label_percent()) +
  labs(title   = "Part de la population avec qualification niveau 4+",
       caption = "Source : Recensement 2001 | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Carte complète avec échelle et flèche nord (ggspatial)
# annotation_scale()       : barre d'échelle (location = "bl" = bas gauche)
# annotation_north_arrow() : flèche nord
ggplot(wards) +
  geom_sf(aes(fill = level4p), color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(name   = "Niveau 4+",
                        option = "mako",
                        labels = label_percent()) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", pad_y = unit(1, "cm"),
                          style = north_arrow_fancy_orienteering()) +
  labs(title   = "Qualification niveau 4+ — Wards de Londres",
       caption = "Source : Recensement 2001 | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ============================================================
# EXERCICE 4 — Carte de base des districts
# ============================================================
# Créez une carte des districts londoniens avec leurs noms.
# geom_sf_text() ou geom_sf_label() placent du texte sur les centroïdes.
#
# Indice :
#   geom_sf(data = ..., fill = ..., color = ..., linewidth = ...)
#   geom_sf_text(data = ..., aes(label = DIST_NAME),
#                fun.geometry = sf::st_centroid, size = 2)

map1 <- ggplot() +
  ___(data = london, ___) +
  ___(data = london, aes(label = DIST_NAME),
      fun.geometry = sf::st_centroid, size = 2, color = "grey30") +
  theme_void(base_size = 12) +
  labs(title   = "Districts de Londres",
       caption = "Source : données Londres | Cours R-Carto") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
map1

# ============================================================
# EXERCICE 5 — Superposer les points de vente
# ============================================================
# Ajoutez les transactions immobilières (ventes) à la carte map1.
# Dans ggplot2, on ajoute une couche avec + geom_sf().
# Jouez sur size et alpha pour la lisibilité.

map1 + ___(data = ventes, color = "#d7191c", size = ___, alpha = ___)

# ============================================================
# JOINTURE SPATIALE : st_join()
# ============================================================
# st_join(x, y) associe à chaque entité de x les attributs de
# l'entité de y dans laquelle elle tombe (ou qui la touche).
#
# Rappel des fonctions spatiales :
#   st_join(polygons, points)     → chaque polygone hérite des attributs
#                                   des points qu'il contient
#   st_intersects(x, y)           → indices des paires qui se croisent
#   st_intersection(x, y)         → géométries découpées à l'intersection

# ============================================================
# EXERCICE 6 — Jointure spatiale districts × ventes
# ============================================================
# Créez un objet ventes_districts qui joint les polygones des districts
# avec les points de vente. Chaque district hérite des ventes qu'il contient.
#
# Indice : st_join(polygons, points) — l'ordre compte !
#          Le premier argument détermine la structure géométrique du résultat.

ventes_districts <- st_join(___, ___)
head(ventes_districts)

# ============================================================
# EXERCICE 7 — Prix moyen par district
# ============================================================
# À partir de ventes_districts, calculez pour chaque district :
#   - le nombre de ventes (count_sales)
#   - le prix moyen (mean_price)
#
# Indice : group_by() + summarise() du tidyverse.
#          Le prix est dans la colonne "price".
#          n() compte le nombre de lignes dans chaque groupe.

ventes_districts_agg <- ventes_districts |>
  group_by(___, ___) |>
  summarise(
    count_sales = n(),
    mean_price  = mean(___, na.rm = TRUE)
  )
head(ventes_districts_agg)

# ============================================================
# EXERCICE 8 — Améliorer la carte des prix
# ============================================================
# La carte ci-dessous est fonctionnelle mais peu soignée.
# Améliorez-la en ajoutant :
#   - une palette viridis option "magma"
#   - des labels en livres sterling (£)
#   - un meilleur thème
#
# Indice :
#   scale_fill_viridis(option = "magma", direction = -1,
#                      labels = dollar_format(prefix = "£"))

# Version initiale (à améliorer)
map2 <- ggplot() +
  geom_sf(data = ventes_districts_agg, aes(fill = mean_price),
          color = "white", linewidth = 0.3) +
  labs(x = "", y = "")
map2

# Votre version améliorée :
map3 <- ggplot() +
  geom_sf(data = ___, aes(fill = ___),
          color = "white", linewidth = 0.3) +
  scale_fill_viridis(
    name      = "Prix moyen",
    direction = ___,
    labels    = ___,
    option    = "___"
  ) +
  labs(title   = "Prix moyen des transactions immobilières — Londres",
       caption = "Source : données Londres | Cours R-Carto") +
  ___()   # choisissez un thème : theme_void(), theme_minimal()…
map3

# ============================================================
# EXERCICE 9 — Paris
# ============================================================
# Maintenant que vous maîtrisez le géotraitement sur Londres,
# reproduisez une carte similaire pour Paris.
#
# Étapes :
#   1. Chargez le shapefile des arrondissements parisiens :
#      PA <- read_sf("data/Paris/Polygons/arrondiss_lambert.shp")
#
#   2. Chargez les données socio-économiques :
#      donnees <- read.csv("data/Paris/Tables/donnees_arrondissement_enrichi.csv")
#
#   3. Faites la jointure avec left_join() sur le code arrondissement :
#      PA_data <- left_join(PA, donnees, by = c("c_arinsee" = "c_arinsee"))
#
#   4. Cartographiez une variable de votre choix avec une palette adaptée.
#
#   Corrigé : 7b_geoprocessing_Paris.R
#      Variables disponibles : prix_m2_appart, taux_chomage,
#      part_log_sociaux, revenu_median, densite_pop_km2, part_diplomes_sup

# ============================================================
# ZONES TAMPONS (code fourni — lisez attentivement)
# ============================================================
# Une zone tampon = zone géographique à une distance fixe d'un objet.
# Ici : population vivant à moins de 500 m de bâtiments classés londoniens.

batiments_classes <- read_sf("data/London/Points/listed_buildings_london.shp")

# Sélection aléatoire de 20 bâtiments pour l'exemple
set.seed(42)
sample_ids        <- sample(batiments_classes$ListEntry, 20)
batiments_classes <- batiments_classes[batiments_classes$ListEntry %in% sample_ids, ]

# Zones de recensement (OA = Output Areas, recensement 2011)
# Téléchargement (à décommenter si le fichier n'existe pas) :
# download.file(
#   url      = "https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip",
#   destfile = "data/London/statistical-gis-boundaries-london.zip"
# )
# unzip("data/London/statistical-gis-boundaries-london.zip", exdir = "data/London")

oas_census <- read_sf(
  "data/London/statistical-gis-boundaries-london/ESRI/OA_2011_London_gen_MHW.shp"
)
oas_census$area_census <- st_area(oas_census)

oas_Southwark <- oas_census[oas_census$LAD11NM == "Southwark", ]

ggplot(oas_Southwark) +
  geom_sf(fill = "#1a4f7a", color = "white", linewidth = 0.2) +
  labs(title   = "Zones de recensement — Southwark",
       caption = "Source : recensement 2011 | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Harmoniser les SCR avant toute opération spatiale (OBLIGATOIRE)
batiments_classes   <- st_transform(batiments_classes, st_crs(oas_census))

# st_buffer(x, dist = ...) : crée une zone à distance fixe (en mètres si SCR projeté)
buffers500m             <- st_buffer(batiments_classes, dist = 500)
buffers500m$area_buffer <- st_area(buffers500m)

ggplot() +
  geom_sf(data = buffers500m,    fill = "#cce5f0", color = "#1a6ea8",
          linewidth = 1.0, alpha = 0.4) +
  geom_sf(data = batiments_classes, color = "#d7191c", size = 2.5, shape = 17) +
  labs(title    = "Bâtiments classés et zones tampons (500 m)",
       subtitle = "20 bâtiments sélectionnés aléatoirement",
       caption  = "Source : données Londres | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5))

# st_make_valid() corrige les géométries invalides (toujours faire avant une intersection)
# st_intersection(x, y) : découpe les géométries à l'intersection — parties non sécantes supprimées
oas_census             <- st_make_valid(oas_census)
buffers_inter          <- st_intersection(buffers500m, oas_census)
buffers_inter$area_inter <- st_area(buffers_inter)

# Population pondérée par la part de surface couverte dans chaque tampon
# weight = aire de l'intersection / aire totale du tampon
buffers_agg <- buffers_inter |>
  mutate(weight     = as.numeric(area_inter / area_buffer),
         population = as.numeric(USUALRES)) |>
  group_by(ListEntry) |>
  summarise_at("population", list(sum = ~sum(. * weight)))

pop_moyenne <- round(mean(buffers_agg$sum))
cat("Population moyenne dans un rayon de 500 m :", pop_moyenne, "habitants\n")

# Rappel des différences entre les trois fonctions spatiales :
#   st_join(x, y)         → joint les attributs, conserve toutes les géométries de x
#   st_intersects(x, y)   → renvoie les indices des paires qui se croisent
#   st_intersection(x, y) → découpe les géométries : seules les parties communes sont gardées

# ============================================================
# CALCULS DE DISTANCE (code fourni)
# ============================================================
# st_distance(x, y) : distance entre deux objets sf
# Résultat : une matrice avec des unités (mètres si SCR en mètres)
# as.numeric() supprime l'unité pour des calculs ultérieurs

Hackney   <- london[london$DIST_NAME == "Hackney",  ]
Southwark <- london[london$DIST_NAME == "Southwark", ]

Hackney_centroid   <- st_centroid(Hackney)
Southwark_centroid <- st_centroid(Southwark)

dist <- st_distance(Hackney_centroid, Southwark_centroid)
cat("Distance Hackney → Southwark :", round(as.numeric(dist)), "mètres\n")

ggplot() +
  geom_sf(data = london, fill = "grey90", color = "grey70", linewidth = 0.3) +
  geom_sf(data = Hackney,   fill = "#2c7bb6", color = "white", linewidth = 0.5) +
  geom_sf(data = Southwark, fill = "#d7191c", color = "white", linewidth = 0.5) +
  geom_sf(data = Hackney_centroid,   color = "#1a4f7a", size = 4, shape = 19) +
  geom_sf(data = Southwark_centroid, color = "#a01010", size = 4, shape = 19) +
  geom_sf(data = st_union(Hackney_centroid, Southwark_centroid) |>
            st_cast("LINESTRING"),
          color = "#333333", linewidth = 1.0, linetype = "dashed") +
  annotate("label",
           x = -0.03, y = 51.525,
           label = paste0(round(as.numeric(dist) / 1000, 1), " km"),
           size = 4, fontface = "bold", fill = "white", label.size = 0.3) +
  labs(title   = "Distance entre Hackney et Southwark",
       caption = "Source : données Londres | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Distance vers plusieurs districts à la fois (matrice)
co <- london |> filter(grepl("Southwark|Croydon", DIST_NAME))
st_distance(Hackney_centroid, co)

# ============================================================
# VOISINAGE PAR DISTANCE (code fourni)
# ============================================================
# dnearneigh(x, d1, d2) : identifie pour chaque point tous les voisins
# dans un rayon entre d1 et d2 mètres.
# nb2listw() convertit la liste de voisins en matrice de poids spatiaux.

housesales    <- read.csv("data/London/Tables/housesales.csv")
housesales_sf <- st_as_sf(housesales, coords = c(17, 18)) |>
  st_set_crs(27700)

W_dist     <- dnearneigh(housesales_sf, d1 = 0, d2 = 2000,
                          row.names = housesales_sf$propid)
W_dist_mat <- nb2listw(W_dist, zero.policy = TRUE)
summary(W_dist_mat, zero.policy = TRUE)

plot(W_dist_mat,
     coords = housesales[, 17:18],
     pch    = 19, cex = 0.1, col = "#2c7bb6",
     main   = "Voisinage par distance (2 000 m)")
