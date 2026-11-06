# ============================================================
# RCarto — Données Spatiales et Simple Features
# Dr. Elisabetta Pietrostefani
# ============================================================
# Ce script couvre :
#   - Carte interactive avec leaflet (voir aussi 9_cartes_interactives.R)
#   - Les 3 types de données spatiales : points, polygones, raster
#   - Le standard Simple Features (sf)
#   - Application : données réelles de Londres
# ============================================================

# ---- Installer les packages (une seule fois) ---------------
# install.packages(c("sf", "spData", "ggplot2", "dplyr",
#                    "leaflet", "maps", "stars", "terra"))

# ---- Charger les packages ----------------------------------
library(sf)        # Simple Features — données vectorielles
library(spData)    # jeux de données géographiques d'exemple
library(ggplot2)   # visualisation — notre outil principal
library(dplyr)     # manipulation de données
library(leaflet)   # cartes interactives
library(maps)      # données cartographiques de base

# ============================================================
# 1. CARTE INTERACTIVE
# ============================================================
# leaflet() crée une carte interactive dans le panneau Viewer
# addTiles() ajoute un fond de carte (ici : OpenStreetMap, libre et sans clé)
# addMarkers() place des marqueurs aux coordonnées indiquées

popup <- c("Clémence", "Baptiste", "Nathalie", "Daphné",
           "Fannie", "Gabrielle", "Yann", "Cédric", "Soilihi")

leaflet() |>
  addTiles() |>
  addMarkers(
    lng = c(-1.5536, -0.5792, 5.4474, -4.4861, -2.0257,
             3.0870,  2.3522,  4.4861,  2.0257),
    lat = c(47.2184, 44.8378, 43.5297, 48.3904, 48.6493,
            45.7772, 48.8566, 48.3904, 48.6493),
    popup = popup
  )

# ------------------------------------------------------------
# EXERCICE 1
# Ajoutez votre propre ville à la carte ci-dessus.
# Trouvez ses coordonnées sur https://www.latlong.net/
# Ajoutez une entrée dans lng, lat et popup, puis relancez.
# ------------------------------------------------------------

# ============================================================
# 2. LES TROIS TYPES DE DONNÉES SPATIALES
# ============================================================

# ---- 2a. Points : des lieux précis ------------------------
# Un point = une paire de coordonnées (longitude, latitude).
# st_as_sf() convertit un data frame ordinaire en objet sf.
#   coords = c("lon", "lat") : colonnes contenant les coordonnées
#   crs = 4326 : déclare le SCR WGS84 (coordonnées GPS standard)

cities <- data.frame(
  nom = c("Paris", "Londres", "Berlin", "Madrid", "Rome"),
  lon = c(2.35,  -0.12,  13.40,  -3.70,  12.50),
  lat = c(48.85,  51.51,  52.52,  40.42,  41.90)
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# geom_sf() dessine n'importe quel objet sf (points, lignes, polygones).
# On superpose deux couches avec deux appels geom_sf() séparés.
# coord_sf(xlim, ylim) zoome sur une zone géographique.
ggplot() +
  geom_sf(data = world[world$continent == "Europe", ],
          fill = "grey90", color = "white", linewidth = 0.3) +
  geom_sf(data = cities, color = "#d7191c", size = 3) +
  geom_sf_label(data = cities, aes(label = nom),
                size = 3, nudge_y = 1.2, label.size = 0) +
  coord_sf(xlim = c(-10, 25), ylim = c(36, 58)) +
  labs(title   = "Points : capitales européennes",
       caption = "Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ------------------------------------------------------------
# EXERCICE 2
# Ajoutez une ville de votre choix au data frame cities.
# Trouvez ses coordonnées sur https://www.latlong.net/
# Relancez le code — votre ville doit apparaître sur la carte.
# ------------------------------------------------------------

# ---- 2b. Polygones : des surfaces -------------------------
# Un polygone = une séquence de points formant une surface fermée.
# Les polygones portent une table attributaire (colonnes de données).
# aes(fill = gdpPercap) colore chaque polygone selon sa valeur.
# na.value = "grey80" définit la couleur pour les données manquantes.

europe <- world[world$continent == "Europe", ]

ggplot(europe) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option   = "plasma",
    name     = "PIB/hab (USD)",
    na.value = "grey80",
    labels   = scales::label_comma()
  ) +
  coord_sf(xlim = c(-10, 40), ylim = c(34, 72)) +
  labs(title   = "Polygones : PIB par habitant en Europe",
       caption = "Source : spData | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title        = element_text(face = "bold", hjust = 0.5),
        legend.key.height = unit(1.2, "cm"))

# ---- 2c. Raster : une grille de valeurs ------------------
# Un raster divise l'espace en pixels, chacun avec une valeur numérique.
# C'est le format naturel pour les phénomènes continus (altitude, température…)
#
# Le dataset volcano (base R) est une matrice 87 x 61 d'altitudes
# représentant le volcan Maungawhau à Auckland (Nouvelle-Zélande).
#
# expand.grid() crée toutes les combinaisons de x et y (coordonnées pixel)
# as.vector(t(volcano)) "aplatit" la matrice en un vecteur de valeurs
# coord_equal() est important : garde le ratio 1:1 pour ne pas déformer le raster

volcano_df <- expand.grid(
  x = 1:ncol(volcano),
  y = 1:nrow(volcano)
)
volcano_df$altitude <- as.vector(t(volcano))

ggplot(volcano_df, aes(x = x, y = y, fill = altitude)) +
  geom_raster() +
  scale_fill_gradientn(
    colors = hcl.colors(50, "Terrain"),
    name   = "Altitude (m)"
  ) +
  coord_equal() +
  labs(title    = "Raster : Maungawhau — Auckland, Nouvelle-Zélande",
       subtitle = "Chaque pixel = une cellule de la matrice d'altitudes",
       caption  = "Source : dataset volcano (base R) | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5))

# ============================================================
# 3. SIMPLE FEATURES (sf) : LE STANDARD VECTORIEL DANS R
# ============================================================
# sf stocke les géométries dans une colonne spéciale d'un data frame
# ordinaire — toutes les fonctions dplyr (filter, mutate, select…)
# fonctionnent directement dessus.
#
# Syntaxe WKT (Well-Known Text) — représentation textuelle des géométries :
#   POINT (5 2)
#   LINESTRING (1 5, 4 4, 4 1, 2 2, 3 2)
#   POLYGON ((1 5, 2 2, 4 1, 4 4, 1 5))
#   POLYGON avec trou : POLYGON ((ext…), (trou…))
#   MULTIPOLYGON (((…)), ((…)))
#
# st_as_sfc() convertit une chaîne WKT en géométrie sf

old_par <- par(mfrow = c(1, 3), pty = "s", mar = c(1, 2, 2, 1))

plot(st_as_sfc("POINT(5 2)"),
     axes = TRUE, main = "POINT", col = "#2c7bb6", pch = 19, cex = 2)

plot(st_as_sfc("LINESTRING(1 5, 4 4, 4 1, 2 2, 3 2)"),
     axes = TRUE, main = "LINESTRING", col = "#d7191c", lwd = 2)

plot(st_as_sfc("POLYGON((1 5, 2 2, 4 1, 4 4, 1 5))"),
     col = "#ffffbf", border = "#2c3e50", axes = TRUE, main = "POLYGON")

par(old_par)

# ---- Explorer le dataset 'world' --------------------------
# world est un objet sf fourni par spData — 177 pays avec attributs.
# class() : confirme que c'est un objet sf (et aussi un data frame)
# names() : liste toutes les colonnes disponibles

class(world)
names(world)

ggplot() +
  geom_sf(data = world, aes(fill = gdpPercap)) +
  scale_fill_viridis_c(
    option   = "plasma",
    name     = "PIB/hab (USD)",
    na.value = "grey80"
  ) +
  labs(title   = "PIB par habitant dans le monde",
       caption = "Source : spData | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ---- Filtrer et superposer des couches --------------------
# Filtrage par condition : world[condition, ] — comme un data frame.
# st_union() fusionne plusieurs polygones en un seul contour.
# On superpose les couches avec des appels geom_sf() successifs.
# L'ordre compte : la dernière couche est dessinée par-dessus.

world_africa <- world[world$continent == "Africa", ]
africa       <- st_union(world_africa)

ggplot() +
  geom_sf(data = world,  fill = "grey90", color = "white", linewidth = 0.2) +
  geom_sf(data = africa, fill = "#d7191c", color = "white", linewidth = 0.3) +
  labs(title   = "Le continent africain",
       caption = "Source : spData | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Isoler un pays dans son contexte continental
kenya <- world[world$name_long == "Kenya", ]

ggplot() +
  geom_sf(data = world_africa, fill = "grey85", color = "white", linewidth = 0.3) +
  geom_sf(data = kenya,        fill = "#2c7bb6", color = "white", linewidth = 0.5) +
  labs(title   = "Le Kenya en Afrique",
       caption = "Source : spData | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ------------------------------------------------------------
# EXERCICE 3
# Adaptez le code ci-dessus pour mettre en évidence
# un autre pays de votre choix sur son continent.
#
# Indice : unique(world$name_long) liste tous les noms de pays.
#          unique(world$continent) liste les continents disponibles.
# ------------------------------------------------------------

# ============================================================
# 4. APPLICATION : DONNÉES RÉELLES DE LONDRES
# ============================================================

# ---- Charger le shapefile des districts ------------------
# read_sf() lit les principaux formats vectoriels : .shp, .gpkg, .geojson…
# L'objet résultant est un sf data frame — inspectez-le comme un tableau.

districts <- read_sf("data/London/Polygons/districts.shp")

head(districts)        # premières lignes
st_crs(districts)      # quel est le SCR ?
nrow(districts)        # combien de districts ?

# Carte de base
ggplot(districts) +
  geom_sf(fill = "lightgrey", color = "white", linewidth = 0.4) +
  labs(title   = "Districts de Londres",
       caption = "Source : données Londres | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ---- Indexation et sélection ------------------------------
# Les objets sf se comportent exactement comme des data frames :

districts[1, ]                              # première ligne
districts[, 1]                              # première colonne
districts[1, 1]                             # cellule [1,1]
districts$DIST_NAME                         # colonne par son nom
districts[districts$DIST_NAME == "Hackney", ]  # filtrage conditionnel

# Avec dplyr : filter() est plus lisible pour des conditions complexes
hackney <- districts |> filter(DIST_NAME == "Hackney")

ggplot() +
  geom_sf(data = districts, fill = "lightgrey", color = "white", linewidth = 0.3) +
  geom_sf(data = hackney,   fill = "#2c7bb6",   color = "white", linewidth = 0.6) +
  labs(title   = "Hackney parmi les districts de Londres",
       caption = "Source : données Londres | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ------------------------------------------------------------
# EXERCICE 4
# Modifiez le code ci-dessus pour mettre en évidence
# un autre district de votre choix.
#
# Indice : unique(districts$DIST_NAME) liste tous les districts.
# Exemples : "Islington", "Tower Hamlets", "Southwark", "Camden"
# ------------------------------------------------------------

# ---- Jointure attributaire --------------------------------
# Les shapefiles ne contiennent souvent que les géométries.
# On charge les données dans un CSV séparé, puis on les joint.
#
# left_join(x, y, by = ...) conserve toutes les lignes de x
# et ajoute les colonnes de y où les clés correspondent.
# Si une ligne de x n'a pas de correspondance dans y → NA.
#
# Ici la clé est : DIST_CODE (dans districts) = Zone_Code (dans le CSV)

qualifications2001_df <- read.csv("data/London/Tables/qualifications2001_2.csv")
head(qualifications2001_df)   # repérer les colonnes disponibles

districts <- left_join(
  districts,
  qualifications2001_df,
  by = c("DIST_CODE" = "Zone_Code")
)

# mutate() crée une nouvelle colonne calculée
# Ici : proportion de personnes avec qualification niveau 4 ou plus
districts <- districts |>
  mutate(Level4p = Level4 / Population1674)

# st_drop_geometry() retire la colonne géométrie pour afficher
# un tableau ordinaire plus lisible
head(districts |> st_drop_geometry() |>
       select(DIST_NAME, Level4, Population1674, Level4p))

# ---- Carte choroplèthe : niveau d'éducation --------------
# aes(fill = Level4p) : colore chaque district selon sa valeur
# label_percent() : formate les étiquettes de légende en pourcentages
# legend.key.height : contrôle la hauteur de la barre de légende

ggplot(districts) +
  geom_sf(aes(fill = Level4p), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "viridis",
    name   = "Part niveau 4+",
    labels = scales::label_percent()
  ) +
  labs(title    = "Niveau d'éducation (qualification niveau 4+)",
       subtitle = "Districts londoniens — Recensement 2001",
       caption  = "Source : données Londres | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle     = element_text(color = "grey40", hjust = 0.5),
        legend.key.height = unit(1.2, "cm"))

# ---- Nuage de points : population vs éducation -----------
# La même grammaire ggplot2 sert pour les cartes ET les graphiques.
# check_overlap = TRUE évite que les étiquettes se chevauchent.
# nudge_x / nudge_y décale légèrement les étiquettes par rapport aux points.

ggplot(districts, aes(x = Population1674, y = Level4p)) +
  geom_point(aes(color = Level4p, size = Population1674), alpha = 0.8) +
  geom_text(
    aes(label = DIST_NAME, color = Level4p),
    size = 2.5, check_overlap = TRUE,
    nudge_x = 15000, nudge_y = 0.015
  ) +
  scale_color_viridis_c(option = "plasma", guide = "none") +
  scale_size_continuous(
    name   = "Population",
    range  = c(2, 8),
    labels = scales::label_comma()
  ) +
  scale_x_continuous(
    name   = "Population (16–74 ans)",
    labels = scales::label_comma()
  ) +
  scale_y_continuous(
    name   = "Part qualification niveau 4+",
    labels = scales::label_percent()
  ) +
  labs(title   = "Population et niveau d'éducation — Districts londoniens",
       caption = "Source : Recensement 2001 | Cours R-Carto") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ------------------------------------------------------------
# EXERCICE 5 — À vous de jouer !
# Créez une carte similaire pour Paris.
#
# Étapes :
#   1. Charger le shapefile des arrondissements :
#      PA <- read_sf("data/Paris/Polygons/arrondiss_lambert.shp")
#
#   2. Charger les données socio-économiques :
#      donnees <- read.csv("data/Paris/Tables/donnees_arrondissement_enrichi.csv")
#
#   3. Vérifier les colonnes disponibles :
#      names(PA)
#      names(donnees)
#
#   4. Joindre les deux tables (clé commune : "c_arinsee") :
#      PA <- left_join(PA, donnees, by = c("c_arinsee" = "c_arinsee"))
#
#   5. Cartographier une variable au choix :
#      ggplot(PA) +
#        geom_sf(aes(fill = ___), color = "white", linewidth = 0.4) +
#        scale_fill_viridis_c(name = "___") +
#        labs(title = "___", caption = "___") +
#        theme_void()
#
# Variables disponibles dans donnees_arrondissement_enrichi.csv :
#   prix_m2_appart    — Prix moyen au m² (€)
#   taux_chomage      — Taux de chômage (%)
#   part_log_sociaux  — Part de logements sociaux (%)
#   revenu_median     — Revenu médian mensuel (€)
#   densite_pop_km2   — Densité de population (hab/km²)
#   part_diplomes_sup — Part de diplômés du supérieur (%)
# ------------------------------------------------------------
