# ============================================================
# RCarto — Systèmes de Coordonnées de Référence (SCR)
# Dr. Elisabetta Pietrostefani
# ============================================================
# Ce script couvre :
#   - Pourquoi les SCR sont indispensables
#   - Inspecter et déclarer un SCR avec sf
#   - st_set_crs() vs st_transform() : ne pas confondre !
#   - Effet de la projection sur la forme des territoires
#   - Application : la France en Lambert-93
#   - Calculer des superficies correctement
# ============================================================

library(sf)
library(spData)    # fournit l'objet 'world'
library(ggplot2)
library(dplyr)
library(patchwork) # assembler plusieurs graphiques côte à côte

# ============================================================
# RÈGLE D'OR
# Toujours vérifier et harmoniser les SCR avant de combiner
# deux couches spatiales. Des données en SCR différents
# peuvent se retrouver au milieu de l'océan si vous les
# superposez sans vérification.
# ============================================================

# ============================================================
# 1. INSPECTER UN SCR
# ============================================================
# st_crs() interroge le SCR d'un objet ou d'un code EPSG.
# Les attributs utiles :
#   $IsGeographic  : TRUE = degrés (lon/lat) | FALSE = mètres (projeté)
#   $units_gdal    : unité des coordonnées (degrees, metre…)
#   $srid          : identifiant standardisé (ex. EPSG:4326)
#   $proj4string   : chaîne proj4 complète

# Interroger directement par code EPSG
st_crs("EPSG:4326")   # WGS84 — coordonnées GPS standard

# Charger un fichier vectoriel et inspecter son SCR
vector_filepath <- system.file("shapes/world.gpkg", package = "spData")
new_vector      <- read_sf(vector_filepath)

st_crs(new_vector)$IsGeographic   # géographique (degrés) ou projeté (mètres) ?
st_crs(new_vector)$units_gdal     # unités des coordonnées
st_crs(new_vector)$srid           # code SRID (ex. EPSG:4326)
st_crs(new_vector)$proj4string    # chaîne proj4

# ============================================================
# 2. DÉCLARER VS REPROJETER : NE PAS CONFONDRE !
# ============================================================
#
#   st_set_crs(x, epsg)   → DÉCLARE le SCR sans toucher aux coordonnées
#                           À utiliser quand le SCR est ABSENT ou mal renseigné.
#
#   st_transform(x, epsg) → REPROJECTE les coordonnées vers un nouveau SCR
#                           À utiliser pour CHANGER de projection.
#
# Confondre les deux est l'une des erreurs les plus fréquentes en SIG.

# Exemple : déclarer un SCR manquant
# new_vector <- st_set_crs(new_vector, "EPSG:4326")

# Exemple : reprojeter en Lambert-93
# new_vector_lambert <- st_transform(new_vector, 2154)

# Codes EPSG courants à connaître :
#   4326  — WGS84          : GPS, données mondiales (degrés)
#   2154  — Lambert-93     : France métropolitaine (mètres) ✓
#   27700 — British National Grid : Royaume-Uni (mètres)
#   3857  — Web Mercator   : fonds de carte web (Google, OSM)
# Recherchez n'importe quel code sur https://epsg.io

# ============================================================
# 3. EFFET DE LA PROJECTION SUR LA FORME
# ============================================================
# Une même couche de données a une apparence très différente
# selon la projection. C'est un choix éditorial autant que technique.

# WGS84 — projection par défaut (longitude/latitude)
# Déforme les terres aux hautes latitudes (pôles semblent très grands)
p1 <- ggplot(world) +
  geom_sf(aes(fill = continent), show.legend = FALSE,
          linewidth = 0.1, color = "white") +
  scale_fill_brewer(palette = "Set3") +
  labs(title    = "WGS84 (EPSG:4326)",
       subtitle = "Longitude / latitude — déformation aux pôles") +
  theme_void(base_size = 10) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 8))

# Mollweide — projection équivalente (aire préservée)
# Idéale pour les cartes thématiques mondiales (densité, clima…)
world_moll <- st_transform(world, crs = "+proj=moll")
p2 <- ggplot(world_moll) +
  geom_sf(aes(fill = continent), show.legend = FALSE,
          linewidth = 0.1, color = "white") +
  scale_fill_brewer(palette = "Set3") +
  labs(title    = "Mollweide",
       subtitle = "Aire préservée — idéale pour les cartes thématiques mondiales") +
  theme_void(base_size = 10) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 8))

# Lambert azimutale centrée sur Paris
# Préserve les distances depuis le centre — vue "satellite"
world_laea <- st_transform(
  world, crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=2.3 +lat_0=48.9"
)
p3 <- ggplot(world_laea) +
  geom_sf(aes(fill = continent), show.legend = FALSE,
          linewidth = 0.1, color = "white") +
  scale_fill_brewer(palette = "Set3") +
  labs(title    = "Lambert azimutale (centrée sur Paris)",
       subtitle = "Distance préservée depuis le centre") +
  theme_void(base_size = 10) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 8))

# patchwork : / empile les graphiques verticalement
p1 / p2 / p3

# ============================================================
# 4. APPLICATION : LA FRANCE EN LAMBERT-93
# ============================================================
# Lambert-93 (EPSG:2154) est la projection officielle IGN pour
# la France métropolitaine. Comparer WGS84 et Lambert-93 montre
# pourquoi la projection choisie change la forme perçue du territoire.

france_wgs84   <- world[world$name_long == "France", ]
france_lambert <- st_transform(france_wgs84, 2154)   # reprojeter en Lambert-93

p_wgs <- ggplot(france_wgs84) +
  geom_sf(fill = "#2c7bb6", color = "white", linewidth = 0.5) +
  labs(title    = "WGS84 (EPSG:4326)",
       subtitle = "Degrés — non adapté aux calculs de distance") +
  theme_void(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 9))

p_lam <- ggplot(france_lambert) +
  geom_sf(fill = "#d7191c", color = "white", linewidth = 0.5) +
  labs(title    = "Lambert-93 (EPSG:2154)",
       subtitle = "Mètres — projection officielle IGN") +
  theme_void(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5, size = 9))

# patchwork : + place les graphiques côte à côte
p_wgs + p_lam

# ---- Calculer des superficies correctement ----------------
# st_area() calcule la superficie de chaque entité.
# En Lambert-93 (mètres), le résultat est en m².
# On divise par 1e6 pour obtenir des km².
# as.numeric() supprime l'unité attachée au résultat.

france_lambert |>
  mutate(superficie_km2 = as.numeric(st_area(france_lambert)) / 1e6) |>
  st_drop_geometry() |>
  select(name_long, superficie_km2)

# ============================================================
# EXERCICE 1 — Comparer trois projections
# ============================================================
# Reprojetez la France en Web Mercator (EPSG:3857) et affichez
# les trois projections côte à côte (WGS84, Lambert-93, Web Mercator).
# Laquelle déforme le plus le territoire ?
#
# Indice : utilisez st_transform(france_wgs84, 3857)
# puis reprenez la structure p_wgs + p_lam + p_mercator

france_mercator <- st_transform(france_wgs84, 3857)

p_mercator <- ggplot(france_mercator) +
  geom_sf(fill = "___", color = "white") +
  labs(title = "Web Mercator (EPSG:3857)") +
  theme_void()

# Affichez les trois côte à côte :
# p_wgs + p_lam + p_mercator

# ============================================================
# EXERCICE 2 — Superficies des régions françaises
# ============================================================
# 1. Chargez le shapefile des régions françaises
# 2. Vérifiez son SCR avec st_crs()
# 3. Reprojetez en Lambert-93
# 4. Calculez la superficie de chaque région avec st_area()
# 5. Cartographiez les superficies avec ggplot2
#
# Indice : n'oubliez pas de diviser par 1e6 pour obtenir des km²

france_reg <- read_sf("data/France/gadm41_FRA_shp/gadm41_FRA_1.shp")

st_crs(france_reg)   # étape 2 : quel est le SCR ?

france_reg_lambert <- st_transform(france_reg, ___)   # étape 3 : quel code EPSG ?

france_reg_lambert <- france_reg_lambert |>
  mutate(superficie_km2 = as.numeric(st_area(___)) / 1e6)  # étape 4

ggplot(france_reg_lambert) +
  geom_sf(aes(fill = superficie_km2), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(name = "Superficie (km²)") +
  labs(title   = "Superficie des régions françaises",
       caption = "Source : GADM | Cours R-Carto") +
  theme_void()
