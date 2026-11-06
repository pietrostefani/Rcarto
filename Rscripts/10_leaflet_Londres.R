# =============================================================================
# RCarto — Leaflet : privation à Londres
# Dr. Elisabetta Pietrostefani
#
# Une carte interactive de Londres par MSOA, avec trois indicateurs au choix :
#   - densité de population (milliers d'hab./km²)
#   - part des ménages en situation de privation (%)
#   - part des 60 ans et plus touchant le Pension Credit (%)
# et plusieurs fonds de carte ouverts (OpenStreetMap), sans clé d'API.
#
# Avant de commencer : ouvrez le projet (fichier .Rproj). Tous les chemins
# commencent par "data/...". Pour vérifier où vous êtes : getwd()
# Exécutez le script section par section (Ctrl/Cmd + Entrée). Les cartes
# s'affichent dans l'onglet "Viewer" de RStudio.
#
# install.packages(c("leaflet", "sf", "dplyr", "readr", "classInt",
#                    "htmltools", "htmlwidgets"))   # si besoin, une seule fois
# =============================================================================


# ---- Packages et fonction utile -------------------------------------------

library(leaflet)     # cartes interactives
library(sf)          # données vectorielles
library(dplyr)       # manipulation de données
library(readr)       # lecture de CSV
library(classInt)    # classes de Fisher-Jenks
library(htmltools)   # texte HTML pour les étiquettes
library(htmlwidgets) # enregistrer la carte

# Un fond OpenStreetMap en niveaux de gris : on ajoute une classe CSS aux
# tuiles, puis un filtre CSS qui les passe en gris (voir cartes_interactives.qmd)
ajouter_osm_gris <- function(carte, group = NULL) {
  carte |>
    addTiles(group = group, options = tileOptions(className = "osm-gris")) |>
    htmlwidgets::prependContent(htmltools::tags$style(
      ".osm-gris { filter: grayscale(100%) brightness(1.05) contrast(0.85); }"))
}

# ---- Préparer les données ----------------------------------------------

msoa   <- read_sf("data/London/statistical-gis-boundaries-london/ESRI/MSOA_2011_London_gen_MHW.shp")
depriv <- read_csv("data/London/Tables/depriv_data.csv", show_col_types = FALSE)

londres <- msoa |>
  left_join(depriv, by = c("MSOA11CD" = "msoacode")) |>
  filter(!is.na(msoaname)) |>
  mutate(
    surface_km2 = as.numeric(st_area(geometry)) / 1e6,   # en British National Grid : des m²
    densite     = round(population / surface_km2 / 1000, 1)   # milliers d'hab./km²
  ) |>
  st_simplify(dTolerance = 25, preserveTopology = TRUE) |>   # contours allégés (25 m)
  st_transform(4326)

# Les arrondissements (boroughs), pour se repérer
boroughs <- read_sf("data/London/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") |>
  st_simplify(dTolerance = 25, preserveTopology = TRUE) |>
  st_transform(4326)

nrow(londres)
summary(select(st_drop_geometry(londres), densite, perc_depr_households, perc_pensioncredit))

# ---- Les palettes ------------------------------------------------------

# Une petite fonction : bornes de Fisher-Jenks, arrondies
bornes_jenks <- function(x, n = 6) {
  round(classIntervals(x, n = n, style = "fisher")$brks, 1)
}

pal_dens <- colorBin("Greens",  domain = londres$densite,
                     bins = bornes_jenks(londres$densite))
pal_depr <- colorBin("Blues",   domain = londres$perc_depr_households,
                     bins = bornes_jenks(londres$perc_depr_households))
pal_pens <- colorBin("Oranges", domain = londres$perc_pensioncredit,
                     bins = bornes_jenks(londres$perc_pensioncredit))

# ---- Les étiquettes ----------------------------------------------------

etiquettes <- sprintf(
  "<strong>%s</strong><br/><em>%s</em><br/>
   Densité : %.1f milliers d'hab./km²<br/>
   Ménages en privation : %.1f %%<br/>
   60 ans et plus au Pension Credit : %.1f %%",
  londres$msoaname, londres$boroughname,
  londres$densite, londres$perc_depr_households, londres$perc_pensioncredit
) |>
  lapply(HTML)

etiquettes[[1]]

# ---- Une fonction pour ajouter un indicateur ---------------------------

ajouter_indicateur <- function(carte, variable, palette, groupe) {
  carte |>
    addPolygons(
      fillColor   = palette(londres[[variable]]),
      fillOpacity = 0.8,
      color = "white", weight = 0.4,
      group = groupe,
      label = etiquettes,
      labelOptions = labelOptions(textsize = "13px", direction = "auto"),
      highlightOptions = highlightOptions(
        weight = 3, color = "#333333", fillOpacity = 0.95, bringToFront = TRUE)
    ) |>
    addLegend(
      pal = palette, values = londres[[variable]],
      title = groupe, group = groupe,          # la légende suit sa couche
      position = "bottomright", opacity = 0.8
    )
}

# ---- Assembler la carte ------------------------------------------------

groupes <- c("Densité (milliers d'hab./km²)",
             "Ménages en privation (%)",
             "60 ans et plus au Pension Credit (%)")

carte_londres <- leaflet(londres) |>
  # 1. Plusieurs fonds de carte ouverts, au choix
  ajouter_osm_gris(group = "OSM gris") |>
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") |>
  addProviderTiles(providers$OpenStreetMap.HOT,    group = "OSM Humanitaire") |>

  # 2. Les trois indicateurs, grâce à notre fonction
  ajouter_indicateur("densite",              pal_dens, groupes[1]) |>
  ajouter_indicateur("perc_depr_households", pal_depr, groupes[2]) |>
  ajouter_indicateur("perc_pensioncredit",   pal_pens, groupes[3]) |>

  # 3. Les limites des boroughs, pour se repérer
  addPolylines(data = boroughs, color = "#333333", weight = 1.2,
               group = "Limites des boroughs") |>

  # 4. Le panneau de contrôle des couches
  addLayersControl(
    baseGroups    = c("OSM gris", "OpenStreetMap", "OSM Humanitaire"),
    overlayGroups = c(groupes, "Limites des boroughs"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup(groupes[2:3]) |>      # au départ, seule la densité est visible

  # 5. Les finitions
  setView(lng = -0.118, lat = 51.51, zoom = 10) |>
  addScaleBar(position = "bottomleft") |>
  addMiniMap(tiles = providers$OpenStreetMap.Mapnik, toggleDisplay = TRUE,
             position = "topleft") |>
  addEasyButton(easyButton(
    icon = "fa-home", title = "Revenir à la vue d'ensemble",
    onClick = JS("function(btn, map){ map.setView([51.51, -0.118], 10); }")
  ))

carte_londres

# ---- Enregistrer la carte ----------------------------------------------

# Décommentez les lignes ci-dessous (enlevez les #) pour enregistrer la carte :
# saveWidget(carte_londres, "privation_londres.html",
#            selfcontained = TRUE, title = "Privation à Londres")


# ---- Lire la carte -----------------------------------------------------------
#
# - Où se concentrent les ménages en situation de privation ? Cette géographie
#   ressemble-t-elle à celle de la densité ?
# - La pauvreté des personnes âgées (Pension Credit) suit-elle la même
#   géographie que la privation des ménages ?
# - Changez de fond de carte : lequel rend la lecture la plus facile ?


# ---- À vous de jouer : Paris -------------------------------------------------
#
# Construisez une carte interactive de Paris avec les données de data/Paris/ :
#   1. le revenu médian par arrondissement en choroplèthe interactive ;
#   2. les stations Vélib' en cercles proportionnels à leur capacité ;
#   3. le nombre d'annonces Airbnb par km² dans chaque arrondissement ;
#   4. le tout avec plusieurs fonds de carte ouverts, un panneau de contrôle
#      des couches et une échelle, puis enregistrez la carte en HTML.
# Script de correction : 10b_cartes_interactives_Paris.R
