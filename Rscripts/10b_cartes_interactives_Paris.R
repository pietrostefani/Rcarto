# =============================================================================
# RCarto — Cartes interactives : Paris (Corrigé)
# Dr. Elisabetta Pietrostefani
#
# Une carte interactive de Paris avec :
#   1. le revenu médian par arrondissement (choroplèthe interactive)
#   2. les stations Vélib' (cercles proportionnels)
#   3. la densité d'annonces Airbnb par arrondissement (jointure spatiale)
#   4. le tout assemblé, avec des fonds de carte ouverts (OpenStreetMap, IGN)
#
# Avant de commencer : ouvrez le projet (fichier .Rproj). Tous les chemins
# commencent par "data/...". Pour vérifier où vous êtes : getwd()
# Exécutez le script section par section (Ctrl/Cmd + Entrée). Les cartes
# s'affichent dans l'onglet "Viewer" de RStudio.
#
# install.packages(c("leaflet", "sf", "dplyr", "readr", "classInt",
#                    "htmltools", "htmlwidgets"))   # si besoin, une seule fois
# =============================================================================


# ---- Packages et fonctions utiles ----------------------------------------

library(leaflet)     # cartes interactives
library(sf)          # données vectorielles
library(dplyr)       # manipulation de données
library(readr)       # lecture de CSV
library(classInt)    # classes de Fisher-Jenks
library(htmltools)   # texte HTML pour les étiquettes
library(htmlwidgets) # enregistrer la carte

# --- Fonds de carte IGN ----------------------------------------------------
# Les tuiles IGN sont servies par la Géoplateforme (data.geopf.fr), sans clé.
# Seuls le nom de la couche (LAYER) et le format d'image changent d'un fond
# à l'autre : on écrit une fonction qui construit l'adresse des tuiles.
# {z}, {x} et {y} sont remplacés par Leaflet : niveau de zoom, colonne, ligne.
url_ign <- function(couche, format = "image/png") {
  paste0("https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0",
         "&STYLE=normal&TILEMATRIXSET=PM&FORMAT=", format,
         "&LAYER=", couche,
         "&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}")
}
url_plan_ign  <- url_ign("GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2")                # Plan IGN
url_ortho_ign <- url_ign("ORTHOIMAGERY.ORTHOPHOTOS", format = "image/jpeg")  # photos aériennes

# --- Fond OpenStreetMap gris -------------------------------------------------
# addTiles() ajoute les tuiles OpenStreetMap (libres, sans clé).
# className = "osm-gris" donne une étiquette CSS à ces tuiles seulement ;
# la règle CSS ajoutée avec prependContent() les passe en niveaux de gris.
# Les polygones et les cercles, eux, gardent leurs couleurs.
ajouter_osm_gris <- function(carte, group = NULL) {
  carte |>
    addTiles(group = group, options = tileOptions(className = "osm-gris")) |>
    htmlwidgets::prependContent(htmltools::tags$style(
      ".osm-gris { filter: grayscale(100%) brightness(1.05) contrast(0.85); }"))
}

# ---- Exercice 1 — Le revenu médian par arrondissement ------------------

# -- Préparer les données

# 1. Les contours des 20 arrondissements (en Lambert 93, en mètres)
arr <- read_sf("data/Paris/Polygons/arrondiss_lambert.shp")

# 2. Les données socio-économiques par arrondissement
#    Certaines colonnes sont lues comme du texte à cause des virgules ("16,055").
#    Pour chaque colonne de texte :
#      - gsub() retire les virgules ("16,055" -> "16055") ;
#      - si le résultat ne contient que des chiffres (et un point), on le
#        convertit en nombre ; sinon (ex. "1er Ardt"), on garde le texte.
donnees <- read.csv("data/Paris/Tables/donnees_arrondissement_enrichi.csv") |>
  mutate(across(where(is.character), ~ {
    x <- gsub(",", "", .x)
    ifelse(grepl("^-?[0-9.]+$", x), as.numeric(x), .x)
  }))

# 3. Jointure attributaire : on ajoute les colonnes de 'donnees' aux polygones,
#    en faisant correspondre le code INSEE de l'arrondissement (c_arinsee)
arr <- arr |>
  left_join(donnees, by = c("c_arinsee" = "c_arinsee"))

# 4. Vérification : quelques colonnes, sans la géométrie (plus lisible)
arr |>
  st_drop_geometry() |>
  select(l_aroff, revenu_median, prix_m2_appart, taux_chomage) |>
  head()

# Copie en WGS 84 (longitude / latitude), le système des cartes web
arr_wgs <- st_transform(arr, 4326)

# -- La carte

# 1. La palette : une fonction qui transforme un revenu en couleur.
#    - colorBin() : des classes dont on fixe les bornes ;
#    - les bornes sont calculées par la méthode de Fisher-Jenks (5 classes) ;
#    - "YlGnBu" : palette séquentielle jaune -> vert -> bleu (ColorBrewer).
pal_revenu <- colorBin("YlGnBu", domain = arr_wgs$revenu_median,
                       bins = classIntervals(arr_wgs$revenu_median, n = 5,
                                             style = "fisher")$brks)

# 2. Les étiquettes affichées au survol, en HTML :
#    - sprintf() insère les valeurs dans le modèle (%s = texte) ;
#    - <strong> = gras, <br/> = retour à la ligne ;
#    - format(big.mark = " ") écrit 3 890 au lieu de 3890 ;
#    - lapply(HTML) indique à leaflet que c'est du HTML, pas du texte brut.
etiq_revenu <- sprintf("<strong>%s</strong><br/>Revenu médian : %s € / mois",
                       arr_wgs$l_aroff,
                       format(arr_wgs$revenu_median, big.mark = " ")) |>
  lapply(HTML)

# 3. La carte, construite couche par couche
leaflet(arr_wgs) |>
  ajouter_osm_gris() |>                                # fond de carte gris
  addPolygons(
    fillColor   = ~pal_revenu(revenu_median),          # couleur selon le revenu (~ = colonne)
    fillOpacity = 0.8,                                 # légère transparence
    color = "white", weight = 1,                       # contours blancs fins
    label = etiq_revenu,                               # info-bulle au survol
    highlightOptions = highlightOptions(               # mise en évidence au survol :
      weight = 3, color = "#333333",                   #   contour plus épais et foncé
      fillOpacity = 0.95, bringToFront = TRUE)         #   couleur pleine, polygone au premier plan
  ) |>
  addLegend(pal = pal_revenu, values = ~revenu_median, opacity = 0.8,
            title = "Revenu médian (€ / mois)", position = "bottomright",
            labFormat = labelFormat(big.mark = " ", digits = 0))  # 3 890, sans décimale

# ---- Exercice 2 — Les stations Vélib' ----------------------------------

# Lecture du GeoJSON : read_sf() lit ce format comme un shapefile.
# On ne garde que les stations situées dans Paris (pas les communes voisines).
velib <- read_sf("data/Paris/Points/velib-disponibilite-en-temps-reel.geojson") |>
  filter(nom_arrondissement_communes == "Paris")

nrow(velib)            # nombre de stations
st_crs(velib)$input    # déjà en WGS 84 : pas besoin de reprojeter

leaflet(velib) |>
  ajouter_osm_gris() |>
  addCircleMarkers(
    # Taille du cercle : on prend la racine carrée de la capacité pour que la
    # SURFACE du cercle (et non son rayon) soit proportionnelle au nombre de bornes.
    # * 0.6 réduit la taille générale pour limiter les chevauchements.
    radius = ~sqrt(capacity) * 0.6,
    color = "white", weight = 0.5,                # fin contour blanc autour des cercles
    fillColor = "#6E9B3E", fillOpacity = 0.8,     # vert Vélib'
    label  = ~name,                               # au survol : le nom de la station
    # Au clic : une petite fiche en HTML, assemblée avec paste0()
    popup  = ~paste0("<b>", name, "</b><br>",
                     numbikesavailable, " vélos disponibles ",
                     "(", mechanical, " mécaniques, ", ebike, " électriques)<br>",
                     capacity, " bornes")
  )

# ---- Exercice 3 — La densité d'annonces Airbnb -------------------------

# 1. Les annonces : un simple tableau avec des colonnes longitude et latitude.
#    st_as_sf() le transforme en objet spatial (des points) ;
#    crs = 4326 indique que les coordonnées sont en WGS 84 (degrés).
#    On reprojette ensuite dans le même SCR que les arrondissements (Lambert 93) :
#    une jointure spatiale exige que les deux couches soient dans le même SCR.
airbnb <- read_csv("data/Paris/airbnb/listings.csv", show_col_types = FALSE) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(st_crs(arr))

nrow(airbnb)   # nombre d'annonces

# 2. Jointure spatiale : st_join() ajoute à chaque annonce le code de
#    l'arrondissement dans lequel elle se trouve (point dans polygone).
#    On retire ensuite la géométrie et on compte les annonces par arrondissement.
airbnb_arr <- st_join(airbnb, arr["c_arinsee"]) |>
  st_drop_geometry() |>
  count(c_arinsee, name = "n_airbnb")

# 3. On ajoute ce comptage aux arrondissements et on calcule une DENSITÉ :
#    st_area() donne la surface en m² (Lambert 93), / 1e6 la convertit en km².
arr <- arr |>
  left_join(airbnb_arr, by = "c_arinsee") |>
  mutate(surface_km2    = as.numeric(st_area(geometry)) / 1e6,
         airbnb_par_km2 = round(n_airbnb / surface_km2))

# 4. On met à jour la copie en WGS 84 pour la carte web
arr_wgs <- st_transform(arr, 4326)

# 5. Les arrondissements où les annonces sont les plus denses
arr |>
  st_drop_geometry() |>
  select(l_aroff, n_airbnb, airbnb_par_km2) |>
  arrange(desc(airbnb_par_km2)) |>
  head()

# ---- Exercice 4 — Assembler la carte -----------------------------------

# 1. Une palette pour le deuxième indicateur (rose -> violet, "RdPu"),
#    différente de celle du revenu pour ne pas confondre les deux couches
pal_airbnb <- colorBin("RdPu", domain = arr_wgs$airbnb_par_km2,
                       bins = classIntervals(arr_wgs$airbnb_par_km2, n = 5,
                                             style = "fisher")$brks)

# 2. Une seule étiquette pour les deux couches, avec les deux indicateurs :
#    quelle que soit la couche affichée, on lit les deux valeurs au survol.
etiq_arr <- sprintf(
  "<strong>%s</strong><br/>Revenu médian : %s € / mois<br/>Annonces Airbnb : %s par km²",
  arr_wgs$l_aroff,
  format(arr_wgs$revenu_median, big.mark = " "),
  format(arr_wgs$airbnb_par_km2, big.mark = " ")
) |>
  lapply(HTML)

# 3. Les options de mise en évidence, définies une fois et réutilisées
surbrillance <- highlightOptions(weight = 3, color = "#333333",
                                 fillOpacity = 0.95, bringToFront = TRUE)

# 4. La carte. On part d'une carte vide (leaflet() sans données) et on
#    indique les données couche par couche avec 'data = '.
#    Le paramètre 'group' donne un nom à chaque couche : c'est ce nom qui
#    apparaît dans le panneau de contrôle et qui relie une légende à sa couche.
carte_paris <- leaflet() |>

  # --- Fonds de carte (un seul visible à la fois) ---
  ajouter_osm_gris(group = "OSM gris") |>
  addTiles(urlTemplate = url_plan_ign,  attribution = "© IGN – Géoplateforme",
           group = "Plan IGN") |>
  addTiles(urlTemplate = url_ortho_ign, attribution = "© IGN – Géoplateforme",
           group = "Photographies aériennes IGN") |>

  # --- Indicateur 1 : revenu médian (polygones + légende du même groupe) ---
  addPolygons(data = arr_wgs, group = "Revenu médian",
              fillColor = ~pal_revenu(revenu_median), fillOpacity = 0.75,
              color = "white", weight = 1, label = etiq_arr,
              highlightOptions = surbrillance) |>
  addLegend(data = arr_wgs, pal = pal_revenu, values = ~revenu_median,
            title = "Revenu médian (€ / mois)", group = "Revenu médian",
            position = "bottomright", opacity = 0.8,
            labFormat = labelFormat(big.mark = " ", digits = 0)) |>

  # --- Indicateur 2 : densité d'annonces Airbnb ---
  addPolygons(data = arr_wgs, group = "Annonces Airbnb",
              fillColor = ~pal_airbnb(airbnb_par_km2), fillOpacity = 0.75,
              color = "white", weight = 1, label = etiq_arr,
              highlightOptions = surbrillance) |>
  addLegend(data = arr_wgs, pal = pal_airbnb, values = ~airbnb_par_km2,
            title = "Annonces Airbnb / km²", group = "Annonces Airbnb",
            position = "bottomright", opacity = 0.8,
            labFormat = labelFormat(big.mark = " ", digits = 0)) |>

  # --- Couche de points : les stations Vélib' (gris foncé, au-dessus) ---
  addCircleMarkers(data = velib, group = "Stations Vélib'",
                   radius = ~sqrt(capacity) * 0.8, stroke = FALSE,
                   fillColor = "#2F4F4F", fillOpacity = 0.6,
                   label = ~name,
                   popup = ~paste0("<b>", name, "</b><br>",
                                   numbikesavailable, " vélos disponibles / ",
                                   capacity, " bornes")) |>

  # --- Le panneau de contrôle des couches (en haut à droite) ---
  #     baseGroups    : boutons radio, un seul fond de carte à la fois
  #     overlayGroups : cases à cocher, on superpose les couches de données
  addLayersControl(
    baseGroups    = c("OSM gris", "Plan IGN", "Photographies aériennes IGN"),
    overlayGroups = c("Revenu médian", "Annonces Airbnb", "Stations Vélib'"),
    options = layersControlOptions(collapsed = FALSE)   # panneau toujours ouvert
  ) |>

  # --- Au départ, seul le revenu médian est affiché ---
  hideGroup(c("Annonces Airbnb", "Stations Vélib'")) |>

  # --- Barre d'échelle en bas à gauche ---
  addScaleBar(position = "bottomleft")

carte_paris

# -- Enregistrer la carte

# selfcontained = TRUE : un seul fichier HTML, qui contient tout (sauf les
# tuiles, chargées depuis Internet à l'ouverture). Il s'ouvre dans n'importe
# quel navigateur, sans R, et peut être envoyé par e-mail.
# Décommentez les lignes ci-dessous (enlevez les #) pour enregistrer la carte :
# saveWidget(carte_paris, "carte_paris.html",
#            selfcontained = TRUE, title = "Paris : revenus, Airbnb et Vélib'")


# ---- À discuter --------------------------------------------------------------
#
# - Les annonces Airbnb sont-elles concentrées dans les arrondissements les
#   plus aisés ? Qu'est-ce qui pourrait expliquer leur géographie ?
# - Les stations Vélib' sont-elles réparties de façon homogène ? Où sont les
#   plus grandes stations ?
# - Avec seulement 20 arrondissements, une carte interactive apporte-t-elle
#   beaucoup par rapport à une carte statique ?
