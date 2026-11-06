# ============================================================
# RCarto — Cartes interactives avec Leaflet
# Dr. Elisabetta Pietrostefani
# ============================================================
# Adaptation en R du notebook Python "Interactive Maps" (G. Filomena),
# inspiré du cours AutoGIS (Université d'Helsinki).
#   - fonds de carte ouverts (OpenStreetMap, IGN), sans clé d'API
#   - marqueurs, couches de points, choroplèthe interactive
#   - données d'API (vélos en libre-service de Londres)
# Les cartes s'affichent dans l'onglet "Viewer" de RStudio.
#
# install.packages(c("leaflet", "jsonlite", "htmlwidgets")) si besoin
#
# Avant de commencer : ouvrez le projet (fichier .Rproj). Tous les
# chemins commencent par "data/...". Pour vérifier : getwd()
# Exécutez le script section par section (Ctrl/Cmd + Entrée).
# ============================================================

# Ce chapitre adapte en R le notebook Python Interactive Maps de Gabriele
# Filomena (Geographic Data Science Lab, Université de Liverpool), lui-même
# inspiré du cours AutoGIS de l'Université d'Helsinki (H. Tenkanen, V.
# Heikinheimo, H. W. Aagesen, C. Fink, K. Hasanzadeh).


# ---- Introduction ----------------------------------------------------------
# Les cartes en ligne sont interactives depuis longtemps : on peut zoomer, se
# déplacer, cliquer sur un objet pour obtenir des informations. Ces
# fonctionnalités reposent sur JavaScript, le langage des pages web. Parmi les
# bibliothèques JavaScript libres de cartographie web, la plus utilisée est
# Leaflet, légère, rapide et très bien documentée.
# Nous n'écrirons pas une seule ligne de JavaScript. Le package R leaflet
# (l'équivalent de folium en Python) traduit notre code R en une carte
# Leaflet, à partir d'objets sf que vous connaissez déjà.
# Une carte leaflet se construit couche par couche, avec l'opérateur |> (ou
# %>%), un peu comme ggplot2 avec + :

library(leaflet)     # cartes interactives
library(sf)          # données vectorielles
library(dplyr)       # manipulation de données
library(readr)       # lecture de CSV
library(jsonlite)    # lecture de JSON (API)
library(htmlwidgets) # enregistrer une carte en HTML


# install.packages(c("leaflet", "jsonlite", "htmlwidgets"))


# ---- Une première carte ----------------------------------------------------
# On crée une carte vide avec leaflet(), on ajoute un fond de carte avec
# addTiles() (par défaut, OpenStreetMap), puis on la centre avec setView() :
# longitude, latitude et niveau de zoom (de 0, le monde entier, à environ 19,
# la rue).

marseille <- c(lng = 5.3698, lat = 43.2965)   # Vieux-Port de Marseille

carte <- leaflet() |>
  addTiles() |>
  setView(lng = marseille["lng"], lat = marseille["lat"], zoom = 13) |>
  addScaleBar(position = "bottomleft")        # barre d'échelle

carte


# >> Attention à l'ordre : longitude, puis latitude
# leaflet attend lng puis lat (x puis y), comme sf. Le Python folium fait
# l'inverse (location = (lat, lng)). C'est une source d'erreurs fréquente : si
# votre carte s'affiche au milieu de l'océan, inversez les coordonnées.

# -- Enregistrer la carte
# Une carte leaflet est un widget HTML. On l'enregistre dans un fichier que
# l'on peut ouvrir dans n'importe quel navigateur, ou envoyer par e-mail :

# saveWidget(carte, "carte_marseille.html", selfcontained = TRUE)


# ---- Les fonds de carte ----------------------------------------------------
# Un fond de carte web est découpé en tuiles (tiles) : de petites images
# carrées, indexées par trois nombres X, Y et Z. Z est le niveau de zoom : à Z
# = 0, une seule tuile couvre la Terre entière ; à chaque niveau
# supplémentaire, chaque tuile est divisée en quatre. X et Y repèrent la tuile
# dans cette grille. Le navigateur ne télécharge que les tuiles visibles à
# l'écran, ce qui rend la navigation fluide.
# Avec addProviderTiles(), on choisit un fournisseur de tuiles par son nom. La
# liste complète des fournisseurs connus de leaflet se trouve dans l'objet
# providers ; on peut les prévisualiser sur leaflet-providers.

leaflet() |>
  addProviderTiles(providers$OpenStreetMap.HOT) |>
  setView(lng = marseille["lng"], lat = marseille["lat"], zoom = 12)


# -- Un fond gris minimaliste
# Sous une carte choroplèthe ou une couche de points, le fond de carte doit
# s'effacer : un fond gris et peu chargé laisse toute la place aux couleurs
# des données. Les fonds gris les plus connus (CARTO Positron, Stamen Toner
# Lite, Esri Light Gray) exigent désormais une clé d'API ou ne sont pas
# ouverts.
# La solution : garder les tuiles OpenStreetMap, libres et sans clé, et les
# passer en niveaux de gris avec un filtre CSS (le langage de mise en forme
# des pages web). On écrit une petite fonction que l'on réutilisera dans tout
# ce chapitre :

# Un fond OpenStreetMap en niveaux de gris : on ajoute une classe CSS aux
# tuiles, puis un filtre CSS qui les passe en gris (voir cartes_interactives.qmd)
ajouter_osm_gris <- function(carte, group = NULL) {
  carte |>
    addTiles(group = group, options = tileOptions(className = "osm-gris")) |>
    htmlwidgets::prependContent(htmltools::tags$style(
      ".osm-gris { filter: grayscale(100%) brightness(1.05) contrast(0.85); }"))
}

leaflet() |>
  ajouter_osm_gris() |>
  setView(lng = marseille["lng"], lat = marseille["lat"], zoom = 13)

# Comment ça marche ?
#   - tileOptions(className = "osm-gris") donne une étiquette (une classe CSS)
#     aux tuiles de ce fond de carte, et à lui seul ;
#   - prependContent() ajoute à la carte une règle CSS qui applique à cette
#     classe un filtre grayscale(100%) (tout en gris), un peu plus de
#     luminosité et un peu moins de contraste ;
#   - les données (polygones, cercles) ne portent pas cette classe : elles
#     gardent leurs couleurs.
# Le filtre fait partie de la carte : il fonctionne aussi dans le fichier HTML
# enregistré avec saveWidget().
# On peut aussi indiquer directement l'adresse (URL) d'un service de tuiles,
# avec ses trois paramètres {z}, {x} et {y}. Par exemple, le Plan IGN de
# l'Institut national de l'information géographique et forestière, en accès
# libre :

# Adresse des tuiles IGN (Géoplateforme) : seul le nom de la couche et le format changent
url_ign <- function(couche, format = "image/png") {
  paste0("https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0",
         "&STYLE=normal&TILEMATRIXSET=PM&FORMAT=", format,
         "&LAYER=", couche,
         "&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}")
}

url_plan_ign  <- url_ign("GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2")                 # Plan IGN
url_ortho_ign <- url_ign("ORTHOIMAGERY.ORTHOPHOTOS", format = "image/jpeg")   # photographies aériennes

leaflet() |>
  addTiles(urlTemplate = url_plan_ign,
           attribution = "© IGN – Géoplateforme") |>
  setView(lng = marseille["lng"], lat = marseille["lat"], zoom = 13)


# -- Fonds de carte libres et ouverts
# Dans ce cours, nous n'utilisons que des fonds de carte issus de données
# ouvertes (OpenStreetMap, IGN, NASA) et accessibles sans clé d'API ni compte.
# Pensez toujours à citer la source : leaflet ajoute automatiquement
# l'attribution en bas à droite de la carte.
#   Nom dans providers | Données | Usage conseillé
#   OpenStreetMap.Mapnik | OpenStreetMap | Le fond OSM standard, très détaillé
#   OpenStreetMap.France | OpenStreetMap (OSM France) | Rendu « à la française », libellés en français
#   OpenStreetMap.HOT | OpenStreetMap (Humanitarian OSM Team) | Cartographie humanitaire, pays du Sud
#   ajouter_osm_gris() (notre fonction) | OpenStreetMap en niveaux de gris | Fond minimaliste, idéal sous une choroplèthe ou des points
#   OpenTopoMap | OpenStreetMap + SRTM | Carte topographique avec courbes de niveau
#   CyclOSM | OpenStreetMap | Pistes cyclables et réseau vélo
#   addTiles(urlTemplate = url_plan_ign) | IGN | Plan IGN, la référence pour la France
#   addTiles(urlTemplate = url_ortho_ign) | IGN | Photographies aériennes (orthophotos) de la France
#   NASAGIBS.ViirsEarthAtNight2012 | NASA | Lumières nocturnes (image de 2012) ; le serveur de la NASA ne répond pas toujours

# >> Fonds de carte à éviter
#   - Google Maps : on trouve en ligne des URL de tuiles Google
#     (mt1.google.com/...), mais leurs conditions d'utilisation interdisent de
#     les utiliser directement dans une carte Leaflet.
#   - CARTO (CartoDB.Positron, CartoDB.DarkMatter, CartoDB.Voyager) : encore
#     présents dans providers et dans beaucoup de tutoriels, mais leurs fonds
#     exigent désormais une clé d'API.
#   - Mapbox, Stadia (anciennement Stamen), Thunderforest, MapTiler : ils
#     exigent une clé d'API personnelle et un compte, avec des conditions qui
#     changent souvent.
#   - Esri (Esri.WorldImagery...) : gratuits à l'affichage, mais ni ouverts,
#     ni libres.
# Pour l'IGN, on n'utilise pas providers$GeoportailFrance : selon la version
# de leaflet installée, ce raccourci pointe encore vers l'ancien service de
# l'IGN (wxs.ign.fr), fermé en 2024, et la carte reste vide. On donne
# directement l'adresse du nouveau service, la Géoplateforme (data.geopf.fr),
# comme ci-dessus.

# -- Comparer plusieurs fonds de carte
# Avec group et addLayersControl(), l'utilisateur choisit lui-même son fond de
# carte (boutons radio en haut à droite) :

leaflet() |>
  ajouter_osm_gris(group = "OSM gris") |>
  addProviderTiles(providers$OpenStreetMap.France,   group = "OSM France") |>
  addProviderTiles(providers$OpenTopoMap,            group = "OpenTopoMap") |>
  addTiles(urlTemplate = url_ortho_ign, attribution = "© IGN – Géoplateforme",
           group = "Orthophotos IGN") |>
  setView(lng = marseille["lng"], lat = marseille["lat"], zoom = 12) |>
  addLayersControl(
    baseGroups = c("OSM gris", "OSM France", "OpenTopoMap", "Orthophotos IGN"),
    options = layersControlOptions(collapsed = FALSE)
  )


# ---- Des marqueurs ---------------------------------------------------------
# addMarkers() ajoute des marqueurs. Deux paramètres sont essentiels :
#   - label : le texte qui s'affiche au survol de la souris (une info-bulle) ;
#   - popup : le texte qui s'affiche au clic.

leaflet() |>
  addProviderTiles(providers$OpenStreetMap.France) |>
  setView(lng = marseille["lng"], lat = marseille["lat"], zoom = 13) |>
  addAwesomeMarkers(
    lng = marseille["lng"], lat = marseille["lat"],
    label = "Vieux-Port",
    popup = "<b>Vieux-Port de Marseille</b><br>Le port historique de la ville",
    icon = awesomeIcons(icon = "ok-sign", markerColor = "green")
  )

# addAwesomeMarkers() permet de choisir l'icône et la couleur du marqueur. Le
# popup accepte du HTML : <b> pour le gras, <br> pour un retour à la ligne.

# -- Plusieurs marqueurs à partir d'un tableau
# En pratique, les coordonnées sont dans un tableau. On passe le tableau à
# leaflet(), puis on désigne les colonnes avec un tilde ~ :

personnes <- data.frame(
  nom = c("Tom", "Kendall", "Sean", "Zachary", "Karla"),
  lng = c(-3.2031323, -0.2416811, -3.4924087, -4.3725404, -2.6607571),
  lat = c(53.4118332, 51.5285582, 55.940874, 55.8553807, 51.4684681)
)

leaflet(personnes) |>
  ajouter_osm_gris() |>
  addAwesomeMarkers(lng = ~lng, lat = ~lat, popup = ~nom, label = ~nom,
                    icon = awesomeIcons(icon = "user", markerColor = "orange"))


# >> addMarkers() ou addAwesomeMarkers() ?
# addMarkers() utilise l'épingle bleue standard de Leaflet. Si elle ne
# s'affiche pas (cela arrive dans certains documents Quarto, par exemple avec
# l'option embed-resources: true), préférez addAwesomeMarkers() ou
# addCircleMarkers().
# Sans setView(), leaflet cadre automatiquement la carte sur les données.


# ---- Une couche de points --------------------------------------------------
# Avec un objet sf, leaflet trouve tout seul les coordonnées : plus besoin de
# lng et lat. Chargeons les stations de métro de Londres.

metro <- read_sf("data/London/Points/tube_stations.shp")
st_crs(metro)$input


# >> Les cartes web sont en WGS 84
# Les cartes web travaillent en longitude / latitude (WGS 84, EPSG:4326). Nos
# stations sont en British National Grid (en mètres) : il faut les reprojeter
# avec st_transform() avant de les cartographier, sinon rien ne s'affiche (ou
# leaflet vous avertit).

metro <- st_transform(metro, 4326)

leaflet(metro) |>
  ajouter_osm_gris() |>
  addCircleMarkers(
    radius = 4, stroke = FALSE,
    fillColor = "#DC241F", fillOpacity = 0.8,   # le rouge du métro londonien
    label = ~STATION
  )

# addCircleMarkers() dessine des cercles de taille fixe à l'écran, plus
# lisibles que des épingles quand les points sont nombreux.


# ---- Une carte choroplèthe interactive -------------------------------------
# Reprenons la densité de population des départements français, calculée dans
# le chapitre Choroplèthes. En Python, folium demande plusieurs couches
# superposées pour obtenir à la fois les couleurs, l'info-bulle et la
# surbrillance au survol. Avec leaflet, tout se fait dans un seul
# addPolygons().

dep <- read_sf("data/France/gadm41_FRA_shp/gadm41_FRA_2.shp")
pop <- read_delim("data/France/pop2022/donnees_departements.csv", delim = ";",
                  col_types = cols(DEP = col_character()))

dep <- dep |>
  left_join(pop, by = c("CC_2" = "DEP")) |>
  st_transform(2154) |>
  mutate(surface_km2 = as.numeric(st_area(geometry)) / 1e6,
         densite     = round(PMUN / surface_km2)) |>
  st_simplify(dTolerance = 500, preserveTopology = TRUE) |>   # contours simplifiés à 500 m
  st_transform(4326)                                         # retour en WGS 84 pour le web


# >> Pourquoi simplifier les contours ?
# Une carte leaflet embarque toutes les coordonnées des polygones dans la page
# web. Les contours GADM sont très détaillés : sans simplification, la page
# pèse plusieurs dizaines de Mo et devient lente. st_simplify() retire les
# sommets superflus (ici, les détails de moins de 500 m), invisibles à
# l'échelle de la France. On simplifie en Lambert 93 pour que la tolérance
# soit en mètres.

# -- Les couleurs : une palette et des classes
# Les fonctions colorBin(), colorQuantile() et colorNumeric() de leaflet
# créent une fonction qui transforme une valeur en couleur :
#   - colorNumeric() : un dégradé continu ;
#   - colorQuantile() : des classes de même effectif (quantiles) ;
#   - colorBin() : des classes dont on fixe les bornes.
# Dans le chapitre sur les choroplèthes, nous avons vu que les classes de
# Fisher-Jenks laissent ici presque tous les départements dans la première
# classe. On réutilise donc les bornes arrondies choisies à la main :

bornes <- c(0, 50, 100, 250, 1000, 5000, 25000)

pal_densite <- colorBin(palette = "YlOrRd", domain = dep$densite,
                        bins = bornes)

pal_densite(c(30, 500, 20000))   # la palette renvoie des couleurs


# -- La carte

# Info-bulle au survol : du HTML, une ligne par département
etiquettes <- sprintf("<strong>%s</strong><br/>%s hab./km²",
                      dep$NAME_2, format(dep$densite, big.mark = " ")) |>
  lapply(htmltools::HTML)

leaflet(dep) |>
  ajouter_osm_gris() |>
  addPolygons(
    fillColor   = ~pal_densite(densite),
    fillOpacity = 0.8,
    color       = "white", weight = 0.5,          # contours
    label       = etiquettes,                     # au survol
    popup       = ~paste0("<b>", NAME_2, "</b><br>",
                          "Population : ", format(PMUN, big.mark = " "), "<br>",
                          "Surface : ", round(surface_km2), " km²"),  # au clic
    highlightOptions = highlightOptions(          # surbrillance au survol
      weight = 2, color = "#333333", fillOpacity = 1, bringToFront = TRUE)
  ) |>
  addLegend(pal = pal_densite, values = ~densite, opacity = 0.8,
            title = "Habitants / km²", position = "bottomright",
            labFormat = labelFormat(big.mark = " ", digits = 0))

# Survolez un département : il s'éclaire et son nom s'affiche. Cliquez : la
# fenêtre (popup) donne sa population et sa surface. Grâce au fond gris,
# seules les couleurs des données attirent l'œil.


# ---- Cartes interactives et API --------------------------------------------
# Les cartes interactives sont souvent alimentées par des API (*Application
# Programming Interfaces*) : des services web qui renvoient des données à
# jour, généralement au format JSON. Transport for London publie ainsi en
# temps réel l'état de toutes les stations de vélos en libre-service de
# Londres.

# # Requête en direct à l'API de Transport for London
# velos <- fromJSON("https://api.tfl.gov.uk/BikePoint/")

# Pour que ce chapitre fonctionne hors connexion, nous utilisons une copie de
# cette réponse enregistrée dans data/London/Points/BikePoint.json. Le code
# est identique :

velos <- fromJSON("data/London/Points/BikePoint.json")
nrow(velos)
names(velos)

# Le nombre de vélos et de bornes de chaque station est rangé dans
# additionalProperties, une liste de tableaux (un par station). On en extrait
# les valeurs utiles :

# Pour une station : lire la valeur associée à une clé
lire_prop <- function(props, cle) as.numeric(props$value[props$key == cle])

velos <- velos |>
  mutate(
    nb_bornes = sapply(additionalProperties, lire_prop, cle = "NbDocks"),
    nb_velos  = sapply(additionalProperties, lire_prop, cle = "NbBikes")
  ) |>
  select(commonName, lat, lon, nb_bornes, nb_velos) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)   # un objet sf

head(velos)

# Avec près de 800 stations, une carte de points devient vite illisible à
# petite échelle. clusterOptions regroupe les points proches en grappes, qui
# s'ouvrent quand on zoome :

leaflet(velos) |>
  ajouter_osm_gris() |>
  addCircleMarkers(
    radius = ~sqrt(nb_bornes) * 1.5,             # taille selon le nombre de bornes
    stroke = FALSE, fillColor = "#1F78B4", fillOpacity = 0.8,
    label  = ~commonName,
    popup  = ~paste0("<b>", commonName, "</b><br>",
                     nb_velos, " vélos disponibles / ", nb_bornes, " bornes"),
    clusterOptions = markerClusterOptions()
  )


# >> Pourquoi sqrt() pour la taille ?
# Le paramètre radius fixe le rayon du cercle, mais l'œil compare des
# surfaces. Prendre la racine carrée rend la surface du cercle proportionnelle
# au nombre de bornes : c'est la règle des symboles proportionnels.

# trajets <- read_csv("data/London/Tables/JourneyDataExtract01Aug2018-07Aug2018.csv") |>
#   rename(depart = `StartStation Id`, arrivee = `EndStation Id`, duree = Duration) |>
#   filter(depart != arrivee, duree > 0, duree <= 180 * 60) |>
#   count(depart, arrivee, name = "n_trajets") |>
#   filter(n_trajets >= quantile(n_trajets, 0.98))       # les 2 % de liaisons les plus fréquentes
#
# # coordonnées des stations (identifiant numérique tiré de "BikePoints_308")
# stations <- fromJSON("data/London/Points/BikePoint.json") |>
#   transmute(id = as.numeric(sub("BikePoints_", "", id)), lon, lat)
#
# lignes <- trajets |>
#   inner_join(stations, by = c("depart" = "id")) |>
#   inner_join(stations, by = c("arrivee" = "id"), suffix = c("_o", "_d"))
#
# leaflet(lignes) |>
#   ajouter_osm_gris() |>
#   addPolylines(
#     lng = ~cbind(lon_o, lon_d), lat = ~cbind(lat_o, lat_d),   # une ligne par liaison
#     weight = ~n_trajets / max(n_trajets) * 6,
#     color = "#D95F02", opacity = 0.6
#   )


# ---- Exercice --------------------------------------------------------------

# >> À vous de jouer
# Choisissez une API de données ouvertes (par exemple Vélib' Métropole,
# data.gouv.fr, Transport for London ou OpenStreetMap / Overpass) et lisez sa
# documentation.
#   - Imaginez deux cartes interactives que vous pourriez construire à partir
#     de ces données.
#   - Pour chacune, choisissez un fond de carte dans le tableau des fonds
#     libres (la section correspondante) et justifiez votre choix.
#   - Quelles données afficheriez-vous au survol (label), et lesquelles au
#     clic (popup) ?


# ---- D'autres packages pour la cartographie interactive en R ---------------
# leaflet offre un très grand contrôle sur la carte. D'autres packages
# permettent d'aller plus vite :
#   - mapview : mapview(dep, zcol = "densite") produit en une ligne une carte
#     interactive pour explorer des données ;
#   - tmap : avec tmap_mode("view"), la même syntaxe produit une carte
#     statique ou interactive ;
#   - leaflet.extras et leafem : des extensions (recherche d'adresse, cartes
#     de chaleur, affichage des coordonnées...) ;
#   - mapgl : des cartes en WebGL (MapLibre), adaptées aux très grands jeux de
#     données.
