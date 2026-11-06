# ============================================================
# RCarto — Lissage et Interpolation Spatiale
# Dr. Elisabetta Pietrostefani
# ============================================================
# Ce script couvre deux méthodes complémentaires :
#
#   IDW (Pondération Inverse à la Distance)
#     Question posée : quelle valeur prédit-on en ce point ?
#     Usage : prix immobiliers, pollution, température
#
#   Heatmap (Carte de chaleur par noyau gaussien)
#     Question posée : où les points sont-ils concentrés ?
#     Usage : densité d'annonces, accidents, commerces
#
# Les deux méthodes sont d'abord démontrées sur Londres,
# puis à reproduire sur les données Airbnb de Paris.
# ============================================================

library(sf)
library(gstat)    # interpolation IDW
library(ggplot2)
library(dplyr)
library(scales)

# ============================================================
# PARTIE 1 — INTERPOLATION IDW (Londres)
# ============================================================
# Principe mathématique :
# L'IDW prédit la valeur en tout point comme une moyenne pondérée
# des observations voisines. Le poids w_j décroît avec la distance :
#
#   ẑ(s₀) = Σ(w_j × Z_j) / Σ(w_j)   avec   w_j = 1 / d(s₀, s_j)^p
#
# Paramètres clés :
#   p (idp)   : puissance — plus elle est grande, plus les points
#               lointains sont ignorés (surface plus "piquée")
#   N (nmax)  : nombre de voisins pris en compte
#   cellsize  : résolution de la grille de prédiction

# ---- Charger les données ----------------------------------
districts <- read_sf("data/London/Polygons/districts.shp")

housesales_sf <- read.csv("data/London/Tables/housesales.csv") |>
  st_as_sf(coords = c(17, 18)) |>
  st_set_crs(27700)

cat("Nb transactions :", nrow(housesales_sf), "\n")

# Visualiser les prix bruts avant interpolation
ggplot() +
  geom_sf(data = districts, fill = "grey92", color = "white", linewidth = 0.3) +
  geom_sf(data = housesales_sf, aes(color = price),
          size = 0.4, alpha = 0.5) +
  scale_color_viridis_c(option    = "plasma", direction = -1,
                         name      = "Prix (£)",
                         labels    = label_comma(prefix = "£")) +
  labs(title   = "Transactions immobilières — Londres",
       caption = "Source : données Londres | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ---- Appliquer l'IDW (code fourni — lisez attentivement) --
# gstat() configure le modèle IDW :
#   formula = price ~ 1 : interpoler "price" sans covariable
#   nmax    : nombre maximum de voisins à considérer
#   set = list(idp = 2) : puissance de pondération (p = 2 par défaut)

housesales_sp <- sf::as_Spatial(housesales_sf)   # gstat nécessite un objet Spatial

gs <- gstat(
  formula = price ~ 1,
  data    = housesales_sp,
  nmax    = 50,
  set     = list(idp = 2)
)

# Grille de prédiction à 500 m — uniquement sur l'emprise de Londres
# st_make_grid() crée une grille régulière de points
# st_filter() garde uniquement les points qui tombent dans les districts
grid_sf <- st_make_grid(districts, cellsize = 500, what = "centers") |>
  st_as_sf() |>
  st_filter(districts)

# predict() applique le modèle IDW à chaque point de la grille
# var1.pred contient les valeurs prédites → renommée "pred"
idw_sf <- predict(gs, sf::as_Spatial(grid_sf)) |>
  st_as_sf() |>
  rename(pred = var1.pred)

ggplot() +
  geom_sf(data = idw_sf, aes(color = pred), size = 1.2, shape = 15) +
  scale_color_viridis_c(option    = "plasma", direction = -1,
                         name      = "Prix interpolé (£)",
                         labels    = label_comma(prefix = "£")) +
  geom_sf(data = districts, fill = NA, color = "white", linewidth = 0.3) +
  labs(title    = "Interpolation IDW — Prix immobiliers à Londres",
       subtitle = "Résolution 500 m | N voisins = 50 | puissance p = 2",
       caption  = "Source : données Londres | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title        = element_text(face = "bold", hjust = 0.5),
        plot.subtitle     = element_text(color = "grey40", hjust = 0.5),
        legend.key.height = unit(1.5, "cm"))

# ============================================================
# EXERCICE 1 — Faire varier les paramètres IDW
# ============================================================
# Testez différentes valeurs et observez l'effet sur la surface :
#
#   nmax = 10   → surface très localisée (forte influence des points isolés)
#   nmax = 100  → surface plus lisse (influence étendue)
#   idp  = 1    → décroissance douce (points lointains influencent encore)
#   idp  = 4    → décroissance rapide (seuls les points proches comptent)
#   cellsize = 250 m → image plus fine (calcul plus long)
#   cellsize = 1000 m → image plus grossière (calcul rapide)
#
# Question : quelle combinaison donne la surface la plus "réaliste" ?

gs_test <- gstat(formula = price ~ 1,
                  data    = sf::as_Spatial(housesales_sf),
                  nmax    = ___,            # testez : 10, 50, 100
                  set     = list(idp = ___)) # testez : 1, 2, 4

grid_test <- st_make_grid(districts, cellsize = ___, what = "centers") |>
  st_as_sf() |> st_filter(districts)

idw_test <- predict(gs_test, sf::as_Spatial(grid_test)) |>
  st_as_sf() |> rename(pred = var1.pred)

ggplot() +
  geom_sf(data = idw_test, aes(color = pred), size = 0.8, shape = 15) +
  scale_color_viridis_c(option = "plasma", direction = -1,
                         labels = label_comma(prefix = "£")) +
  geom_sf(data = districts, fill = NA, color = "white", linewidth = 0.3) +
  labs(title = paste0("IDW — nmax = ___, p = ___, cellsize = ___ m")) +
  theme_void()

# ============================================================
# PARTIE 2 — CARTE DE CHALEUR (Heatmap)
# ============================================================
# Une heatmap répond à une question DIFFÉRENTE de l'IDW :
# non pas "quelle valeur en ce point" mais "combien de points se concentrent ici".
#
# stat_density_2d() de ggplot2 calcule une estimation de densité
# par noyau gaussien 2D directement sur les coordonnées.
# Aucun package supplémentaire nécessaire.
#
# geom = "raster"          : affiche la densité comme une image continue
# contour = FALSE          : désactive les courbes de niveau
# interpolate = TRUE       : lisse l'image entre les pixels
# after_stat(density)      : utilise la densité calculée comme variable fill
#
# Astuce : un fond sombre (fill = "grey15") met en valeur les zones chaudes.

coords <- st_coordinates(housesales_sf) |>
  as.data.frame() |>
  setNames(c("x", "y"))    # st_coordinates() renvoie X et Y — on les renomme

ggplot() +
  geom_sf(data = districts, fill = "grey15", color = "grey35", linewidth = 0.3) +
  stat_density_2d(
    data        = coords,
    aes(x = x, y = y, fill = after_stat(density)),
    geom        = "raster",
    contour     = FALSE,
    interpolate = TRUE,
    alpha       = 0.85
  ) +
  scale_fill_viridis_c(option = "inferno", name = "Densité", guide = "none") +
  geom_sf(data = districts, fill = NA, color = "white", linewidth = 0.3) +
  labs(title    = "Densité des transactions immobilières — Londres",
       subtitle = "Estimation par noyau gaussien",
       caption  = "Source : données Londres | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5, color = "white"),
        plot.subtitle   = element_text(color = "grey60", hjust = 0.5),
        plot.background = element_rect(fill = "grey15", color = NA))

# ============================================================
# EXERCICE 2 — Heatmap des biens les plus chers
# ============================================================
# Filtrez les transactions au-dessus de 500 000 £ et produisez
# une heatmap pour voir où se concentrent les biens haut de gamme.
# Comparez avec la heatmap générale — les zones sont-elles les mêmes ?
#
# Indice : extrayez les coordonnées avec st_coordinates() après filter()

coords_cher <- housesales_sf |>
  filter(price > ___) |>
  st_coordinates() |>
  as.data.frame() |>
  setNames(c("x", "y"))

ggplot() +
  geom_sf(data = districts, fill = "grey15", color = "grey35", linewidth = 0.3) +
  stat_density_2d(
    data        = coords_cher,
    aes(x = x, y = y, fill = after_stat(density)),
    geom        = "raster", contour = FALSE, interpolate = TRUE, alpha = 0.85
  ) +
  scale_fill_viridis_c(option = "___", guide = "none") +  # essayez "magma" ou "plasma"
  geom_sf(data = districts, fill = NA, color = "white", linewidth = 0.3) +
  labs(title = "Transactions > 500 000 £ — Londres") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey15", color = NA),
        plot.title      = element_text(color = "white", face = "bold", hjust = 0.5))

# ============================================================
# EXERCICE — Reproduire sur Paris Airbnb
# ============================================================
# Reproduisez les deux analyses (IDW et heatmap) avec les annonces Airbnb.
#
# PARTIE A — IDW sur le prix Airbnb
#
# 1. Chargez les données :
#    PA <- read_sf("data/Paris/Polygons/arrondiss_lambert.shp") |>
#            st_transform(2154)
#    listings_sf <- read.csv("data/Paris/airbnb/listings.csv") |>
#                     st_as_sf(coords = c(8, 7)) |>    # lon, lat
#                     st_set_crs(4326) |>
#                     st_transform(st_crs(PA))
#
# 2. Filtrez à moins de 200 €/nuit :
#    listings_filtered <- listings_sf |> filter(price < ___)
#
# 3. Appliquez l'IDW (même structure que Londres, cellsize = 250 m)
#
# 4. Cartographiez le prix interpolé
#
# PARTIE B — Heatmap de densité Airbnb
#
# 1. Extrayez les coordonnées de listings_filtered
# 2. Appliquez stat_density_2d() sur fond sombre
# 3. Comparez la densité globale avec celle des annonces > 150 €/nuit

library(patchwork)   # pour afficher deux heatmaps côte à côte

# Corrigé disponible dans : 8b_interpolation_Paris.R
# et dans le chapitre « Interpolation : Paris » sur le site du cours.
