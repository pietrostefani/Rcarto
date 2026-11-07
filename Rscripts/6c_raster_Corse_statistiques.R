# =============================================================================
# RCarto — Raster : explorer les valeurs d'un MNT et choisir son agrégation
# Exemple : la Corse
# Dr. Elisabetta Pietrostefani
#
# Avant de cartographier un raster, il faut REGARDER SES VALEURS :
#   1. inspecter le raster (résolution, nombre de cellules, valeurs manquantes)
#   2. explorer la distribution des altitudes (statistiques, histogramme)
#   3. tester plusieurs agrégations de pixels : que gagne-t-on, que perd-on ?
#   4. agréger le raster sur les communes : quelle statistique choisir ?
#
# Avant de commencer : ouvrez le projet (fichier .Rproj). Tous les chemins
# commencent par "data/...". Exécutez le script section par section.
#
# install.packages(c("terra", "sf", "dplyr", "ggplot2", "tidyterra",
#                    "exactextractr", "patchwork"))   # si besoin
# =============================================================================

library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyterra)       # geom_spatraster() pour ggplot2
library(exactextractr)   # statistiques zonales (raster -> polygones)
library(patchwork)       # assembler plusieurs graphiques

terraOptions(progress = 0)   # pas de barre de progression


# ---- 1. Charger et inspecter le raster ------------------------------------

dem <- rast("data/France/Corse/Corse_tout_lambert.tif")
dem                 # dimensions, résolution, étendue, SCR

res(dem)            # taille d'un pixel, en mètres (Lambert 93)
ncell(dem)          # nombre de cellules : plus de 57 millions !

# Les communes corses, pour découper le MNT sur la terre ferme
communes <- read_sf("data/France/gadm41_FRA_shp/gadm41_FRA_5.shp") |>
  filter(NAME_1 == "Corse") |>
  st_transform(crs(dem))

dem <- crop(dem, vect(communes)) |>
  mask(vect(communes))   # les pixels en mer deviennent NA

# Combien de pixels gardent une valeur ? Quelle surface représentent-ils ?
n_valides <- global(dem, "notNA")[1, 1]
n_valides
n_valides * prod(res(dem)) / 1e6   # surface en km² (≈ 8 700 km²)


# ---- 2. Explorer les valeurs ----------------------------------------------

# 2a. Statistiques globales : calculées sur TOUS les pixels
global(dem, c("mean", "sd", "min", "max"), na.rm = TRUE)

# 2b. Quantiles et histogramme : on travaille sur un ÉCHANTILLON de pixels.
#     Avec des millions de cellules, 100 000 pixels tirés au hasard suffisent
#     à décrire la distribution, et c'est beaucoup plus rapide.
set.seed(1)
ech <- spatSample(dem, size = 100000, method = "random", na.rm = TRUE)
names(ech) <- "altitude"

quantile(ech$altitude, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))

ggplot(ech, aes(x = altitude)) +
  geom_histogram(binwidth = 50, fill = "#8C6D46", color = "white") +
  geom_vline(xintercept = median(ech$altitude), linetype = "dashed") +
  labs(title = "Distribution des altitudes — Corse",
       subtitle = "Échantillon de 100 000 pixels ; tirets = médiane",
       x = "Altitude (m)", y = "Nombre de pixels") +
  theme_minimal()

# Questions :
# - La distribution est-elle symétrique ? Pourquoi la moyenne est-elle
#   supérieure à la médiane ?
# - Quelle part de l'île se trouve au-dessus de 1 000 m ?
mean(ech$altitude > 1000)

# 2c. Où sont ces valeurs ? Une carte rapide, sur une version allégée
#     (les cartes ggplot de millions de pixels sont lentes)
dem_apercu <- aggregate(dem, fact = 10, fun = "mean")

ggplot() +
  geom_spatraster(data = dem_apercu) +
  scale_fill_hypso_c(palette = "etopo1_hypso", na.value = NA,
                     name = "Altitude (m)") +
  labs(title = "Relief de la Corse") +
  theme_void()


# ---- 3. Choisir une agrégation de pixels ---------------------------------
#
# aggregate(fact = k) regroupe k × k pixels en un seul. Deux choix à faire :
#   - le FACTEUR : plus il est grand, plus le raster est léger... et grossier
#   - la FONCTION : comment résumer les k × k valeurs en une seule ?
#       "mean"  : altitude moyenne (le choix habituel pour un relief)
#       "max"   : le point le plus haut (sommets, zones inondables...)
#       "modal" : la valeur la plus fréquente (pour un raster de CATÉGORIES)

# 3a. On compare plusieurs facteurs avec la fonction "mean"
facteurs <- c(1, 3, 9, 27)

comparaison <- lapply(facteurs, function(k) {
  r <- if (k == 1) dem else aggregate(dem, fact = k, fun = "mean", na.rm = TRUE)
  stats <- global(r, c("mean", "sd", "max"), na.rm = TRUE)
  data.frame(facteur     = k,
             resolution  = round(res(r)[1]),            # taille du pixel (m)
             nb_cellules = global(r, "notNA")[1, 1],
             moyenne     = round(stats$mean),
             ecart_type  = round(stats$sd),
             maximum     = round(stats$max))
}) |> bind_rows()

comparaison

# À observer dans le tableau :
# - le nombre de cellules est divisé par k² : 9 fois moins avec fact = 3,
#   729 fois moins avec fact = 27 ;
# - la MOYENNE bouge peu : l'agrégation conserve le niveau général
#   (elle baisse un peu car les pixels du littoral se mélangent avec la mer) ;
# - le MAXIMUM baisse nettement : on lisse les sommets. Le point le plus haut
#   passe de 2 664 m à environ 2 430 m avec des pixels de 740 m ;
# - l'ÉCART-TYPE de l'île change à peine : à cette échelle, la variation
#   oppose surtout la côte et la montagne, et elle survit à l'agrégation.
#   Ce qu'on perd, ce sont les contrastes À L'INTÉRIEUR des gros pixels.

# 3b. La même agrégation (fact = 27, pixels d'environ 740 m) avec deux
#     fonctions différentes
dem_mean <- aggregate(dem, fact = 27, fun = "mean", na.rm = TRUE)
dem_max  <- aggregate(dem, fact = 27, fun = "max",  na.rm = TRUE)

global(c(dem_mean, dem_max), "max", na.rm = TRUE)

carte_agg <- function(r, titre) {
  ggplot() +
    geom_spatraster(data = r) +
    scale_fill_hypso_c(palette = "etopo1_hypso", na.value = NA,
                       limits = c(0, 2700), name = "Altitude (m)") +
    labs(title = titre) +
    theme_void()
}

carte_agg(dem_apercu, "fact = 10, moyenne (~270 m)") +
  carte_agg(dem_mean, "fact = 27, moyenne (~740 m)") +
  carte_agg(dem_max,  "fact = 27, maximum (~740 m)") +
  plot_layout(guides = "collect")

# 3c. Quel facteur choisir ? Il dépend de la QUESTION et de l'ÉCHELLE :
# - une carte de toute l'île : fact = 9 ou 10 suffit, l'œil ne voit pas mieux ;
# - un calcul de PENTE : agréger lisse le relief et sous-estime les pentes,
#   gardez une résolution fine ;
# - un résumé par commune (section 4) : on peut agréger, à condition que
#   chaque commune garde assez de pixels.


# ---- 4. Agréger le raster sur les communes --------------------------------
#
# exact_extract() calcule une statistique par polygone, en tenant compte de
# la part de chaque pixel réellement couverte par la commune.
# Pour aller vite, on travaille sur le MNT agrégé par 3 (~80 m).

dem_80 <- aggregate(dem, fact = 3, fun = "mean", na.rm = TRUE)

stats_com <- exact_extract(dem_80, communes,
                           c("count", "mean", "median", "min", "max", "stdev"),
                           progress = FALSE)

communes <- bind_cols(communes, stats_com) |>
  mutate(denivele = max - min)   # écart entre le point le plus bas et le plus haut

# 4a. Combien de pixels par commune ? Une statistique sur trop peu de pixels
#     est fragile : vérifiez avant de choisir votre résolution.
summary(communes$count)

# 4b. Moyenne ou médiane ? Comparez-les commune par commune.
ggplot(communes, aes(x = median, y = mean)) +
  geom_abline(linetype = "dashed", color = "grey50") +
  geom_point(alpha = 0.6, color = "#8C6D46") +
  labs(title = "Altitude moyenne et médiane par commune",
       subtitle = "Sur la diagonale, les deux statistiques sont égales",
       x = "Médiane (m)", y = "Moyenne (m)") +
  theme_minimal()

# 4c. Trois façons de résumer le relief d'une commune
carte_com <- function(var, titre) {
  ggplot(communes) +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.05) +
    scale_fill_viridis_c(option = "magma", direction = -1, name = "m") +
    labs(title = titre) +
    theme_void()
}

carte_com("mean",     "Altitude moyenne") +
  carte_com("max",      "Altitude maximale") +
  carte_com("denivele", "Dénivelé (max − min)")

# Les communes de montagne ont une altitude moyenne élevée, mais aussi un
# fort dénivelé : la moyenne seule cache cette hétérogénéité.

# Les communes au relief le plus contrasté
communes |>
  st_drop_geometry() |>
  select(NAME_5, mean, min, max, denivele) |>
  arrange(desc(denivele)) |>
  head()


# ---- À vous de jouer -------------------------------------------------------
#
# 1. Refaites la section 4 avec dem (pixels de 27 m) puis avec dem_mean
#    (pixels de 740 m). Les moyennes par commune changent-elles ? Et les
#    maximums ? Que se passe-t-il pour les plus petites communes ?
# 2. Ajoutez la PENTE (terrain(dem_80, v = "slope", unit = "degrees")) et
#    calculez la pente moyenne par commune. Quelle carte vous semble la plus
#    utile pour décrire la "montagne" corse : altitude ou pente ?
# 3. Quelle statistique utiliseriez-vous pour repérer les communes exposées
#    aux inondations ? Et pour un indice d'enneigement ?
