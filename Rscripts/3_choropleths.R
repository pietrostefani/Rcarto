# ============================================================
# RCarto — Choroplèthes
# Dr. Elisabetta Pietrostefani
# ============================================================
# Ce script couvre :
#   - Construction d'une carte choroplèthe avec ggplot2
#   - Les 4 méthodes de classification (classInt)
#   - Comparaison visuelle des méthodes
#   - Choix de la palette de couleurs
#   - Carte complète publiable
#   - Superposition choroplèthe + symboles proportionnels
# ============================================================

library(sf)
library(ggplot2)
library(dplyr)
library(classInt)       # méthodes de classification
library(viridis)        # palettes perceptuellement uniformes
library(RColorBrewer)   # palettes ColorBrewer
library(scales)         # formatage des axes et légendes
library(rnaturalearth)
library(rnaturalearthdata)

# ============================================================
# 1. DONNÉES : RÉGIONS FRANÇAISES MÉTROPOLITAINES
# ============================================================
# ne_states() charge les régions d'un pays depuis Natural Earth.
# filter() exclut les DOM-TOM pour ne garder que la métropole.
# select() ne conserve que les colonnes utiles.

france_regions <- ne_states(country = "France", returnclass = "sf") |>
  filter(!name %in% c("Guyane française", "Martinique",
                       "Guadeloupe", "La Réunion", "Mayotte")) |>
  select(name, region, iso_3166_2, geometry)

glimpse(france_regions)   # aperçu rapide de la structure

# Données socio-économiques simulées (dans un vrai projet : read_csv() + left_join())
# set.seed() fixe le générateur aléatoire pour des résultats reproductibles
set.seed(42)
n <- nrow(france_regions)

france_regions <- france_regions |>
  mutate(
    taux_chomage  = round(runif(n, min = 4.5, max = 18.0), 1),
    revenu_median = round(rnorm(n, mean = 1850, sd = 280), 0),
    densite_pop   = round(exp(rnorm(n, mean = 4.8, sd = 1.2)), 0),
    part_diplomes = round(rnorm(n, mean = 28, sd = 7), 1)
  )

# Extraire un vecteur numérique brut — nécessaire pour classIntervals()
chomage_vec <- france_regions |> st_drop_geometry() |> pull(taux_chomage)

# Fonction utilitaire : rend les labels de légende lisibles (voir introduction.R)
clean_cut_labels <- function(x) {
  levels(x) <- gsub("\\(|\\[|\\]|\\)", "", levels(x)) |>
    gsub(",", " – ", x = _)
  x
}

# ============================================================
# 2. CARTE DE BASE : DEUX SYNTAXES COMPARÉES
# ============================================================

# -- mapsf : syntaxe proche de la cartographie classique
library(mapsf)
mf_map(france_regions, type = "base",
       col    = "#e8f4f8",
       border = "white",
       lwd    = 0.8)
mf_title("Régions françaises — mapsf")
mf_credits("Source : Natural Earth")

# -- ggplot2 : syntaxe que nous utilisons tout au long du cours
# geom_sf() sans aes(fill = ...) donne une carte de base uniforme
ggplot(france_regions) +
  geom_sf(fill = "#e8f4f8", color = "white", linewidth = 0.5) +
  labs(title   = "Régions françaises — ggplot2",
       caption = "Source : Natural Earth | Cours R-Carto") +
  theme_void(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Pourquoi ggplot2 ? Parce que la même grammaire sert pour
# les histogrammes, nuages de points ET les cartes.

# ============================================================
# 3. TOUJOURS EXAMINER LA DISTRIBUTION AVANT DE CLASSIFIER
# ============================================================
# La forme de la distribution détermine quelle méthode choisir.
# after_stat(count) * 1.5 met la courbe de densité à l'échelle
# des comptages de l'histogramme.

ggplot(france_regions, aes(x = taux_chomage)) +
  geom_histogram(binwidth = 1.5, fill = "#2c7bb6", color = "white", alpha = 0.85) +
  geom_density(aes(y = after_stat(count) * 1.5),
               color = "#d7191c", linewidth = 1) +
  labs(title    = "Distribution du taux de chômage par région",
       subtitle = "Données simulées à des fins pédagogiques",
       x = "Taux de chômage (%)", y = "Nombre de régions") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

# ============================================================
# 4. LES 4 MÉTHODES DE CLASSIFICATION
# ============================================================
# classIntervals(vecteur, n = nb_classes, style = "méthode")
# retourne un objet avec $brks (les bornes) et $var (les données)
#
# cut() découpe un vecteur en classes selon des bornes données.
#   include.lowest = TRUE : inclut la valeur minimale dans la première classe
#   dig.lab = 4           : nombre de décimales dans les labels
#   ordered_result = TRUE : crée un facteur ordonné (important pour la légende)

# ---- Méthode 1 : Intervalle égal --------------------------
# Divise la plage totale en n intervalles de largeur identique.
# Avantage : simple à interpréter.
# Inconvénient : si la distribution est asymétrique, certaines classes
# peuvent être quasi-vides (peu d'observations) — carte peu informative.

breaks_equal <- classIntervals(chomage_vec, n = 5, style = "equal")

france_regions <- france_regions |>
  mutate(classe_equal = cut(taux_chomage,
                             breaks         = breaks_equal$brks,
                             include.lowest = TRUE,
                             dig.lab        = 4,
                             ordered_result = TRUE) |> clean_cut_labels())

ggplot(france_regions) +
  geom_sf(aes(fill = classe_equal), color = "white", linewidth = 0.4) +
  scale_fill_brewer(palette = "YlOrRd", name = "Taux de\nchômage (%)") +
  labs(title    = "Intervalle égal (n = 5)",
       subtitle = "Chaque classe couvre un intervalle de valeurs identique",
       caption  = "Données simulées | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5))

cat("Bornes — intervalle égal :", round(breaks_equal$brks, 1), "\n")

# ---- Méthode 2 : Quantiles --------------------------------
# Chaque classe contient le même nombre d'observations.
# Avantage : toutes les classes sont également peuplées — bonne lecture comparative.
# Inconvénient : peut regrouper dans la même classe des valeurs très différentes,
# donnant l'impression d'une variation là où elle est faible.

breaks_quantile <- classIntervals(chomage_vec, n = 5, style = "quantile")

france_regions <- france_regions |>
  mutate(classe_quantile = cut(taux_chomage,
                                breaks         = breaks_quantile$brks,
                                include.lowest = TRUE,
                                dig.lab        = 4,
                                ordered_result = TRUE) |> clean_cut_labels())

ggplot(france_regions) +
  geom_sf(aes(fill = classe_quantile), color = "white", linewidth = 0.4) +
  scale_fill_brewer(palette = "YlOrRd", name = "Taux de\nchômage (%)") +
  labs(title    = "Quantiles (n = 5)",
       subtitle = "Chaque classe contient le même nombre de régions",
       caption  = "Données simulées | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5))

cat("Bornes — quantiles :", round(breaks_quantile$brks, 1), "\n")

# ---- Méthode 3 : Jenks / Ruptures naturelles --------------
# Minimise la variance au sein de chaque classe et maximise
# la variance entre les classes. Suit les "sauts naturels" de la distribution.
# Avantage : reflète bien la structure réelle des données.
# Inconvénient : calcul plus lent sur de grands jeux de données.
# → Méthode recommandée pour un usage général.

breaks_jenks <- classIntervals(chomage_vec, n = 5, style = "jenks")

france_regions <- france_regions |>
  mutate(classe_jenks = cut(taux_chomage,
                             breaks         = breaks_jenks$brks,
                             include.lowest = TRUE,
                             dig.lab        = 4,
                             ordered_result = TRUE) |> clean_cut_labels())

ggplot(france_regions) +
  geom_sf(aes(fill = classe_jenks), color = "white", linewidth = 0.4) +
  scale_fill_brewer(palette = "YlOrRd", name = "Taux de\nchômage (%)") +
  labs(title    = "Jenks / Ruptures naturelles (n = 5)",
       subtitle = "Les bornes suivent les sauts naturels de la distribution",
       caption  = "Données simulées | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5))

cat("Bornes — Jenks :", round(breaks_jenks$brks, 1), "\n")

# ---- Méthode 4 : Écart-type ------------------------------
# Les classes sont définies par rapport à la moyenne, en multiples
# d'écart-type. Met en évidence les valeurs atypiques (au-dessus
# ou en dessous de la moyenne).
# Avantage : idéal pour détecter des anomalies.
# Inconvénient : suppose une distribution proche de la normale.

breaks_sd <- classIntervals(chomage_vec, n = 5, style = "sd")

france_regions <- france_regions |>
  mutate(classe_sd = cut(taux_chomage,
                          breaks         = breaks_sd$brks,
                          include.lowest = TRUE,
                          dig.lab        = 4,
                          ordered_result = TRUE) |> clean_cut_labels())

ggplot(france_regions) +
  geom_sf(aes(fill = classe_sd), color = "white", linewidth = 0.4) +
  scale_fill_brewer(palette = "RdBu", direction = -1,
                    name = "Taux de\nchômage (%)") +
  labs(title    = "Écart-type",
       subtitle = paste0("Moyenne : ", round(mean(chomage_vec), 1),
                         "% | Écart-type : ", round(sd(chomage_vec), 1), "%"),
       caption  = "Données simulées | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey40", hjust = 0.5))

# ---- Comparaison visuelle des 4 méthodes ------------------
# Chaque méthode place les bornes à des endroits différents
# sur la même distribution. Ce graphique le montre clairement.
# facet_wrap() crée un petit multiple pour chaque méthode.

bornes <- data.frame(
  valeur  = c(breaks_equal$brks, breaks_quantile$brks,
              breaks_jenks$brks, breaks_sd$brks),
  methode = rep(
    c("Intervalle égal", "Quantiles", "Jenks", "Écart-type"),
    times = c(length(breaks_equal$brks), length(breaks_quantile$brks),
              length(breaks_jenks$brks), length(breaks_sd$brks))
  )
) |>
  mutate(methode = factor(methode,
                           levels = c("Intervalle égal", "Quantiles",
                                      "Jenks", "Écart-type")))

histo <- data.frame(x = chomage_vec)

ggplot() +
  geom_histogram(data = histo, aes(x = x),
                 binwidth = 1.2, fill = "#cce5f0", color = "white") +
  geom_vline(data = bornes, aes(xintercept = valeur),
             color = "#d7191c", linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~methode, nrow = 2) +
  labs(title   = "Position des bornes selon la méthode de classification",
       x       = "Taux de chômage (%)",
       y       = "Nombre de régions",
       caption = "Les lignes rouges indiquent les bornes de chaque classe") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold"))

# ============================================================
# 5. PALETTES DE COULEURS
# ============================================================
# Règle : la palette doit correspondre à la nature de la variable.
#
#   Séquentielle  → variable continue allant dans un seul sens
#                   (chômage, densité, revenu)
#   Divergente    → variable organisée autour d'un point central
#                   (écart à la moyenne, variation +/-)
#   Catégorielle  → variable nominale sans ordre
#                   (type de zone, vote majoritaire)

# ---- Palette séquentielle (viridis) -----------------------
# scale_fill_viridis_c() : version continue de viridis
# Viridis est perceptuellement uniforme (lisible en N&B et pour les daltoniens)
# label_number(suffix = "%") formate les étiquettes de légende

ggplot(france_regions) +
  geom_sf(aes(fill = taux_chomage), color = "white", linewidth = 0.4) +
  scale_fill_viridis_c(
    option = "viridis",
    name   = "Taux de\nchômage (%)",
    labels = label_number(suffix = "%")
  ) +
  labs(title    = "Palette séquentielle : viridis",
       subtitle = "Continue, sans classification préalable",
       caption  = "Données simulées | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title        = element_text(face = "bold", hjust = 0.5),
        legend.key.height = unit(1.5, "cm"))

# ---- Palette divergente (RdBu) ----------------------------
# Pour des variables bipolaires — ici l'écart au revenu médian.
# scale_fill_distiller() avec palette divergente
# direction = 1 : rouge pour les valeurs positives (au-dessus du médian)

france_regions <- france_regions |>
  mutate(ecart_revenu = revenu_median - median(revenu_median))

ggplot(france_regions) +
  geom_sf(aes(fill = ecart_revenu), color = "white", linewidth = 0.4) +
  scale_fill_distiller(
    palette   = "RdBu",
    direction = 1,
    name      = "Écart au\nmédian (€)",
    labels    = label_number()
  ) +
  labs(title    = "Palette divergente : RdBu",
       subtitle = "Bleu = au-dessus du médian national | Rouge = en dessous",
       caption  = "Données simulées | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title        = element_text(face = "bold", hjust = 0.5),
        legend.key.height = unit(1.5, "cm"))

# ---- Palette catégorielle (manuelle) ----------------------
# scale_fill_manual() pour un contrôle total des couleurs par catégorie
# case_when() est l'équivalent de if/else pour plusieurs conditions

france_regions <- france_regions |>
  mutate(zone = case_when(
    taux_chomage < 8  ~ "Faible chômage",
    taux_chomage < 12 ~ "Chômage modéré",
    TRUE              ~ "Chômage élevé"     # TRUE = "tous les autres cas"
  ),
  zone = factor(zone, levels = c("Faible chômage", "Chômage modéré", "Chômage élevé")))

ggplot(france_regions) +
  geom_sf(aes(fill = zone), color = "white", linewidth = 0.4) +
  scale_fill_manual(
    values = c("Faible chômage"  = "#2c7bb6",
               "Chômage modéré" = "#ffffbf",
               "Chômage élevé"  = "#d7191c"),
    name = "Niveau de\nchômage"
  ) +
  labs(title   = "Palette catégorielle : 3 niveaux",
       caption = "Données simulées | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ============================================================
# 6. CARTE COMPLÈTE PUBLIABLE
# ============================================================
# Une carte publiable comporte obligatoirement :
# titre, sous-titre, légende (avec unités), source, et un thème soigné.
# theme() permet d'ajuster chaque élément visuellement.

ggplot(france_regions) +
  geom_sf(aes(fill = classe_jenks), color = "white", linewidth = 0.4) +
  scale_fill_brewer(palette = "YlOrRd",
                    name    = "Taux de chômage (%)\n[Jenks, n = 5]") +
  labs(title    = "Taux de chômage par région",
       subtitle = "France métropolitaine — données simulées",
       caption  = "Source : données simulées à des fins pédagogiques\nCours R-Carto | Dr. E. Pietrostefani") +
  theme_void(base_size = 13) +
  theme(
    plot.title        = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle     = element_text(color = "grey40", hjust = 0.5,
                                      margin = margin(b = 8)),
    plot.caption      = element_text(color = "grey55", size = 9,
                                      hjust = 0, margin = margin(t = 10)),
    legend.position   = "right",
    legend.title      = element_text(face = "bold", size = 10),
    legend.key.height = unit(0.9, "cm"),
    plot.margin       = margin(10, 20, 10, 20),
    plot.background   = element_rect(fill = "white", color = NA)
  )

# ---- Choroplèthe + symboles proportionnels ----------------
# On peut superposer plusieurs couches dans ggplot2.
# st_centroid() calcule le centre géographique de chaque polygone.
# st_coordinates() extrait les coordonnées x/y de la géométrie.
# geom_point() dessine les symboles proportionnels sur les centroïdes.

centroids <- france_regions |>
  st_centroid() |>
  mutate(lon = st_coordinates(geometry)[, 1],
         lat = st_coordinates(geometry)[, 2])

ggplot(france_regions) +
  geom_sf(aes(fill = revenu_median), color = "white", linewidth = 0.4) +
  scale_fill_distiller(
    palette   = "Blues", direction = 1,
    name      = "Revenu\nmédian (€)",
    labels    = label_comma(suffix = " €")
  ) +
  geom_point(
    data  = centroids,
    aes(x = lon, y = lat, size = densite_pop),
    color = "#d7191c", alpha = 0.65
  ) +
  scale_size_continuous(
    name   = "Densité\n(hab/km²)",
    range  = c(1, 10),           # taille min et max des symboles en points
    breaks = c(50, 200, 500, 1000)
  ) +
  labs(title    = "Revenu médian et densité de population",
       subtitle = "Choroplèthe + symboles proportionnels",
       caption  = "Source : données simulées | Cours R-Carto") +
  theme_void(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA))

# ============================================================
# EXERCICE — À vous de jouer !
# ============================================================
# Avec les données france_regions chargées dans ce script :
#
# 1. Cartographiez la variable part_diplomes avec 3 méthodes
#    de classification différentes (equal, quantile, jenks).
#    Pour chaque carte, justifiez en commentaire pourquoi cette
#    méthode est ou non adaptée à cette distribution.
#
# 2. Choisissez la palette la plus appropriée et justifiez.
#
# 3. Produisez une carte finale complète avec titre, sous-titre,
#    légende et source.
#
# Bonus : superposez densite_pop en symboles proportionnels
#         sur votre carte finale.
#
# Indice : remplacez les ___ par vos choix de paramètres.

diplomes_vec <- france_regions |> st_drop_geometry() |> pull(part_diplomes)
breaks_ex    <- classIntervals(diplomes_vec, n = 5, style = "___")  # choisir le style

france_regions <- france_regions |>
  mutate(classe_ex = cut(part_diplomes,
                          breaks         = breaks_ex$brks,
                          include.lowest = TRUE,
                          dig.lab        = 4,
                          ordered_result = TRUE) |> clean_cut_labels())

ggplot(france_regions) +
  geom_sf(aes(fill = classe_ex), color = "white", linewidth = 0.4) +
  scale_fill_brewer(palette = "___", name = "___") +
  labs(title = "___", subtitle = "___", caption = "___") +
  theme_void()
