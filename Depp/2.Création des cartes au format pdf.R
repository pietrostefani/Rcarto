############################################################################################################################################################################
############################################################################################################################################################################

                                                  # 2. Creation des Cartes au format PDF vectorisé



# Ce programme vise à créer les cartes au format PDF vectorisé. 
# Il se décompose comme suit : 
  # A) Carte Bac Général
  # B) Carte Bac Pro
  # C) Carte Bac Techno


############################################################################################################################################################################
############################################################################################################################################################################



#################################################################
#################################################################

# A) Carte Bac Général

#################################################################
#################################################################

#### Tests statistiques et choix des classes ####


# Distribution des valeurs à vérifier
hist(CPCP_BCG$VAR_G)
hist(CPCP_BCG$VAR_G,  breaks = 15) # Si le premier histogramme n'est pas assez détaillé, utiliser ce modèle et augmenter la valeur au besoin

# Choix de la méthode de découpage des classes
#classes = quantile(CPCP_BCG$VAR_G,  probs = seq(0, 1, by = 1/5), na.rm = T ) # Si quantiles, nombre de classes = n, remplacer 5 par n
#classes_c<-as.character(round(classes, digits=1))


classes = c(75.3, 94.0, 96.0, 96.48, 96.8, 98.0) # Si seuils manuels, remplacer A, ..., G par valeurs entières à part cas particuliers (cf. consignes cartographiques)

# Mise en forme des classes pour utiliser des bornes entières (optionnel mais fortement conseillé à part cas particuliers)
#classes=c(floor(first(classes)), ceiling(last(classes))) # nombre de classes = n, remplacer 6 par n + 1

# calcul moyenne nationale

moy_G=sum(CPCP_BCG$ADMIS)/sum(CPCP_BCG$PRESENTS)*100
moy_G<-round(moy_G, digits=1)

#### Contenu de la carte ####

# Textes
TitFig="Taux de réussite au baccalauréat général - Session 2022" 
MoyNat=paste0("France métropolitaine\n+ DROM : ", moy_G, "%") 

# Couleurs des territoires

Col=c("#D4E6F5", "#B1D9F2", "#7CB0E0", "#3476B0", "#0F4E7D") # Bleu avec cinq classes
#Col=c("#D4E6F5", "#B1D9F2", "#7CB0E0", "#3476B0") # Bleu avec quatre classes


# On rend la base "spatiale"

CPCP_BCG = st_as_sf(CPCP_BCG)

# Creation de la carte

Carte_BCG <- tm_shape(CPCP_BCG) +
  tm_fill(col = "VAR_G",
          palette = Col, style="fixed", breaks=classes, 
        #legend.format = c(text.separator = " "), 
      #  labels = classes_c,
      labels = c("75.3 - 94.0", "94.0 - 96.0", "96.0 - 96.5", "96.5 - 97.0", "97.0 - 98.0"), # Pour définir manuellement les bornes 
        legend.is.portrait=T, title = "",        
          colorNA = NULL, showNA = F) +            
  tm_borders("white", lwd = 0.25) +
  tm_layout(main.title = TitFig,
            main.title.position = c("left", "top"),
            main.title.size = 0.8,
            fontfamily = "Marianne",
            frame = F,
            legend.position = c(0.5, 0),
            legend.hist.height = -1,
            legend.text.size = 0.58,
            legend.title.size = 0.58,
            inner.margins = c(0.075, 0, 0.10, 0)) +
  tm_shape(CHAB) +
  tm_borders("black", lwd = 0.8) +
  tm_credits(MoyNat, size = 0.49, fontfamily = "Marianne",
             position = c("LEFT", "BOTTOM")) +
  tm_credits("en %", size = 0.58, fontfamily = "Marianne", 
             position = c(0.440, 0.05)) +
           #  position = c(0.635, 0.05)) +
  tm_shape(LIPA) +
  tm_lines(lwd = 0.25, lty = 2) +
  tm_shape(LIBL) +
  tm_lines(col = "white") +
  tm_text(text = "NOM", size = 0.4, along.lines = T) +
  tm_shape(MAYN) +                                                        
  tm_borders("black", lwd = 0.2)                                        

  
  #### Export de la carte comme pdf vectorisé ####
tmap_save(Carte_BCG,"C:/Users/tberut/Desktop/Journée du BAC/2022/Journée du 09.07/BCG_2022.pdf",width = 131.4, units = "mm") 

# tmap_save(Carte_BCG,"C:/Users/tberut/Desktop/Journée du BAC/2022/BCG_2022.jpg", width = 131.4, units = "mm") 


#################################################################
#################################################################

# B) Carte Bac Pro

#################################################################
#################################################################

# Choix de la méthode de découpage des classes
#classes = quantile(CPCP_BCP$VAR_P,  probs = seq(0, 1, by = 1/5), na.rm = T )
#classes_p<-as.character(round(classes, digits=1))
classes = c(63.0, 79.8, 81.5, 84.0, 85.0, 87.2) # Si seuils manuels, remplacer A, ..., G par valeurs entières à part cas particuliers (cf. consignes cartographiques)

# calcul moyenne nationale
moy_P=sum(CPCP_BCP$ADMIS)/sum(CPCP_BCP$PRESENTS)*100
moy_P<-round(moy_P, digits=1)

# Textes
TitFig="Taux de réussite au baccalauréat professionnel - Session 2022" 
MoyNat=paste0("France métropolitaine\n+ DROM : ", moy_P, "%") 


# Couleurs des territoires
Col=c("#FCE5D6", "#F7CAAC", "#EF9F6E", "#ED8248", "#DE6C30") # Orange avec cinq classes

# On rend les bases "spatiales"

CPCP_BCP = st_as_sf(CPCP_BCP)


Carte_BCP <- tm_shape(CPCP_BCP) +
  tm_fill(col = "VAR_P",
          palette = Col, style="fixed", breaks=classes, 
         # legend.format = c(text.separator = " "), 
          labels = c("63.0 - 80.0", "80.0 - 82.0", "82.0 - 84.0", "84.0 - 85.0", "85.0 - 87.2"), # Pour définir manuellement les bornes 
          legend.is.portrait=T, title = "",        
          colorNA = NULL, showNA = F) +            
  tm_borders("white", lwd = 0.25) +
  tm_layout(main.title = TitFig,
            main.title.position = c("left", "top"),
            main.title.size = 0.8,
            fontfamily = "Marianne",
            frame = F,
            legend.position = c(0.5, 0),
            legend.hist.height = -1,
            legend.text.size = 0.58,
            legend.title.size = 0.58,
            inner.margins = c(0.075, 0, 0.10, 0)) +
  tm_shape(CHAB) +
  tm_borders("black", lwd = 0.8) +
  tm_credits(MoyNat, size = 0.49, fontfamily = "Marianne",
             position = c("LEFT", "BOTTOM")) +
  tm_credits("en %", size = 0.58, fontfamily = "Marianne", 
             position = c(0.440, 0.05)) +
           #  position = c(0.635, 0.05)) +
  tm_shape(LIPA) +
  tm_lines(lwd = 0.25, lty = 2) +
  tm_shape(LIBL) +
  tm_lines(col = "white") +
  tm_text(text = "NOM", size = 0.4, along.lines = T) +
  tm_shape(MAYN) +                                                        
  tm_borders("black", lwd = 0.2)                                        


#### Export de la carte comme pdf vectorisé ####
tmap_save(Carte_BCP,"C:/Users/tberut/Desktop/Journée du BAC/2022/Journée du 09.07/BCP_2022.pdf",width = 131.4, units = "mm") 



#################################################################
#################################################################

# C) Carte Bac Techno

#################################################################
#################################################################

classes = c(69.4, 89.0, 90.0, 91.0, 94.0, 95.1) # Si seuils manuels, remplacer A, ..., G par valeurs entières à part cas particuliers (cf. consignes cartographiques)
#classes = quantile(CPCP_BCT$VAR_T,  probs = seq(0, 1, by = 1/5), na.rm = T )

# calcul moyenne nationale
moy_T=sum(CPCP_BCT$ADMIS)/sum(CPCP_BCT$PRESENTS)*100
moy_T<-round(moy_T, digits=1)

# Textes
TitFig="Taux de réussite au baccalauréat technologique - Session 2022" 
MoyNat=paste0("France métropolitaine\n+ DROM : ", moy_T, "%") 

# Couleurs des territoires
Col=c("#e5edc4", "#d0dc8f", "#b7c955", "#9bb611", "#8aa311") # Vert avec cinq classes




# On rend les bases "spatiales"
CPCP_BCT = st_as_sf(CPCP_BCT)


# Creation de la carte
Carte_BCT <- tm_shape(CPCP_BCT) +
  tm_fill(col = "VAR_T",
          palette = Col, style="fixed", breaks=classes, 
          #legend.format = c(text.separator = " "), # Pour définir manuellement les bornes 
          labels = c("69.5 - 89.0", "89.0 - 90.0", "90.0 - 91.0", "91.0 - 94.0", "94.0 - 95.1"), # Pour définir manuellement les bornes 
          legend.is.portrait=T, title = "",       
          colorNA = NULL, showNA = F) +          
  tm_borders("white", lwd = 0.25) +
  tm_layout(main.title = TitFig,
            main.title.position = c("left", "top"),
            main.title.size = 0.8,
            fontfamily = "Marianne",
            frame = F,
            legend.position = c(0.5, 0),
            legend.hist.height = -1,
            legend.text.size = 0.58,
            legend.title.size = 0.58,
            inner.margins = c(0.075, 0, 0.10, 0)) +
  tm_shape(CHAB) +
  tm_borders("black", lwd = 0.8) +
  tm_credits(MoyNat, size = 0.49, fontfamily = "Marianne",
             position = c("LEFT", "BOTTOM")) +
  tm_credits("en %", size = 0.58, fontfamily = "Marianne", 
             position = c(0.440, 0.05)) +
          #  position = c(0.635, 0.05)) +
  tm_shape(LIPA) +
  tm_lines(lwd = 0.25, lty = 2) +
  tm_shape(LIBL) +
  tm_lines(col = "white") +
  tm_text(text = "NOM", size = 0.4, along.lines = T) +
  tm_shape(MAYN) +                                                        
  tm_borders("black", lwd = 0.2)                                        


#### Export de la carte comme pdf vectorisé ####
tmap_save(Carte_BCT,"C:/Users/tberut/Desktop/Journée du BAC/2022/Journée du 09.07/BCT_2022.pdf",width = 131.4, units = "mm") 